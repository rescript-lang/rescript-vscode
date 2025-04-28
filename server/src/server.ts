import process from "process";
import * as p from "vscode-languageserver-protocol";
import * as v from "vscode-languageserver";
import * as rpc from "vscode-jsonrpc/node";
import * as path from "path";
import fs from "fs";
// TODO: check DidChangeWatchedFilesNotification.
import {
  DidOpenTextDocumentNotification,
  DidChangeTextDocumentNotification,
  DidCloseTextDocumentNotification,
  DidChangeConfigurationNotification,
  InitializeParams,
  InlayHintParams,
  CodeLensParams,
  SignatureHelpParams,
} from "vscode-languageserver-protocol";
import * as lookup from "./lookup";
import * as utils from "./utils";
import * as codeActions from "./codeActions";
import * as c from "./constants";
import * as chokidar from "chokidar";
import { assert } from "console";
import { fileURLToPath } from "url";
import { WorkspaceEdit } from "vscode-languageserver";
import { onErrorReported } from "./errorReporter";
import * as ic from "./incrementalCompilation";
import config, { extensionConfiguration } from "./config";
import { projectsFiles } from "./projectFiles";

// This holds client capabilities specific to our extension, and not necessarily
// related to the LS protocol. It's for enabling/disabling features that might
// work in one client, like VSCode, but perhaps not in others, like vim.
export interface extensionClientCapabilities {
  supportsMarkdownLinks?: boolean | null;
  supportsSnippetSyntax?: boolean | null;
}
let extensionClientCapabilities: extensionClientCapabilities = {};

// Below here is some state that's not important exactly how long it lives.
let pullConfigurationPeriodically: NodeJS.Timeout | null = null;

// https://microsoft.github.io/language-server-protocol/specification#initialize
// According to the spec, there could be requests before the 'initialize' request. Link in comment tells how to handle them.
let initialized = false;
let serverSentRequestIdCounter = 0;
// https://microsoft.github.io/language-server-protocol/specification#exit
let shutdownRequestAlreadyReceived = false;
let stupidFileContentCache: Map<string, string> = new Map();

// ^ caching AND states AND distributed system. Why does LSP has to be stupid like this

// This keeps track of code actions extracted from diagnostics.
let codeActionsFromDiagnostics: codeActions.filesCodeActions = {};

// will be properly defined later depending on the mode (stdio/node-rpc)
let send: (msg: p.Message) => void = (_) => {};

let findRescriptBinary = (projectRootPath: p.DocumentUri | null) =>
  config.extensionConfiguration.binaryPath == null
    ? lookup.findFilePathFromProjectRoot(
        projectRootPath,
        path.join(c.nodeModulesBinDir, c.rescriptBinName)
      )
    : utils.findBinary(
        config.extensionConfiguration.binaryPath,
        c.rescriptBinName
      );

let createInterfaceRequest = new v.RequestType<
  p.TextDocumentIdentifier,
  p.TextDocumentIdentifier,
  void
>("textDocument/createInterface");

let openCompiledFileRequest = new v.RequestType<
  p.TextDocumentIdentifier,
  p.TextDocumentIdentifier,
  void
>("textDocument/openCompiled");

let getCurrentCompilerDiagnosticsForFile = (
  fileUri: string
): p.Diagnostic[] => {
  let diagnostics: p.Diagnostic[] | null = null;

  projectsFiles.forEach((projectFile, _projectRootPath) => {
    if (diagnostics == null && projectFile.filesDiagnostics[fileUri] != null) {
      diagnostics = projectFile.filesDiagnostics[fileUri].slice();
    }
  });

  return diagnostics ?? [];
};
let sendUpdatedDiagnostics = () => {
  projectsFiles.forEach((projectFile, projectRootPath) => {
    let { filesWithDiagnostics } = projectFile;
    let compilerLogPath = path.join(projectRootPath, c.compilerLogPartialPath);
    let content = fs.readFileSync(compilerLogPath, { encoding: "utf-8" });
    let {
      done,
      result: filesAndErrors,
      codeActions,
      linesWithParseErrors,
    } = utils.parseCompilerLogOutput(content);

    if (linesWithParseErrors.length > 0) {
      let params: p.ShowMessageParams = {
        type: p.MessageType.Warning,
        message: `There are more compiler warning/errors that we could not parse. You can help us fix this by opening an [issue on the repository](https://github.com/rescript-lang/rescript-vscode/issues/new?title=Compiler%20log%20parse%20error), pasting the contents of the file [lib/bs/.compiler.log](file://${compilerLogPath}).`,
      };
      let message: p.NotificationMessage = {
        jsonrpc: c.jsonrpcVersion,
        method: "window/showMessage",
        params: params,
      };
      send(message);
    }

    projectFile.filesDiagnostics = filesAndErrors;
    codeActionsFromDiagnostics = codeActions;

    // diff
    Object.keys(filesAndErrors).forEach((file) => {
      let params: p.PublishDiagnosticsParams = {
        uri: file,
        diagnostics: filesAndErrors[file],
      };
      let notification: p.NotificationMessage = {
        jsonrpc: c.jsonrpcVersion,
        method: "textDocument/publishDiagnostics",
        params: params,
      };
      send(notification);

      filesWithDiagnostics.add(file);
    });
    if (done) {
      // clear old files
      filesWithDiagnostics.forEach((file) => {
        if (filesAndErrors[file] == null) {
          // Doesn't exist in the new diagnostics. Clear this diagnostic
          let params: p.PublishDiagnosticsParams = {
            uri: file,
            diagnostics: [],
          };
          let notification: p.NotificationMessage = {
            jsonrpc: c.jsonrpcVersion,
            method: "textDocument/publishDiagnostics",
            params: params,
          };
          send(notification);
          filesWithDiagnostics.delete(file);
        }
      });
    }
  });
};
let deleteProjectDiagnostics = (projectRootPath: string) => {
  let root = projectsFiles.get(projectRootPath);
  if (root != null) {
    root.filesWithDiagnostics.forEach((file) => {
      let params: p.PublishDiagnosticsParams = {
        uri: file,
        diagnostics: [],
      };
      let notification: p.NotificationMessage = {
        jsonrpc: c.jsonrpcVersion,
        method: "textDocument/publishDiagnostics",
        params: params,
      };
      send(notification);
    });

    projectsFiles.delete(projectRootPath);
    if (config.extensionConfiguration.incrementalTypechecking?.enable) {
      ic.removeIncrementalFileFolder(projectRootPath);
    }
  }
};
let sendCompilationFinishedMessage = () => {
  let notification: p.NotificationMessage = {
    jsonrpc: c.jsonrpcVersion,
    method: "rescript/compilationFinished",
  };

  send(notification);
};

let debug = false;

let syncProjectConfigCache = (rootPath: string) => {
  try {
    if (debug) console.log("syncing project config cache for " + rootPath);
    utils.runAnalysisAfterSanityCheck(rootPath, ["cache-project", rootPath]);
    if (debug) console.log("OK - synced project config cache for " + rootPath);
  } catch (e) {
    if (debug) console.error(e);
  }
};

let deleteProjectConfigCache = (rootPath: string) => {
  try {
    if (debug) console.log("deleting project config cache for " + rootPath);
    utils.runAnalysisAfterSanityCheck(rootPath, ["cache-delete", rootPath]);
    if (debug) console.log("OK - deleted project config cache for " + rootPath);
  } catch (e) {
    if (debug) console.error(e);
  }
};

let compilerLogsWatcher = chokidar
  .watch([], {
    awaitWriteFinish: {
      stabilityThreshold: 1,
    },
  })
  .on("all", (_e, changedPath) => {
    if (changedPath.includes("build.ninja")) {
      if (config.extensionConfiguration.cache?.projectConfig?.enable === true) {
        let projectRoot = utils.findProjectRootOfFile(changedPath);
        if (projectRoot != null) {
          syncProjectConfigCache(projectRoot);
        }
      }
    } else {
      try {
        sendUpdatedDiagnostics();
        sendCompilationFinishedMessage();
        if (config.extensionConfiguration.inlayHints?.enable === true) {
          sendInlayHintsRefresh();
        }
        if (config.extensionConfiguration.codeLens === true) {
          sendCodeLensRefresh();
        }
      } catch {
        console.log("Error while sending updated diagnostics");
      }
    }
  });
let stopWatchingCompilerLog = () => {
  // TODO: cleanup of compilerLogs?
  compilerLogsWatcher.close();
};

type clientSentBuildAction = {
  title: string;
  projectRootPath: string;
};
let openedFile = (fileUri: string, fileContent: string) => {
  let filePath = fileURLToPath(fileUri);

  stupidFileContentCache.set(filePath, fileContent);

  let projectRootPath = utils.findProjectRootOfFile(filePath);
  if (projectRootPath != null) {
    let projectRootState = projectsFiles.get(projectRootPath);
    if (projectRootState == null) {
      if (config.extensionConfiguration.incrementalTypechecking?.enable) {
        ic.recreateIncrementalFileFolder(projectRootPath);
      }
      const namespaceName =
        utils.getNamespaceNameFromConfigFile(projectRootPath);

      projectRootState = {
        openFiles: new Set(),
        filesWithDiagnostics: new Set(),
        filesDiagnostics: {},
        namespaceName:
          namespaceName.kind === "success" ? namespaceName.result : null,
        rescriptVersion: utils.findReScriptVersion(projectRootPath),
        bsbWatcherByEditor: null,
        bscBinaryLocation: utils.findBscExeBinary(projectRootPath),
        editorAnalysisLocation: utils.findEditorAnalysisBinary(projectRootPath),
        hasPromptedToStartBuild: /(\/|\\)node_modules(\/|\\)/.test(
          projectRootPath
        )
          ? "never"
          : false,
      };
      projectsFiles.set(projectRootPath, projectRootState);
      compilerLogsWatcher.add(
        path.join(projectRootPath, c.compilerLogPartialPath)
      );
      if (config.extensionConfiguration.cache?.projectConfig?.enable === true) {
        compilerLogsWatcher.add(
          path.join(projectRootPath, c.buildNinjaPartialPath)
        );
        syncProjectConfigCache(projectRootPath);
      }
    }
    let root = projectsFiles.get(projectRootPath)!;
    root.openFiles.add(filePath);
    // check if .bsb.lock is still there. If not, start a bsb -w ourselves
    // because otherwise the diagnostics info we'll display might be stale
    let bsbLockPath = path.join(projectRootPath, c.bsbLock);
    if (
      projectRootState.hasPromptedToStartBuild === false &&
      config.extensionConfiguration.askToStartBuild === true &&
      !fs.existsSync(bsbLockPath)
    ) {
      // TODO: sometime stale .bsb.lock dangling. bsb -w knows .bsb.lock is
      // stale. Use that logic
      // TODO: close watcher when lang-server shuts down
      if (findRescriptBinary(projectRootPath) != null) {
        let payload: clientSentBuildAction = {
          title: c.startBuildAction,
          projectRootPath: projectRootPath,
        };
        let params = {
          type: p.MessageType.Info,
          message: `Start a build for this project to get the freshest data?`,
          actions: [payload],
        };
        let request: p.RequestMessage = {
          jsonrpc: c.jsonrpcVersion,
          id: serverSentRequestIdCounter++,
          method: "window/showMessageRequest",
          params: params,
        };
        send(request);
        projectRootState.hasPromptedToStartBuild = true;
        // the client might send us back the "start build" action, which we'll
        // handle in the isResponseMessage check in the message handling way
        // below
      } else {
        let request: p.NotificationMessage = {
          jsonrpc: c.jsonrpcVersion,
          method: "window/showMessage",
          params: {
            type: p.MessageType.Error,
            message:
              config.extensionConfiguration.binaryPath == null
                ? `Can't find ReScript binary in  ${path.join(
                    projectRootPath,
                    c.nodeModulesBinDir
                  )} or parent directories. Did you install it? It's required to use "rescript" > 9.1`
                : `Can't find ReScript binary in the directory ${config.extensionConfiguration.binaryPath}`,
          },
        };
        send(request);
      }
    }

    // no need to call sendUpdatedDiagnostics() here; the watcher add will
    // call the listener which calls it
  }
};

let closedFile = (fileUri: string) => {
  let filePath = fileURLToPath(fileUri);

  if (config.extensionConfiguration.incrementalTypechecking?.enable) {
    ic.handleClosedFile(filePath);
  }

  stupidFileContentCache.delete(filePath);

  let projectRootPath = utils.findProjectRootOfFile(filePath);
  if (projectRootPath != null) {
    let root = projectsFiles.get(projectRootPath);
    if (root != null) {
      root.openFiles.delete(filePath);
      // clear diagnostics too if no open files open in said project
      if (root.openFiles.size === 0) {
        compilerLogsWatcher.unwatch(
          path.join(projectRootPath, c.compilerLogPartialPath)
        );
        compilerLogsWatcher.unwatch(
          path.join(projectRootPath, c.buildNinjaPartialPath)
        );
        deleteProjectConfigCache(projectRootPath);
        deleteProjectDiagnostics(projectRootPath);
        if (root.bsbWatcherByEditor !== null) {
          root.bsbWatcherByEditor.kill();
          root.bsbWatcherByEditor = null;
        }
      }
    }
  }
};

let updateOpenedFile = (fileUri: string, fileContent: string) => {
  let filePath = fileURLToPath(fileUri);
  assert(stupidFileContentCache.has(filePath));
  stupidFileContentCache.set(filePath, fileContent);
  if (config.extensionConfiguration.incrementalTypechecking?.enable) {
    ic.handleUpdateOpenedFile(filePath, fileContent, send, () => {
      if (config.extensionConfiguration.codeLens) {
        sendCodeLensRefresh();
      }
      if (config.extensionConfiguration.inlayHints) {
        sendInlayHintsRefresh();
      }
    });
  }
};
let getOpenedFileContent = (fileUri: string) => {
  let filePath = fileURLToPath(fileUri);
  let content = stupidFileContentCache.get(filePath)!;
  assert(content != null);
  return content;
};

export default function listen(useStdio = false) {
  // Start listening now!
  // We support two modes: the regular node RPC mode for VSCode, and the --stdio
  // mode for other editors The latter is _technically unsupported_. It's an
  // implementation detail that might change at any time
  if (useStdio) {
    let writer = new rpc.StreamMessageWriter(process.stdout);
    let reader = new rpc.StreamMessageReader(process.stdin);
    // proper `this` scope for writer
    send = (msg: p.Message) => writer.write(msg);
    reader.listen(onMessage);
  } else {
    // proper `this` scope for process
    send = (msg: p.Message) => process.send!(msg);
    process.on("message", onMessage);
  }
}

function hover(msg: p.RequestMessage) {
  let params = msg.params as p.HoverParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let code = getOpenedFileContent(params.textDocument.uri);
  let tmpname = utils.createFileInTempDir();
  fs.writeFileSync(tmpname, code, { encoding: "utf-8" });
  let response = utils.runAnalysisCommand(
    filePath,
    [
      "hover",
      filePath,
      params.position.line,
      params.position.character,
      tmpname,
      Boolean(extensionClientCapabilities.supportsMarkdownLinks),
    ],
    msg
  );
  fs.unlink(tmpname, () => null);
  return response;
}

function inlayHint(msg: p.RequestMessage) {
  const params = msg.params as p.InlayHintParams;
  const filePath = fileURLToPath(params.textDocument.uri);

  const response = utils.runAnalysisCommand(
    filePath,
    [
      "inlayHint",
      filePath,
      params.range.start.line,
      params.range.end.line,
      config.extensionConfiguration.inlayHints?.maxLength,
    ],
    msg
  );
  return response;
}

function sendInlayHintsRefresh() {
  let request: p.RequestMessage = {
    jsonrpc: c.jsonrpcVersion,
    method: p.InlayHintRefreshRequest.method,
    id: serverSentRequestIdCounter++,
  };
  send(request);
}

function codeLens(msg: p.RequestMessage) {
  const params = msg.params as p.CodeLensParams;
  const filePath = fileURLToPath(params.textDocument.uri);

  const response = utils.runAnalysisCommand(
    filePath,
    ["codeLens", filePath],
    msg
  );
  return response;
}

function sendCodeLensRefresh() {
  let request: p.RequestMessage = {
    jsonrpc: c.jsonrpcVersion,
    method: p.CodeLensRefreshRequest.method,
    id: serverSentRequestIdCounter++,
  };
  send(request);
}

function signatureHelp(msg: p.RequestMessage) {
  let params = msg.params as p.SignatureHelpParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let code = getOpenedFileContent(params.textDocument.uri);
  let tmpname = utils.createFileInTempDir();
  fs.writeFileSync(tmpname, code, { encoding: "utf-8" });
  let response = utils.runAnalysisCommand(
    filePath,
    [
      "signatureHelp",
      filePath,
      params.position.line,
      params.position.character,
      tmpname,
      config.extensionConfiguration.signatureHelp?.forConstructorPayloads
        ? "true"
        : "false",
    ],
    msg
  );
  fs.unlink(tmpname, () => null);
  return response;
}

function definition(msg: p.RequestMessage) {
  // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_definition
  let params = msg.params as p.DefinitionParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let response = utils.runAnalysisCommand(
    filePath,
    ["definition", filePath, params.position.line, params.position.character],
    msg
  );
  return response;
}

function typeDefinition(msg: p.RequestMessage) {
  // https://microsoft.github.io/language-server-protocol/specification/specification-current/#textDocument_typeDefinition
  let params = msg.params as p.TypeDefinitionParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let response = utils.runAnalysisCommand(
    filePath,
    [
      "typeDefinition",
      filePath,
      params.position.line,
      params.position.character,
    ],
    msg
  );
  return response;
}

function references(msg: p.RequestMessage) {
  // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_references
  let params = msg.params as p.ReferenceParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let result: typeof p.ReferencesRequest.type = utils.getReferencesForPosition(
    filePath,
    params.position
  );
  let response: p.ResponseMessage = {
    jsonrpc: c.jsonrpcVersion,
    id: msg.id,
    result,
    // error: code and message set in case an exception happens during the definition request.
  };
  return response;
}

function prepareRename(msg: p.RequestMessage): p.ResponseMessage {
  // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_prepareRename
  let params = msg.params as p.PrepareRenameParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let locations: null | p.Location[] = utils.getReferencesForPosition(
    filePath,
    params.position
  );
  let result: p.Range | null = null;
  if (locations !== null) {
    locations.forEach((loc) => {
      if (
        path.normalize(fileURLToPath(loc.uri)) ===
        path.normalize(fileURLToPath(params.textDocument.uri))
      ) {
        let { start, end } = loc.range;
        let pos = params.position;
        if (
          start.character <= pos.character &&
          start.line <= pos.line &&
          end.character >= pos.character &&
          end.line >= pos.line
        ) {
          result = loc.range;
        }
      }
    });
  }
  return {
    jsonrpc: c.jsonrpcVersion,
    id: msg.id,
    result,
  };
}

function rename(msg: p.RequestMessage) {
  // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_rename
  let params = msg.params as p.RenameParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let documentChanges: (p.RenameFile | p.TextDocumentEdit)[] | null =
    utils.runAnalysisAfterSanityCheck(filePath, [
      "rename",
      filePath,
      params.position.line,
      params.position.character,
      params.newName,
    ]);
  let result: WorkspaceEdit | null = null;
  if (documentChanges !== null) {
    result = { documentChanges };
  }
  let response: p.ResponseMessage = {
    jsonrpc: c.jsonrpcVersion,
    id: msg.id,
    result,
  };
  return response;
}

function documentSymbol(msg: p.RequestMessage) {
  // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentSymbol
  let params = msg.params as p.DocumentSymbolParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let extension = path.extname(params.textDocument.uri);
  let code = getOpenedFileContent(params.textDocument.uri);
  let tmpname = utils.createFileInTempDir(extension);
  fs.writeFileSync(tmpname, code, { encoding: "utf-8" });
  let response = utils.runAnalysisCommand(
    filePath,
    ["documentSymbol", tmpname],
    msg,
    /* projectRequired */ false
  );
  fs.unlink(tmpname, () => null);
  return response;
}

function askForAllCurrentConfiguration() {
  // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_configuration
  let params: p.ConfigurationParams = {
    items: [
      {
        section: "rescript.settings",
      },
    ],
  };
  let req: p.RequestMessage = {
    jsonrpc: c.jsonrpcVersion,
    id: c.configurationRequestId,
    method: p.ConfigurationRequest.type.method,
    params,
  };
  send(req);
}

function semanticTokens(msg: p.RequestMessage) {
  // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens
  let params = msg.params as p.SemanticTokensParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let extension = path.extname(params.textDocument.uri);
  let code = getOpenedFileContent(params.textDocument.uri);
  let tmpname = utils.createFileInTempDir(extension);
  fs.writeFileSync(tmpname, code, { encoding: "utf-8" });
  let response = utils.runAnalysisCommand(
    filePath,
    ["semanticTokens", tmpname],
    msg,
    /* projectRequired */ false
  );
  fs.unlink(tmpname, () => null);
  return response;
}

async function completion(msg: p.RequestMessage) {
  // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_completion
  let params = msg.params as p.ReferenceParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let code = getOpenedFileContent(params.textDocument.uri);
  let tmpname = utils.createFileInTempDir();
  fs.writeFileSync(tmpname, code, { encoding: "utf-8" });

  if (config.extensionConfiguration.newCompletion) {
    await new Promise<void>((resolve) => {
      ic.triggerIncrementalCompilationOfFile(filePath, code, send, () => {
        resolve();
      });
    });
  }

  let response = utils.runAnalysisCommand(
    filePath,
    [
      "completion",
      filePath,
      params.position.line,
      params.position.character,
      tmpname,
    ],
    msg
  );
  fs.unlink(tmpname, () => null);
  return response;
}

function completionResolve(msg: p.RequestMessage) {
  const item = msg.params as p.CompletionItem;
  let response: p.ResponseMessage = {
    jsonrpc: c.jsonrpcVersion,
    id: msg.id,
    result: item,
  };

  if (item.documentation == null && item.data != null) {
    const data = item.data as { filePath: string; modulePath: string };
    let result = utils.runAnalysisAfterSanityCheck(
      data.filePath,
      ["completionResolve", data.filePath, data.modulePath],
      true
    );
    item.documentation = { kind: "markdown", value: result };
  }

  return response;
}

function codeAction(msg: p.RequestMessage): p.ResponseMessage {
  let params = msg.params as p.CodeActionParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let code = getOpenedFileContent(params.textDocument.uri);
  let extension = path.extname(params.textDocument.uri);
  let tmpname = utils.createFileInTempDir(extension);

  // Check local code actions coming from the diagnostics, or from incremental compilation.
  let localResults: v.CodeAction[] = [];
  const fromDiagnostics =
    codeActionsFromDiagnostics[params.textDocument.uri] ?? [];
  const fromIncrementalCompilation =
    ic.getCodeActionsFromIncrementalCompilation(filePath) ?? [];
  [...fromDiagnostics, ...fromIncrementalCompilation].forEach(
    ({ range, codeAction }) => {
      if (utils.rangeContainsRange(range, params.range)) {
        localResults.push(codeAction);
      }
    }
  );

  fs.writeFileSync(tmpname, code, { encoding: "utf-8" });
  let response = utils.runAnalysisCommand(
    filePath,
    [
      "codeAction",
      filePath,
      params.range.start.line,
      params.range.start.character,
      params.range.end.line,
      params.range.end.character,
      tmpname,
    ],
    msg
  );
  fs.unlink(tmpname, () => null);

  let { result } = response;

  // We must send `null` when there are no results, empty array isn't enough.
  let codeActions =
    result != null && Array.isArray(result)
      ? [...localResults, ...result]
      : localResults;

  let res: v.ResponseMessage = {
    jsonrpc: c.jsonrpcVersion,
    id: msg.id,
    result: codeActions.length > 0 ? codeActions : null,
  };
  return res;
}

function format(msg: p.RequestMessage): Array<p.Message> {
  // technically, a formatting failure should reply with the error. Sadly
  // the LSP alert box for these error replies sucks (e.g. doesn't actually
  // display the message). In order to signal the client to display a proper
  // alert box (sometime with actionable buttons), we need to first send
  // back a fake success message (because each request mandates a
  // response), then right away send a server notification to display a
  // nicer alert. Ugh.
  let fakeSuccessResponse: p.ResponseMessage = {
    jsonrpc: c.jsonrpcVersion,
    id: msg.id,
    result: [],
  };
  let params = msg.params as p.DocumentFormattingParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let extension = path.extname(params.textDocument.uri);
  if (extension !== c.resExt && extension !== c.resiExt) {
    let params: p.ShowMessageParams = {
      type: p.MessageType.Error,
      message: `Not a ${c.resExt} or ${c.resiExt} file. Cannot format it.`,
    };
    let response: p.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params: params,
    };
    return [fakeSuccessResponse, response];
  } else {
    // code will always be defined here, even though technically it can be undefined
    let code = getOpenedFileContent(params.textDocument.uri);

    let projectRootPath = utils.findProjectRootOfFile(filePath);
    let project =
      projectRootPath != null ? projectsFiles.get(projectRootPath) : null;
    let bscExeBinaryPath = project?.bscBinaryLocation ?? null;

    let formattedResult = utils.formatCode(bscExeBinaryPath, filePath, code);
    if (formattedResult.kind === "success") {
      let max = code.length;
      let result: p.TextEdit[] = [
        {
          range: {
            start: { line: 0, character: 0 },
            end: { line: max, character: max },
          },
          newText: formattedResult.result,
        },
      ];
      let response: p.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: msg.id,
        result: result,
      };
      return [response];
    } else {
      // let the diagnostics logic display the updated syntax errors,
      // from the build.
      // Again, not sending the actual errors. See fakeSuccessResponse
      // above for explanation
      return [fakeSuccessResponse];
    }
  }
}

let updateDiagnosticSyntax = (fileUri: string, fileContent: string) => {
  if (config.extensionConfiguration.incrementalTypechecking?.enable) {
    // The incremental typechecking already sends syntax diagnostics.
    return;
  }
  let filePath = fileURLToPath(fileUri);
  let extension = path.extname(filePath);
  let tmpname = utils.createFileInTempDir(extension);
  fs.writeFileSync(tmpname, fileContent, { encoding: "utf-8" });

  // We need to account for any existing diagnostics from the compiler for this
  // file. If we don't we might accidentally clear the current file's compiler
  // diagnostics if there's no syntax diagostics to send. This is because
  // publishing an empty diagnostics array is equivalent to saying "clear all
  // errors".
  let compilerDiagnosticsForFile =
    getCurrentCompilerDiagnosticsForFile(fileUri);
  let syntaxDiagnosticsForFile: p.Diagnostic[] =
    utils.runAnalysisAfterSanityCheck(filePath, ["diagnosticSyntax", tmpname]);

  let notification: p.NotificationMessage = {
    jsonrpc: c.jsonrpcVersion,
    method: "textDocument/publishDiagnostics",
    params: {
      uri: fileUri,
      diagnostics: [...syntaxDiagnosticsForFile, ...compilerDiagnosticsForFile],
    },
  };

  fs.unlink(tmpname, () => null);

  send(notification);
};

function createInterface(msg: p.RequestMessage): p.Message {
  let params = msg.params as p.TextDocumentIdentifier;
  let extension = path.extname(params.uri);
  let filePath = fileURLToPath(params.uri);
  let projDir = utils.findProjectRootOfFile(filePath);

  if (projDir === null) {
    let params: p.ShowMessageParams = {
      type: p.MessageType.Error,
      message: `Cannot locate project directory to generate the interface file.`,
    };

    let response: p.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params: params,
    };

    return response;
  }

  if (extension !== c.resExt) {
    let params: p.ShowMessageParams = {
      type: p.MessageType.Error,
      message: `Not a ${c.resExt} file. Cannot create an interface for it.`,
    };

    let response: p.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params: params,
    };

    return response;
  }

  let resPartialPath = filePath.split(projDir)[1];

  // The .cmi filename may have a namespace suffix appended.
  let namespaceResult = utils.getNamespaceNameFromConfigFile(projDir);

  if (namespaceResult.kind === "error") {
    let params: p.ShowMessageParams = {
      type: p.MessageType.Error,
      message: `Error reading ReScript config file.`,
    };

    let response: p.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params,
    };

    return response;
  }

  let namespace = namespaceResult.result;
  let suffixToAppend = namespace.length > 0 ? "-" + namespace : "";

  let cmiPartialPath = path.join(
    path.dirname(resPartialPath),
    path.basename(resPartialPath, c.resExt) + suffixToAppend + c.cmiExt
  );

  let cmiPath = path.join(projDir, c.compilerDirPartialPath, cmiPartialPath);
  let cmiAvailable = fs.existsSync(cmiPath);

  if (!cmiAvailable) {
    let params: p.ShowMessageParams = {
      type: p.MessageType.Error,
      message: `No compiled interface file found. Please compile your project first.`,
    };

    let response: p.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params,
    };

    return response;
  }

  let response = utils.runAnalysisCommand(
    filePath,
    ["createInterface", filePath, cmiPath],
    msg
  );
  let result = typeof response.result === "string" ? response.result : "";

  try {
    let resiPath = lookup.replaceFileExtension(filePath, c.resiExt);
    fs.writeFileSync(resiPath, result, { encoding: "utf-8" });
    let response: p.ResponseMessage = {
      jsonrpc: c.jsonrpcVersion,
      id: msg.id,
      result: {
        uri: utils.pathToURI(resiPath),
      },
    };
    return response;
  } catch (e) {
    let response: p.ResponseMessage = {
      jsonrpc: c.jsonrpcVersion,
      id: msg.id,
      error: {
        code: p.ErrorCodes.InternalError,
        message: "Unable to create interface file.",
      },
    };
    return response;
  }
}

function openCompiledFile(msg: p.RequestMessage): p.Message {
  let params = msg.params as p.TextDocumentIdentifier;
  let filePath = fileURLToPath(params.uri);
  let projDir = utils.findProjectRootOfFile(filePath);

  if (projDir === null) {
    let params: p.ShowMessageParams = {
      type: p.MessageType.Error,
      message: `Cannot locate project directory.`,
    };

    let response: p.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params: params,
    };

    return response;
  }

  let compiledFilePath = utils.getCompiledFilePath(filePath, projDir);

  if (
    compiledFilePath.kind === "error" ||
    !fs.existsSync(compiledFilePath.result)
  ) {
    let message =
      compiledFilePath.kind === "success"
        ? `No compiled file found. Expected it at: ${compiledFilePath.result}`
        : `No compiled file found. Please compile your project first.`;

    let params: p.ShowMessageParams = {
      type: p.MessageType.Error,
      message,
    };

    let response: p.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params,
    };

    return response;
  }

  let response: p.ResponseMessage = {
    jsonrpc: c.jsonrpcVersion,
    id: msg.id,
    result: {
      uri: utils.pathToURI(compiledFilePath.result),
    },
  };

  return response;
}

function onMessage(msg: p.Message) {
  if (p.Message.isNotification(msg)) {
    // notification message, aka the client ends it and doesn't want a reply
    if (!initialized && msg.method !== "exit") {
      // From spec: "Notifications should be dropped, except for the exit notification. This will allow the exit of a server without an initialize request"
      // For us: do nothing. We don't have anything we need to clean up right now
      // TODO: we might have things we need to clean up now... like some watcher stuff
    } else if (msg.method === "exit") {
      // The server should exit with success code 0 if the shutdown request has been received before; otherwise with error code 1
      if (shutdownRequestAlreadyReceived) {
        process.exit(0);
      } else {
        process.exit(1);
      }
    } else if (msg.method === DidOpenTextDocumentNotification.method) {
      let params = msg.params as p.DidOpenTextDocumentParams;
      openedFile(params.textDocument.uri, params.textDocument.text);
      updateDiagnosticSyntax(params.textDocument.uri, params.textDocument.text);
    } else if (msg.method === DidChangeTextDocumentNotification.method) {
      let params = msg.params as p.DidChangeTextDocumentParams;
      let extName = path.extname(params.textDocument.uri);
      if (extName === c.resExt || extName === c.resiExt) {
        let changes = params.contentChanges;
        if (changes.length === 0) {
          // no change?
        } else {
          // we currently only support full changes
          updateOpenedFile(
            params.textDocument.uri,
            changes[changes.length - 1].text
          );
          updateDiagnosticSyntax(
            params.textDocument.uri,
            changes[changes.length - 1].text
          );
        }
      }
    } else if (msg.method === DidCloseTextDocumentNotification.method) {
      let params = msg.params as p.DidCloseTextDocumentParams;
      closedFile(params.textDocument.uri);
    } else if (msg.method === DidChangeConfigurationNotification.type.method) {
      // Can't seem to get this notification to trigger, but if it does this will be here and ensure we're synced up at the server.
      askForAllCurrentConfiguration();
    }
  } else if (p.Message.isRequest(msg)) {
    // request message, aka client sent request and waits for our mandatory reply
    if (!initialized && msg.method !== "initialize") {
      let response: p.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: msg.id,
        error: {
          code: p.ErrorCodes.ServerNotInitialized,
          message: "Server not initialized.",
        },
      };
      send(response);
    } else if (msg.method === "initialize") {
      // Save initial configuration, if present
      let initParams = msg.params as InitializeParams;
      let initialConfiguration = initParams.initializationOptions
        ?.extensionConfiguration as extensionConfiguration | undefined;

      if (initialConfiguration != null) {
        config.extensionConfiguration = initialConfiguration;
      }

      // These are static configuration options the client can set to enable certain
      let extensionClientCapabilitiesFromClient = initParams
        .initializationOptions?.extensionClientCapabilities as
        | extensionClientCapabilities
        | undefined;

      if (extensionClientCapabilitiesFromClient != null) {
        extensionClientCapabilities = extensionClientCapabilitiesFromClient;
      }

      extensionClientCapabilities.supportsSnippetSyntax = Boolean(
        initParams.capabilities.textDocument?.completion?.completionItem
          ?.snippetSupport
      );

      // send the list of features we support
      let result: p.InitializeResult = {
        // This tells the client: "hey, we support the following operations".
        // Example: we want to expose "jump-to-definition".
        // By adding `definitionProvider: true`, the client will now send "jump-to-definition" requests.
        capabilities: {
          // TODO: incremental sync?
          textDocumentSync: v.TextDocumentSyncKind.Full,
          documentFormattingProvider: true,
          hoverProvider: true,
          definitionProvider: true,
          typeDefinitionProvider: true,
          referencesProvider: true,
          codeActionProvider: true,
          renameProvider: { prepareProvider: true },
          documentSymbolProvider: true,
          completionProvider: {
            triggerCharacters: [".", ">", "@", "~", '"', "=", "("],
            resolveProvider: true,
          },
          semanticTokensProvider: {
            legend: {
              tokenTypes: [
                "operator",
                "variable",
                "type",
                "modifier", // emit jsx-tag < and > in <div> as modifier
                "namespace",
                "enumMember",
                "property",
                "interface", // emit jsxlowercase, div in <div> as interface
              ],
              tokenModifiers: [],
            },
            documentSelector: [{ scheme: "file", language: "rescript" }],
            // TODO: Support range for full, and add delta support
            full: true,
          },
          inlayHintProvider: config.extensionConfiguration.inlayHints?.enable,
          codeLensProvider: config.extensionConfiguration.codeLens
            ? {
                workDoneProgress: false,
              }
            : undefined,
          signatureHelpProvider: config.extensionConfiguration.signatureHelp
            ?.enabled
            ? {
                triggerCharacters: ["("],
                retriggerCharacters: ["=", ","],
              }
            : undefined,
        },
      };
      let response: p.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: msg.id,
        result: result,
      };
      initialized = true;

      // Periodically pull configuration from the client.
      pullConfigurationPeriodically = setInterval(() => {
        askForAllCurrentConfiguration();
      }, c.pullConfigurationInterval);

      send(response);
    } else if (msg.method === "initialized") {
      // sent from client after initialize. Nothing to do for now
      let response: p.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: msg.id,
        result: null,
      };
      send(response);
    } else if (msg.method === "shutdown") {
      // https://microsoft.github.io/language-server-protocol/specification#shutdown
      if (shutdownRequestAlreadyReceived) {
        let response: p.ResponseMessage = {
          jsonrpc: c.jsonrpcVersion,
          id: msg.id,
          error: {
            code: p.ErrorCodes.InvalidRequest,
            message: `Language server already received the shutdown request`,
          },
        };
        send(response);
      } else {
        shutdownRequestAlreadyReceived = true;
        // TODO: recheck logic around init/shutdown...
        stopWatchingCompilerLog();
        // TODO: delete bsb watchers

        if (pullConfigurationPeriodically != null) {
          clearInterval(pullConfigurationPeriodically);
        }

        let response: p.ResponseMessage = {
          jsonrpc: c.jsonrpcVersion,
          id: msg.id,
          result: null,
        };
        send(response);
      }
    } else if (msg.method === p.HoverRequest.method) {
      send(hover(msg));
    } else if (msg.method === p.DefinitionRequest.method) {
      send(definition(msg));
    } else if (msg.method === p.TypeDefinitionRequest.method) {
      send(typeDefinition(msg));
    } else if (msg.method === p.ReferencesRequest.method) {
      send(references(msg));
    } else if (msg.method === p.PrepareRenameRequest.method) {
      send(prepareRename(msg));
    } else if (msg.method === p.RenameRequest.method) {
      send(rename(msg));
    } else if (msg.method === p.DocumentSymbolRequest.method) {
      send(documentSymbol(msg));
    } else if (msg.method === p.CompletionRequest.method) {
      completion(msg).then(send);
    } else if (msg.method === p.CompletionResolveRequest.method) {
      send(completionResolve(msg));
    } else if (msg.method === p.SemanticTokensRequest.method) {
      send(semanticTokens(msg));
    } else if (msg.method === p.CodeActionRequest.method) {
      send(codeAction(msg));
    } else if (msg.method === p.DocumentFormattingRequest.method) {
      let responses = format(msg);
      responses.forEach((response) => send(response));
    } else if (msg.method === createInterfaceRequest.method) {
      send(createInterface(msg));
    } else if (msg.method === openCompiledFileRequest.method) {
      send(openCompiledFile(msg));
    } else if (msg.method === p.InlayHintRequest.method) {
      let params = msg.params as InlayHintParams;
      let extName = path.extname(params.textDocument.uri);
      if (extName === c.resExt) {
        send(inlayHint(msg));
      }
    } else if (msg.method === p.CodeLensRequest.method) {
      let params = msg.params as CodeLensParams;
      let extName = path.extname(params.textDocument.uri);
      if (extName === c.resExt) {
        send(codeLens(msg));
      }
    } else if (msg.method === p.SignatureHelpRequest.method) {
      let params = msg.params as SignatureHelpParams;
      let extName = path.extname(params.textDocument.uri);
      if (extName === c.resExt) {
        send(signatureHelp(msg));
      }
    } else {
      let response: p.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: msg.id,
        error: {
          code: p.ErrorCodes.InvalidRequest,
          message: "Unrecognized editor request.",
        },
      };
      send(response);
    }
  } else if (p.Message.isResponse(msg)) {
    if (msg.id === c.configurationRequestId) {
      if (msg.result != null) {
        // This is a response from a request to get updated configuration. Note
        // that it seems to return the configuration in a way that lets the
        // current workspace settings override the user settings. This is good
        // as we get started, but _might_ be problematic further down the line
        // if we want to support having several projects open at the same time
        // without their settings overriding eachother. Not a problem now though
        // as we'll likely only have "global" settings starting out.
        let [configuration] = msg.result as [
          extensionConfiguration | null | undefined
        ];
        if (configuration != null) {
          config.extensionConfiguration = configuration;
        }
      }
    } else if (
      msg.result != null &&
      // @ts-ignore
      msg.result.title != null &&
      // @ts-ignore
      msg.result.title === c.startBuildAction
    ) {
      let msg_ = msg.result as clientSentBuildAction;
      let projectRootPath = msg_.projectRootPath;
      // TODO: sometime stale .bsb.lock dangling
      // TODO: close watcher when lang-server shuts down. However, by Node's
      // default, these subprocesses are automatically killed when this
      // language-server process exits
      let rescriptBinaryPath = findRescriptBinary(projectRootPath);
      if (rescriptBinaryPath != null) {
        let bsbProcess = utils.runBuildWatcherUsingValidBuildPath(
          rescriptBinaryPath,
          projectRootPath
        );
        let root = projectsFiles.get(projectRootPath)!;
        root.bsbWatcherByEditor = bsbProcess;
        // bsbProcess.on("message", (a) => console.log(a));
      }
    }
  }
}

// Gate behind a debug setting potentially?
onErrorReported((msg) => {
  let params: p.ShowMessageParams = {
    type: p.MessageType.Warning,
    message: `ReScript tooling: Internal error. Something broke. Here's the error message that you can report if you want:
    
${msg}

(this message will only be reported once every 15 minutes)`,
  };
  let message: p.NotificationMessage = {
    jsonrpc: c.jsonrpcVersion,
    method: "window/showMessage",
    params: params,
  };
  send(message);
});
