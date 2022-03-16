import process from "process";
import * as p from "vscode-languageserver-protocol";
import * as m from "vscode-jsonrpc/lib/messages";
import * as v from "vscode-languageserver";
import * as rpc from "vscode-jsonrpc";
import * as path from "path";
import fs from "fs";
// TODO: check DidChangeWatchedFilesNotification.
import {
  DidOpenTextDocumentNotification,
  DidChangeTextDocumentNotification,
  DidCloseTextDocumentNotification,
} from "vscode-languageserver-protocol";
import * as utils from "./utils";
import * as c from "./constants";
import * as chokidar from "chokidar";
import { assert } from "console";
import { fileURLToPath } from "url";
import { ChildProcess } from "child_process";
import { WorkspaceEdit } from "vscode-languageserver";

// https://microsoft.github.io/language-server-protocol/specification#initialize
// According to the spec, there could be requests before the 'initialize' request. Link in comment tells how to handle them.
let initialized = false;
let serverSentRequestIdCounter = 0;
// https://microsoft.github.io/language-server-protocol/specification#exit
let shutdownRequestAlreadyReceived = false;
let stupidFileContentCache: Map<string, string> = new Map();
let projectsFiles: Map<
  string, // project root path
  {
    openFiles: Set<string>;
    filesWithDiagnostics: Set<string>;
    bsbWatcherByEditor: null | ChildProcess;
  }
> = new Map();
// ^ caching AND states AND distributed system. Why does LSP has to be stupid like this

// will be properly defined later depending on the mode (stdio/node-rpc)
let send: (msg: m.Message) => void = (_) => {};

interface CreateInterfaceRequestParams {
  uri: string;
}

let createInterfaceRequest = new v.RequestType<
  CreateInterfaceRequestParams,
  string,
  void
>("rescript-vscode.create_interface");

interface OpenCompiledFileParams {
  uri: string;
}

let openCompiledFileRequest = new v.RequestType<
  OpenCompiledFileParams,
  OpenCompiledFileParams,
  void
>("rescript-vscode.open_compiled");

let sendUpdatedDiagnostics = () => {
  projectsFiles.forEach(({ filesWithDiagnostics }, projectRootPath) => {
    let content = fs.readFileSync(
      path.join(projectRootPath, c.compilerLogPartialPath),
      { encoding: "utf-8" }
    );
    let { done, result: filesAndErrors } =
      utils.parseCompilerLogOutput(content);

    // diff
    Object.keys(filesAndErrors).forEach((file) => {
      let params: p.PublishDiagnosticsParams = {
        uri: file,
        diagnostics: filesAndErrors[file],
      };
      let notification: m.NotificationMessage = {
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
          let notification: m.NotificationMessage = {
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
      let notification: m.NotificationMessage = {
        jsonrpc: c.jsonrpcVersion,
        method: "textDocument/publishDiagnostics",
        params: params,
      };
      send(notification);
    });

    projectsFiles.delete(projectRootPath);
  }
};
let sendCompilationFinishedMessage = () => {
  let notification: m.NotificationMessage = {
    jsonrpc: c.jsonrpcVersion,
    method: "rescript/compilationFinished",
  };

  send(notification);
};

let compilerLogsWatcher = chokidar
  .watch([], {
    awaitWriteFinish: {
      stabilityThreshold: 1,
    },
  })
  .on("all", (_e, changedPath) => {
    sendUpdatedDiagnostics();
    sendCompilationFinishedMessage();
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
    if (!projectsFiles.has(projectRootPath)) {
      projectsFiles.set(projectRootPath, {
        openFiles: new Set(),
        filesWithDiagnostics: new Set(),
        bsbWatcherByEditor: null,
      });
      compilerLogsWatcher.add(
        path.join(projectRootPath, c.compilerLogPartialPath)
      );
    }
    let root = projectsFiles.get(projectRootPath)!;
    root.openFiles.add(filePath);
    let firstOpenFileOfProject = root.openFiles.size === 1;
    // check if .bsb.lock is still there. If not, start a bsb -w ourselves
    // because otherwise the diagnostics info we'll display might be stale
    let bsbLockPath = path.join(projectRootPath, c.bsbLock);
    if (firstOpenFileOfProject && !fs.existsSync(bsbLockPath)) {
      // TODO: sometime stale .bsb.lock dangling. bsb -w knows .bsb.lock is
      // stale. Use that logic
      // TODO: close watcher when lang-server shuts down
      if (utils.findNodeBuildOfProjectRoot(projectRootPath) != null) {
        let payload: clientSentBuildAction = {
          title: c.startBuildAction,
          projectRootPath: projectRootPath,
        };
        let params = {
          type: p.MessageType.Info,
          message: `Start a build for this project to get the freshest data?`,
          actions: [payload],
        };
        let request: m.RequestMessage = {
          jsonrpc: c.jsonrpcVersion,
          id: serverSentRequestIdCounter++,
          method: "window/showMessageRequest",
          params: params,
        };
        send(request);
        // the client might send us back the "start build" action, which we'll
        // handle in the isResponseMessage check in the message handling way
        // below
      } else {
        // we should send something to say that we can't find bsb.exe. But right now we'll silently not do anything
      }
    }

    // no need to call sendUpdatedDiagnostics() here; the watcher add will
    // call the listener which calls it
  }
};
let closedFile = (fileUri: string) => {
  let filePath = fileURLToPath(fileUri);

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
};
let getOpenedFileContent = (fileUri: string) => {
  let filePath = fileURLToPath(fileUri);
  let content = stupidFileContentCache.get(filePath)!;
  assert(content != null);
  return content;
};

// Start listening now!
// We support two modes: the regular node RPC mode for VSCode, and the --stdio
// mode for other editors The latter is _technically unsupported_. It's an
// implementation detail that might change at any time
if (process.argv.includes("--stdio")) {
  let writer = new rpc.StreamMessageWriter(process.stdout);
  let reader = new rpc.StreamMessageReader(process.stdin);
  // proper `this` scope for writer
  send = (msg: m.Message) => writer.write(msg);
  reader.listen(onMessage);
} else {
  // proper `this` scope for process
  send = (msg: m.Message) => process.send!(msg);
  process.on("message", onMessage);
}

function hover(msg: p.RequestMessage) {
  let params = msg.params as p.HoverParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let response = utils.runAnalysisCommand(
    filePath,
    ["hover", filePath, params.position.line, params.position.character],
    msg
  );
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
  let response: m.ResponseMessage = {
    jsonrpc: c.jsonrpcVersion,
    id: msg.id,
    result,
    // error: code and message set in case an exception happens during the definition request.
  };
  return response;
}

function prepareRename(msg: p.RequestMessage): m.ResponseMessage {
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
  let response: m.ResponseMessage = {
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
  let response = utils.runAnalysisCommand(
    filePath,
    ["documentSymbol", filePath],
    msg
  );
  return response;
}

function semanticTokens(msg: p.RequestMessage) {
  // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens
  let params = msg.params as p.SemanticTokensParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let code = getOpenedFileContent(params.textDocument.uri);
  let extension = path.extname(params.textDocument.uri);
  let tmpname = utils.createFileInTempDir(extension);
  fs.writeFileSync(tmpname, code, { encoding: "utf-8" });
  let response = utils.runAnalysisCommand(
    filePath,
    ["semanticTokens", tmpname],
    msg
  );
  fs.unlink(tmpname, () => null);
  return response;
}

function completion(msg: p.RequestMessage) {
  let params = msg.params as p.ReferenceParams;
  let filePath = fileURLToPath(params.textDocument.uri);
  let code = getOpenedFileContent(params.textDocument.uri);
  let tmpname = utils.createFileInTempDir();
  fs.writeFileSync(tmpname, code, { encoding: "utf-8" });
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

function format(msg: p.RequestMessage): Array<m.Message> {
  // technically, a formatting failure should reply with the error. Sadly
  // the LSP alert box for these error replies sucks (e.g. doesn't actually
  // display the message). In order to signal the client to display a proper
  // alert box (sometime with actionable buttons), we need to first send
  // back a fake success message (because each request mandates a
  // response), then right away send a server notification to display a
  // nicer alert. Ugh.
  let fakeSuccessResponse: m.ResponseMessage = {
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
    let response: m.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params: params,
    };
    return [fakeSuccessResponse, response];
  } else {
    // See comment on findBscNativeDirOfFile for why we need
    // to recursively search for bsc.exe upward
    let bscNativePath = utils.findBscNativeOfFile(filePath);
    if (bscNativePath === null) {
      let params: p.ShowMessageParams = {
        type: p.MessageType.Error,
        message: `Cannot find a nearby bsc.exe in rescript or bs-platform. It's needed for formatting.`,
      };
      let response: m.NotificationMessage = {
        jsonrpc: c.jsonrpcVersion,
        method: "window/showMessage",
        params: params,
      };
      return [fakeSuccessResponse, response];
    } else {
      // code will always be defined here, even though technically it can be undefined
      let code = getOpenedFileContent(params.textDocument.uri);
      let formattedResult = utils.formatUsingValidBscNativePath(
        code,
        bscNativePath,
        extension === c.resiExt
      );
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
        let response: m.ResponseMessage = {
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
}

function createInterface(msg: p.RequestMessage): m.Message {
  let params = msg.params as CreateInterfaceRequestParams;
  let extension = path.extname(params.uri);
  let filePath = fileURLToPath(params.uri);
  let bscNativePath = utils.findBscNativeOfFile(filePath);
  let projDir = utils.findProjectRootOfFile(filePath);
  let code = getOpenedFileContent(params.uri);
  let isReactComponent = code.includes("@react.component");

  if (bscNativePath === null || projDir === null) {
    let params: p.ShowMessageParams = {
      type: p.MessageType.Error,
      message: `Cannot find a nearby bsc.exe to generate the interface file.`,
    };

    let response: m.NotificationMessage = {
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

    let response: m.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params: params,
    };

    return response;
  }

  if (isReactComponent) {
    let params: p.ShowMessageParams = {
      type: p.MessageType.Error,
      message: `Creating an interface with @react.component is not currently supported.`,
    };

    let response: m.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params: params,
    };

    return response;
  }

  let resPartialPath = filePath.split(projDir)[1];

  // The .cmi filename may have a namespace suffix appended.
  let namespaceResult = utils.getNamespaceNameFromBsConfig(projDir);

  if (namespaceResult.kind === "error") {
    let params: p.ShowMessageParams = {
      type: p.MessageType.Error,
      message: `Error reading bsconfig file.`,
    };

    let response: m.NotificationMessage = {
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

    let response: m.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params,
    };

    return response;
  }

  let intfResult = utils.createInterfaceFileUsingValidBscExePath(
    filePath,
    cmiPath,
    bscNativePath
  );

  if (intfResult.kind === "success") {
    let response: m.ResponseMessage = {
      jsonrpc: c.jsonrpcVersion,
      id: msg.id,
      result: intfResult.result,
    };

    return response;
  } else {
    let response: m.ResponseMessage = {
      jsonrpc: c.jsonrpcVersion,
      id: msg.id,
      error: {
        code: m.ErrorCodes.InternalError,
        message: "Unable to create interface file.",
      },
    };

    return response;
  }
}

function openCompiledFile(msg: p.RequestMessage): m.Message {
  let params = msg.params as OpenCompiledFileParams;
  let filePath = fileURLToPath(params.uri);
  let projDir = utils.findProjectRootOfFile(filePath);

  if (projDir === null) {
    let params: p.ShowMessageParams = {
      type: p.MessageType.Error,
      message: `Cannot locate project directory.`,
    };

    let response: m.NotificationMessage = {
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

    let response: m.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params,
    };

    return response;
  }

  let result: OpenCompiledFileParams = {
    uri: compiledFilePath.result,
  };

  let response: m.ResponseMessage = {
    jsonrpc: c.jsonrpcVersion,
    id: msg.id,
    result,
  };

  return response;
}

function onMessage(msg: m.Message) {
  if (m.isNotificationMessage(msg)) {
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
      let extName = path.extname(params.textDocument.uri);
      if (extName === c.resExt || extName === c.resiExt) {
        openedFile(params.textDocument.uri, params.textDocument.text);
      }
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
        }
      }
    } else if (msg.method === DidCloseTextDocumentNotification.method) {
      let params = msg.params as p.DidCloseTextDocumentParams;
      closedFile(params.textDocument.uri);
    }
  } else if (m.isRequestMessage(msg)) {
    // request message, aka client sent request and waits for our mandatory reply
    if (!initialized && msg.method !== "initialize") {
      let response: m.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: msg.id,
        error: {
          code: m.ErrorCodes.ServerNotInitialized,
          message: "Server not initialized.",
        },
      };
      send(response);
    } else if (msg.method === "initialize") {
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
          renameProvider: { prepareProvider: true },
          // disabled right now until we use the parser to show non-stale symbols per keystroke
          // documentSymbolProvider: true,
          completionProvider: { triggerCharacters: [".", ">", "@", "~", '"'] },
          semanticTokensProvider: {
            legend: {
              tokenTypes: [
                "keyword-tag",
                "variable",
                "type",
                "jsx-tag",
                "namespace",
                "enumMember",
                "property",
                "jsx-lowercase"
              ],
              tokenModifiers: [],
            },
            documentSelector: null,
            // TODO: Support range for full, and add delta support
            full: true,
          },
        },
      };
      let response: m.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: msg.id,
        result: result,
      };
      initialized = true;
      send(response);
    } else if (msg.method === "initialized") {
      // sent from client after initialize. Nothing to do for now
      let response: m.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: msg.id,
        result: null,
      };
      send(response);
    } else if (msg.method === "shutdown") {
      // https://microsoft.github.io/language-server-protocol/specification#shutdown
      if (shutdownRequestAlreadyReceived) {
        let response: m.ResponseMessage = {
          jsonrpc: c.jsonrpcVersion,
          id: msg.id,
          error: {
            code: m.ErrorCodes.InvalidRequest,
            message: `Language server already received the shutdown request`,
          },
        };
        send(response);
      } else {
        shutdownRequestAlreadyReceived = true;
        // TODO: recheck logic around init/shutdown...
        stopWatchingCompilerLog();
        // TODO: delete bsb watchers

        let response: m.ResponseMessage = {
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
      send(completion(msg));
    } else if (msg.method === p.SemanticTokensRequest.method) {
      send(semanticTokens(msg));
    } else if (msg.method === p.DocumentFormattingRequest.method) {
      let responses = format(msg);
      responses.forEach((response) => send(response));
    } else if (msg.method === createInterfaceRequest.method) {
      send(createInterface(msg));
    } else if (msg.method === openCompiledFileRequest.method) {
      send(openCompiledFile(msg));
    } else {
      let response: m.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: msg.id,
        error: {
          code: m.ErrorCodes.InvalidRequest,
          message: "Unrecognized editor request.",
        },
      };
      send(response);
    }
  } else if (m.isResponseMessage(msg)) {
    // response message. Currently the client should have only sent a response
    // for asking us to start the build (see window/showMessageRequest in this
    // file)

    if (
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
      let found = utils.findNodeBuildOfProjectRoot(projectRootPath);
      if (found != null) {
        let bsbProcess = utils.runBuildWatcherUsingValidBuildPath(
          found.buildPath,
          found.isReScript,
          projectRootPath
        );
        let root = projectsFiles.get(projectRootPath)!;
        root.bsbWatcherByEditor = bsbProcess;
        // bsbProcess.on("message", (a) => console.log(a));
      }
    }
  }
}
