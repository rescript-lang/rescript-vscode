import process, { allowedNodeEnvironmentFlags } from "process";
import * as p from "vscode-languageserver-protocol";
import * as t from "vscode-languageserver-types";
import * as j from "vscode-jsonrpc";
import * as m from "vscode-jsonrpc/lib/messages";
import * as v from "vscode-languageserver";
import * as path from 'path';
import fs from 'fs';
// TODO: check DidChangeWatchedFilesNotification. Check DidChangeTextDocumentNotification. Do they fire on uninitialized files?
import { DidOpenTextDocumentNotification, DidChangeTextDocumentNotification, DidCloseTextDocumentNotification, DidChangeWatchedFilesNotification, CompletionResolveRequest } from 'vscode-languageserver-protocol';
import { uriToFsPath, URI } from 'vscode-uri';
import * as utils from './utils';
import * as c from './constants';
import * as chokidar from 'chokidar'
import { assert } from 'console';
// TODO: what's this?
import { fileURLToPath } from 'url';

// https://microsoft.github.io/language-server-protocol/specification#initialize
// According to the spec, there could be requests before the 'initialize' request. Link in comment tells how to handle them.
let initialized = false;
// https://microsoft.github.io/language-server-protocol/specification#exit
let shutdownRequestAlreadyReceived = false;
let stupidFileContentCache: Map<string, string> = new Map()
/*
  Map {
    '/foo/lib/bs/.compiler.log': Map {
      '/foo/src/A.res': {
        trackedContent: 'let a = 1',
        hasDiagnostics: false
      }
      '/foo/src/B.res': {
        trackedContent: null,
        hasDiagnostics: true
      }
    },
  }
*/
let projectsFiles: Map<
  string,
  {
    openFiles: Set<string>,
    filesWithDiagnostics: Set<string>,
  }>
  = new Map()
// ^ caching AND states AND distributed system. Why does LSP has to be stupid like this

let sendUpdatedDiagnostics = () => {
  projectsFiles.forEach(({ filesWithDiagnostics }, compilerLogPath) => {
    let content = fs.readFileSync(compilerLogPath, { encoding: 'utf-8' });
    console.log("new log content: ", compilerLogPath, content)
    let { done, result: filesAndErrors } = utils.parseCompilerLogOutput(content)

    // diff
    Object.keys(filesAndErrors).forEach(file => {
      // send diagnostic
      let params: p.PublishDiagnosticsParams = {
        uri: file,
        diagnostics: filesAndErrors[file],
      }
      let notification: m.NotificationMessage = {
        jsonrpc: c.jsonrpcVersion,
        method: 'textDocument/publishDiagnostics',
        params: params,
      };
      process.send!(notification);

      filesWithDiagnostics.add(file)
    })
    if (done) {
      // clear old files
      filesWithDiagnostics.forEach(file => {
        if (filesAndErrors[file] == null) {
          // Doesn't exist in the new diagnostics. Clear this diagnostic
          let params: p.PublishDiagnosticsParams = {
            uri: file,
            diagnostics: [],
          }
          let notification: m.NotificationMessage = {
            jsonrpc: c.jsonrpcVersion,
            method: 'textDocument/publishDiagnostics',
            params: params,
          };
          process.send!(notification);
          filesWithDiagnostics.delete(file)
        }
      })
    }
  });
}
let deleteProjectDiagnostics = (compilerLogPath: string) => {
  let compilerLog = projectsFiles.get(compilerLogPath)
  if (compilerLog != null) {
    compilerLog.filesWithDiagnostics.forEach(file => {
      let params: p.PublishDiagnosticsParams = {
        uri: file,
        diagnostics: [],
      }
      let notification: m.NotificationMessage = {
        jsonrpc: c.jsonrpcVersion,
        method: 'textDocument/publishDiagnostics',
        params: params,
      };
      process.send!(notification);
    })

    projectsFiles.delete(compilerLogPath)
  }
}

let compilerLogsWatcher = chokidar.watch([])
  .on('all', (_e, changedPath) => {
    console.log('new log change', changedPath, Math.random())
    sendUpdatedDiagnostics()
  })
let stopWatchingCompilerLog = () => {
  // TODO: cleanup of compilerLogs?
  compilerLogsWatcher.close()
}

let openedFile = (fileUri: string, fileContent: string) => {
  let filePath = uriToFsPath(URI.parse(fileUri), true);

  stupidFileContentCache.set(filePath, fileContent)

  let compilerLogDir = utils.findDirOfFileNearFile(c.compilerLogPartialPath, filePath)
  if (compilerLogDir != null) {
    let compilerLogPath = path.join(compilerLogDir, c.compilerLogPartialPath);
    if (!projectsFiles.has(compilerLogPath)) {
      projectsFiles.set(compilerLogPath, { openFiles: new Set(), filesWithDiagnostics: new Set() })
      compilerLogsWatcher.add(compilerLogPath)
    }
    let compilerLog = projectsFiles.get(compilerLogPath)!
    compilerLog.openFiles.add(filePath)
    // no need to call sendUpdatedDiagnostics() here; the watcher add will
    // call the listener which calls it
  }
}
let closedFile = (fileUri: string) => {
  let filePath = uriToFsPath(URI.parse(fileUri), true);

  stupidFileContentCache.delete(filePath)

  let compilerLogDir = utils.findDirOfFileNearFile(c.compilerLogPartialPath, filePath)
  if (compilerLogDir != null) {
    let compilerLogPath = path.join(compilerLogDir, c.compilerLogPartialPath);
    let compilerLog = projectsFiles.get(compilerLogPath)
    if (compilerLog != null) {
      compilerLog.openFiles.delete(filePath)
      // clear diagnostics too if no open files open in said project
      if (compilerLog.openFiles.size === 0) {
        compilerLogsWatcher.unwatch(compilerLogPath)
        deleteProjectDiagnostics(compilerLogPath)
      }
    }
  }
}
let updateOpenedFile = (fileUri: string, fileContent: string) => {
  let filePath = uriToFsPath(URI.parse(fileUri), true)
  assert(stupidFileContentCache.has(filePath))
  stupidFileContentCache.set(filePath, fileContent)
}
let getOpenedFileContent = (fileUri: string) => {
  let filePath = uriToFsPath(URI.parse(fileUri), true)
  let content = stupidFileContentCache.get(filePath)!
  assert(content != null)
  return content
}

process.on('message', (a: (m.RequestMessage | m.NotificationMessage)) => {
  if ((a as m.RequestMessage).id == null) {
    // this is a notification message, aka client sent and forgot
    let aa = (a as m.NotificationMessage)
    if (!initialized && aa.method !== 'exit') {
      // From spec: "Notifications should be dropped, except for the exit notification. This will allow the exit of a server without an initialize request"
      // For us: do nothing. We don't have anything we need to clean up right now
      // TODO: think of fs watcher
    } else if (aa.method === 'exit') {
      // The server should exit with success code 0 if the shutdown request has been received before; otherwise with error code 1
      if (shutdownRequestAlreadyReceived) {
        process.exit(0)
      } else {
        process.exit(1)
      }
    } else if (aa.method === DidOpenTextDocumentNotification.method) {
      let params = (aa.params as p.DidOpenTextDocumentParams);
      let extName = path.extname(params.textDocument.uri)
      if (extName === c.resExt || extName === c.resiExt) {
        console.log("new file coming", params.textDocument.uri)
        openedFile(params.textDocument.uri, params.textDocument.text)
      }
    } else if (aa.method === DidChangeTextDocumentNotification.method) {
      let params = (aa.params as p.DidChangeTextDocumentParams);
      let extName = path.extname(params.textDocument.uri)
      if (extName === c.resExt || extName === c.resiExt) {
        let changes = params.contentChanges
        if (changes.length === 0) {
          // no change?
        } else {
          // we currently only support full changes
          updateOpenedFile(params.textDocument.uri, changes[changes.length - 1].text)
        }
      }
    } else if (aa.method === DidCloseTextDocumentNotification.method) {
      let params = (aa.params as p.DidCloseTextDocumentParams);
      closedFile(params.textDocument.uri)
    }
  } else {
    // this is a request message, aka client sent request, waits for our reply
    let aa = (a as m.RequestMessage)
    if (!initialized && aa.method !== 'initialize') {
      let response: m.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: aa.id,
        error: {
          code: m.ErrorCodes.ServerNotInitialized,
          message: "Server not initialized."
        }
      };
      process.send!(response);
    } else if (aa.method === 'initialize') {
      // startWatchingCompilerLog(process)
      // send the list of things we support
      let result: p.InitializeResult = {
        capabilities: {
          // TODO: incremental sync?
          textDocumentSync: v.TextDocumentSyncKind.Full,
          documentFormattingProvider: true,
        }
      }
      let response: m.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: aa.id,
        result: result,
      };
      initialized = true;
      process.send!(response);
    } else if (aa.method === 'initialized') {
      // sent from client after initialize. Nothing to do for now
      let response: m.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: aa.id,
        result: null,
      };
      process.send!(response);
    } else if (aa.method === 'shutdown') {
      // https://microsoft.github.io/language-server-protocol/specification#shutdown
      if (shutdownRequestAlreadyReceived) {
        let response: m.ResponseMessage = {
          jsonrpc: c.jsonrpcVersion,
          id: aa.id,
          error: {
            code: m.ErrorCodes.InvalidRequest,
            message: `Language server already received the shutdown request`
          }
        };
        process.send!(response);
      } else {
        shutdownRequestAlreadyReceived = true
        // TODO: recheck logic around init/shutdown...
        stopWatchingCompilerLog()

        let response: m.ResponseMessage = {
          jsonrpc: c.jsonrpcVersion,
          id: aa.id,
          result: null,
        };
        process.send!(response);
      }
    } else if (aa.method === p.DocumentFormattingRequest.method) {
      let params = (aa.params as p.DocumentFormattingParams)
      let filePath = uriToFsPath(URI.parse(params.textDocument.uri), true);
      let extension = path.extname(params.textDocument.uri);
      if (extension !== c.resExt && extension !== c.resiExt) {
        let response: m.ResponseMessage = {
          jsonrpc: c.jsonrpcVersion,
          id: aa.id,
          error: {
            code: m.ErrorCodes.InvalidRequest,
            message: `Not a ${c.resExt} or ${c.resiExt} file.`
          }
        };
        process.send!(response);
      } else {
        let nodeModulesParentPath = utils.findDirOfFileNearFile(c.bscPartialPath, filePath)
        if (nodeModulesParentPath == null) {
          let response: m.ResponseMessage = {
            jsonrpc: c.jsonrpcVersion,
            id: aa.id,
            error: {
              code: m.ErrorCodes.InvalidRequest,
              message: `Cannot find a nearby ${c.bscPartialPath}. It's needed for formatting.`,
            }
          };
          process.send!(response);
        } else {
          // code will always be defined here, even though technically it can be undefined
          let code = getOpenedFileContent(params.textDocument.uri)
          let formattedResult = utils.formatUsingValidBscPath(
            code,
            path.join(nodeModulesParentPath, c.bscPartialPath),
            extension === c.resiExt,
          );
          if (formattedResult.kind === 'success') {
            let result: p.TextEdit[] = [{
              range: {
                start: { line: 0, character: 0 },
                end: { line: Number.MAX_VALUE, character: Number.MAX_VALUE }
              },
              newText: formattedResult.result,
            }]
            let response: m.ResponseMessage = {
              jsonrpc: c.jsonrpcVersion,
              id: aa.id,
              result: result,
            };
            process.send!(response);
          } else {
            let response: m.ResponseMessage = {
              jsonrpc: c.jsonrpcVersion,
              id: aa.id,
              result: [],
              // technically a formatting failure should return the error but
              // since this is LSP... the idiom seems to be to silently return
              // nothing (to avoid an alert window each time on bad formatting)
              // while sending a diagnostic about the error afterward. We won't
              // send an extra diagnostic because the .compiler.log watcher
              // should have reported th won't send an extra diagnostic because
              // theiler.log watcher should have reported them.

              // error: {
              //  code: m.ErrorCodes.ParseError,
              //  message: formattedResult.error,
              // }
            };
            process.send!(response);
          }
        }
      }

    } else {
      let response: m.ResponseMessage = {
        jsonrpc: c.jsonrpcVersion,
        id: aa.id,
        error: {
          code: m.ErrorCodes.InvalidRequest,
          message: "Unrecognized editor request."
        }
      };
      process.send!(response);
    }
  }
})
