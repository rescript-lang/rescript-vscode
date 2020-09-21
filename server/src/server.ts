import process from "process";
import * as p from "vscode-languageserver-protocol";
import * as t from "vscode-languageserver-types";
import * as j from "vscode-jsonrpc";
import * as m from "vscode-jsonrpc/lib/messages";
import * as v from "vscode-languageserver";
import * as path from 'path';
import fs from 'fs';
// TODO: check DidChangeWatchedFilesNotification.
import { DidOpenTextDocumentNotification, DidChangeTextDocumentNotification, DidCloseTextDocumentNotification, DidChangeWatchedFilesNotification, CompletionResolveRequest } from 'vscode-languageserver-protocol';
import * as utils from './utils';
import * as c from './constants';
import * as chokidar from 'chokidar'
import { assert } from 'console';
import { fileURLToPath } from 'url';

// https://microsoft.github.io/language-server-protocol/specification#initialize
// According to the spec, there could be requests before the 'initialize' request. Link in comment tells how to handle them.
let initialized = false;
// https://microsoft.github.io/language-server-protocol/specification#exit
let shutdownRequestAlreadyReceived = false;
let stupidFileContentCache: Map<string, string> = new Map()
let projectsFiles: Map<
  string, // project root path
  {
    openFiles: Set<string>,
    filesWithDiagnostics: Set<string>,
  }>
  = new Map()
// ^ caching AND states AND distributed system. Why does LSP has to be stupid like this

let sendUpdatedDiagnostics = () => {
  projectsFiles.forEach(({ filesWithDiagnostics }, projectRootPath) => {
    let content = fs.readFileSync(path.join(projectRootPath, c.compilerLogPartialPath), { encoding: 'utf-8' });
    let { done, result: filesAndErrors } = utils.parseCompilerLogOutput(content)

    // diff
    Object.keys(filesAndErrors).forEach(file => {
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
let deleteProjectDiagnostics = (projectRootPath: string) => {
  let root = projectsFiles.get(projectRootPath)
  if (root != null) {
    root.filesWithDiagnostics.forEach(file => {
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

    projectsFiles.delete(projectRootPath)
  }
}

let compilerLogsWatcher = chokidar.watch([])
  .on('all', (_e, changedPath) => {
    sendUpdatedDiagnostics()
  })
let stopWatchingCompilerLog = () => {
  // TODO: cleanup of compilerLogs?
  compilerLogsWatcher.close()
}

let openedFile = (fileUri: string, fileContent: string) => {
  let filePath = fileURLToPath(fileUri)

  stupidFileContentCache.set(filePath, fileContent)

  let projectRootPath = utils.findProjectRootOfFile(filePath)
  if (projectRootPath != null) {
    if (!projectsFiles.has(projectRootPath)) {
      projectsFiles.set(projectRootPath, { openFiles: new Set(), filesWithDiagnostics: new Set() })
      compilerLogsWatcher.add(path.join(projectRootPath, c.compilerLogPartialPath))
    }
    let root = projectsFiles.get(projectRootPath)!
    root.openFiles.add(filePath)
    // no need to call sendUpdatedDiagnostics() here; the watcher add will
    // call the listener which calls it
  }
}
let closedFile = (fileUri: string) => {
  let filePath = fileURLToPath(fileUri)

  stupidFileContentCache.delete(filePath)

  let projectRootPath = utils.findProjectRootOfFile(filePath)
  if (projectRootPath != null) {
    let root = projectsFiles.get(projectRootPath)
    if (root != null) {
      root.openFiles.delete(filePath)
      // clear diagnostics too if no open files open in said project
      if (root.openFiles.size === 0) {
        compilerLogsWatcher.unwatch(path.join(projectRootPath, c.compilerLogPartialPath))
        deleteProjectDiagnostics(projectRootPath)
      }
    }
  }
}
let updateOpenedFile = (fileUri: string, fileContent: string) => {
  let filePath = fileURLToPath(fileUri)
  assert(stupidFileContentCache.has(filePath))
  stupidFileContentCache.set(filePath, fileContent)
}
let getOpenedFileContent = (fileUri: string) => {
  let filePath = fileURLToPath(fileUri)
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
      let filePath = fileURLToPath(params.textDocument.uri)
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
        let projectRootPath = utils.findProjectRootOfFile(filePath)
        if (projectRootPath == null) {
          let response: m.ResponseMessage = {
            jsonrpc: c.jsonrpcVersion,
            id: aa.id,
            error: {
              code: m.ErrorCodes.InvalidRequest,
              message: `Cannot find a nearby ${c.bsconfigPartialPath}. It's needed for determining the project's root.`,
            }
          };
          process.send!(response);
        } else {
          let bscPath = path.join(projectRootPath, c.bscPartialPath)
          if (!fs.existsSync(bscPath)) {
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
              bscPath,
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
