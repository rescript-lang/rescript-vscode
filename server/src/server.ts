import process from "process";

import * as p from "vscode-languageserver-protocol";
import * as t from "vscode-languageserver-types";
import * as j from "vscode-jsonrpc";
import * as m from "vscode-jsonrpc/lib/messages";
import * as v from "vscode-languageserver";
import * as path from 'path';
import fs from 'fs';
import * as childProcess from 'child_process';
import { DidOpenTextDocumentNotification, DidChangeTextDocumentNotification, DidCloseTextDocumentNotification } from 'vscode-languageserver-protocol';
import * as tmp from 'tmp';
import { Range } from 'vscode-languageserver-textdocument';

// See https://microsoft.github.io/language-server-protocol/specification Abstract Message
// version is fixed to 2.0
let jsonrpcVersion = '2.0';
let bscPartialPath = path.join('node_modules', 'bs-platform', process.platform, 'bsc.exe');
let bsbLogPartialPath = 'bsb.log';
let resExt = '.res';
let resiExt = '.resi';

// https://microsoft.github.io/language-server-protocol/specification#initialize
// According to the spec, there could be requests before the 'initialize' request. Link in comment tells how to handle them.
let initialized = false;
// https://microsoft.github.io/language-server-protocol/specification#exit
let shutdownRequestAlreadyReceived = false;
let diagnosisTimer: null | NodeJS.Timeout = null;

// congrats. A simple UI problem is now a distributed system problem
let stupidFileContentCache: { [key: string]: string } = {
}

let findDirOfFileNearFile = (fileToFind: p.DocumentUri, source: p.DocumentUri): null | p.DocumentUri => {
	let dir = path.dirname(source)
	if (fs.existsSync(path.join(dir, fileToFind))) {
		return dir
	} else {
		if (dir === source) {
			// reached top
			return null
		} else {
			return findDirOfFileNearFile(fileToFind, dir)
		}
	}
}

type formattingResult = {
	kind: 'success',
	result: string
} | {
	kind: 'error'
	error: string,
};
let formatUsingValidBscPath = (code: string, bscPath: p.DocumentUri, isInterface: boolean): formattingResult => {
	// library cleans up after itself. No need to manually remove temp file
	let tmpobj = tmp.fileSync();
	let extension = isInterface ? resiExt : resExt;
	let fileToFormat = tmpobj.name + extension;
	fs.writeFileSync(fileToFormat, code, { encoding: 'utf-8' });
	try {
		let result = childProcess.execFileSync(bscPath, ['-color', 'never', '-format', fileToFormat], { stdio: 'pipe' })
		return {
			kind: 'success',
			result: result.toString(),
		}
	} catch (e) {
		return {
			kind: 'error',
			error: e.message,
		}
	}
}

let parseBsbOutputLocation = (location: string): Range => {
	// example bsb output location:
	// 3:9
	// 3:5-8
	// 3:9-6:1

	// language-server position is 0-based. Ours is 1-based. Don't forget to convert
	// also, our end character is inclusive. Language-server's is exclusive
	let isRange = location.indexOf('-') >= 0
	if (isRange) {
		let [from, to] = location.split('-')
		let [fromLine, fromChar] = from.split(':')
		let isSingleLine = to.indexOf(':') >= 0
		let [toLine, toChar] = isSingleLine ? to.split(':') : [fromLine, to]
		return {
			start: { line: parseInt(fromLine) - 1, character: parseInt(fromChar) - 1 },
			end: { line: parseInt(toLine) - 1, character: parseInt(toChar) },
		}
	} else {
		let [line, char] = location.split(':')
		let start = { line: parseInt(line) - 1, character: parseInt(char) }
		return {
			start: start,
			end: start,
		}
	}
}

let parseBsbLogOutput = (content: string) => {
	/* example bsb.log file content:

Cleaning... 6 files.
Cleaning... 87 files.
[1/5] [34mBuilding[39m [2msrc/TestFramework.reiast[22m
[2/5] [34mBuilding[39m [2msrc/TestFramework.reast[22m
[3/5] Building src/test.resast
FAILED: src/test.resast
/Users/chenglou/github/bucklescript/darwin/bsc.exe   -bs-jsx 3 -bs-no-version-header -o src/test.resast -bs-syntax-only -bs-binary-ast /Users/chenglou/github/reason-react/src/test.res

  Syntax error!
  /Users/chenglou/github/reason-react/src/test.res 1:8-2:3

  1 │ let a =
  2 │ let b =
  3 │

  This let-binding misses an expression

[8/29] Building src/legacy/ReactDOMServerRe.reast
FAILED: src/test.cmj src/test.cmi

  Warning number 8
  /Users/chenglou/github/reason-react/src/test.res 3:5-8

  1 │ let a = j`😀`
  2 │ let b = `😀`
  3 │ let None = None
  4 │ let bla: int = "
  5 │   hi

  You forgot to handle a possible case here, for example:
  Some _

  We've found a bug for you!
  /Users/chenglou/github/reason-react/src/test.res 3:9

  1 │ let a = 1
  2 │ let b = "hi"
  3 │ let a = b + 1

  This has type:
    string

  But somewhere wanted:
    int


[15/62] [34mBuilding[39m [2msrc/ReactDOMServer.reast[22m
	*/

	// we're gonna chop that
	let res: string[][] = [];
	let lines = content.split('\n');
	for (let i = 0; i < lines.length; i++) {
		let line = lines[i];
		if (line.startsWith('  We\'ve found a bug for you!')) {
			res.push([])
		} else if (line.startsWith('  Warning number ')) {
			res.push([])
		} else if (line.startsWith('  Syntax error!')) {
			res.push([])
		} else if (/^  [0-9]+ /.test(line)) {
			// code display. Swallow
		} else if (line.startsWith('  ')) {
			res[res.length - 1].push(line)
		}
	}

	// map of file path to list of diagnosis
	let ret: { [key: string]: t.Diagnostic[] } = {}
	res.forEach(diagnosisLines => {
		let [fileAndLocation, ...diagnosisMessage] = diagnosisLines
		let lastSpace = fileAndLocation.lastIndexOf(' ')
		let file = fileAndLocation.substring(2, lastSpace)
		let location = fileAndLocation.substring(lastSpace)
		if (ret[file] == null) {
			ret[file] = []
		}
		let cleanedUpDiagnosis = diagnosisMessage
			.map(line => {
				// remove the spaces in front
				return line.slice(2)
			})
			.join('\n')
			// remove start and end whitespaces/newlines
			.trim();
		ret[file].push({
			range: parseBsbOutputLocation(location),
			message: cleanedUpDiagnosis,
		})
	})

	return ret
}

let startWatchingBsbOutputFile = (root: p.DocumentUri, process: NodeJS.Process) => {
	// TOOD: setTimeout instead
	let id = setInterval(() => {
		let openFiles = Object.keys(stupidFileContentCache);
		let bsbLogDirs: Set<p.DocumentUri> = new Set();
		openFiles.forEach(openFile => {
			// TODO: remove this hack
			let filePath = openFile.replace('file://', '');
			let bsbLogDir = findDirOfFileNearFile(bsbLogPartialPath, filePath)
			if (bsbLogDir != null) {
				bsbLogDirs.add(bsbLogDir);
			}
		});

		let files: { [key: string]: t.Diagnostic[] } = {}

		let res = Array.from(bsbLogDirs)
			.forEach(bsbLogDir => {
				let bsbLogPath = path.join(bsbLogDir, bsbLogPartialPath);
				let content = fs.readFileSync(bsbLogPath, { encoding: 'utf-8' });
				let filesAndErrors = parseBsbLogOutput(content)
				Object.keys(filesAndErrors).forEach(file => {
					// assumption: there's no existing files[file] entry
					// this is true; see the lines above. A file can only belong to one bsb.log root
					files[file] = filesAndErrors[file]
				})
			});

		Object.keys(files).forEach(file => {
			let params: p.PublishDiagnosticsParams = {
				uri: file,
				// there's a new optional version param from https://github.com/microsoft/language-server-protocol/issues/201
				// not using it for now, sigh
				diagnostics: files[file],
			}
			let notification: m.NotificationMessage = {
				jsonrpc: jsonrpcVersion,
				method: 'textDocument/publishDiagnostics',
				params: params,
			};
			process.send!(notification);
		})
	}, 1000);

	return id;
}
let stopWatchingBsbOutputFile = (timerId: NodeJS.Timeout) => {
	clearInterval(timerId);
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
			if (extName === resExt || extName === resiExt) {
				stupidFileContentCache[params.textDocument.uri] = params.textDocument.text;
			}
		} else if (aa.method === DidChangeTextDocumentNotification.method) {
			let params = (aa.params as p.DidChangeTextDocumentParams);
			let extName = path.extname(params.textDocument.uri)
			if (extName === resExt || extName === resiExt) {
				let changes = params.contentChanges
				if (changes.length === 0) {
					// no change?
				} else {
					// we currently only support full changes
					stupidFileContentCache[params.textDocument.uri] = changes[changes.length - 1].text;
				}
			}
		} else if (aa.method === DidCloseTextDocumentNotification.method) {
			let params = (aa.params as p.DidCloseTextDocumentParams);
			delete stupidFileContentCache[params.textDocument.uri];
		}
	} else {
		// this is a request message, aka client sent request, waits for our reply
		let aa = (a as m.RequestMessage)
		if (!initialized && aa.method !== 'initialize') {
			let response: m.ResponseMessage = {
				jsonrpc: jsonrpcVersion,
				id: aa.id,
				error: {
					code: m.ErrorCodes.ServerNotInitialized,
					message: "Server not initialized."
				}
			};
			process.send!(response);
		} else if (aa.method === 'initialize') {
			let param: p.InitializeParams = aa.params
			let root = param.rootUri
			if (root == null) {
				// TODO: handle single file
				console.log("not handling single file")
			} else {
				// diagnosisTimer = startWatchingBsbOutputFile(root, process)
			}
			// send the list of things we support
			let result: p.InitializeResult = {
				capabilities: {
					// TODO: incremental sync
					textDocumentSync: v.TextDocumentSyncKind.Full,
					documentFormattingProvider: true,
				}
			}
			let response: m.ResponseMessage = {
				jsonrpc: jsonrpcVersion,
				id: aa.id,
				result: result,
			};
			initialized = true;
			process.send!(response);
		} else if (aa.method === 'initialized') {
			// sent from client after initialize. Nothing to do for now
			let response: m.ResponseMessage = {
				jsonrpc: jsonrpcVersion,
				id: aa.id,
				result: null,
			};
			process.send!(response);
		} else if (aa.method === 'shutdown') {
			// https://microsoft.github.io/language-server-protocol/specification#shutdown
			if (shutdownRequestAlreadyReceived) {
				let response: m.ResponseMessage = {
					jsonrpc: jsonrpcVersion,
					id: aa.id,
					error: {
						code: m.ErrorCodes.InvalidRequest,
						message: `Language server already received the shutdown request`
					}
				};
				process.send!(response);
			} else {
				shutdownRequestAlreadyReceived = true
				if (diagnosisTimer != null) {
					stopWatchingBsbOutputFile(diagnosisTimer)
				}
				let response: m.ResponseMessage = {
					jsonrpc: jsonrpcVersion,
					id: aa.id,
					result: null,
				};
				process.send!(response);
			}
		} else if (aa.method === p.DocumentFormattingRequest.method) {
			let params = (aa.params as p.DocumentFormattingParams)
			// TODO: remove this hack
			let filePath = params.textDocument.uri.replace('file://', '')
			let extension = path.extname(params.textDocument.uri);
			if (extension !== resExt && extension !== resiExt) {
				let response: m.ResponseMessage = {
					jsonrpc: jsonrpcVersion,
					id: aa.id,
					error: {
						code: m.ErrorCodes.InvalidRequest,
						message: `Not a ${resExt} or ${resiExt} file.`
					}
				};
				process.send!(response);
			} else {
				let nodeModulesParentPath = findDirOfFileNearFile(bscPartialPath, filePath)
				if (nodeModulesParentPath == null) {
					let response: m.ResponseMessage = {
						jsonrpc: jsonrpcVersion,
						id: aa.id,
						error: {
							code: m.ErrorCodes.InvalidRequest,
							message: `Cannot find a nearby ${bscPartialPath}. It's needed for formatting.`,
						}
					};
					process.send!(response);
				} else {
					// file to format potentially doesn't exist anymore because of races. But that's ok, the error from bsc should handle it
					let code = stupidFileContentCache[params.textDocument.uri];
					// TODO: error here?
					if (code === undefined) {
						console.log("can't find file")
					}
					let formattedResult = formatUsingValidBscPath(
						code,
						path.join(nodeModulesParentPath, bscPartialPath),
						extension === resiExt,
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
							jsonrpc: jsonrpcVersion,
							id: aa.id,
							result: result,
						};
						process.send!(response);

						let params2: p.PublishDiagnosticsParams = {
							uri: params.textDocument.uri,
							// there's a new optional version param from https://github.com/microsoft/language-server-protocol/issues/201
							// not using it for now, sigh
							diagnostics: [],
						}
						let notification: m.NotificationMessage = {
							jsonrpc: jsonrpcVersion,
							method: 'textDocument/publishDiagnostics',
							params: params2,
						};
						process.send!(notification);
					} else {
						let response: m.ResponseMessage = {
							jsonrpc: jsonrpcVersion,
							id: aa.id,
							result: [],
							// technically a formatting failure should return the error but
							// since this is LSP... the idiom seems to be to silently return
							// nothing (to avoid an alert window each time on bad formatting)
							// while sending a diangosis about the error afterward

							// error: {
							// 	code: m.ErrorCodes.ParseError,
							// 	message: formattedResult.error,
							// }
						};
						process.send!(response);

						let filesAndErrors = parseBsbLogOutput(formattedResult.error)
						Object.keys(filesAndErrors).forEach(file => {
							let params2: p.PublishDiagnosticsParams = {
								uri: params.textDocument.uri,
								// there's a new optional version param from https://github.com/microsoft/language-server-protocol/issues/201
								// not using it for now, sigh
								diagnostics: filesAndErrors[file],
							}
							let notification: m.NotificationMessage = {
								jsonrpc: jsonrpcVersion,
								method: 'textDocument/publishDiagnostics',
								params: params2,
							};
							process.send!(notification);
						})
					}
				}
			}

		} else {
			let response: m.ResponseMessage = {
				jsonrpc: jsonrpcVersion,
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
