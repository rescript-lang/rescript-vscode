import process from "process";

import * as p from "vscode-languageserver-protocol";
import * as t from "vscode-languageserver-types";
import * as j from "vscode-jsonrpc";
import * as m from "vscode-jsonrpc/lib/messages";
import * as v from "vscode-languageserver";
import { attachPartialResult } from 'vscode-languageserver/lib/progress';
import * as path from 'path';
import fs from 'fs';
import * as childProcess from 'child_process';
import { DidOpenTextDocumentNotification, DidChangeTextDocumentNotification, DidCloseTextDocumentNotification } from 'vscode-languageserver-protocol';
import * as tmp from 'tmp';

// See https://microsoft.github.io/language-server-protocol/specification Abstract Message
// version is fixed to 2.0
let jsonrpcVersion = '2.0'
let bscPartialPath = path.join('node_modules', '.bin', 'bsc')

// https://microsoft.github.io/language-server-protocol/specification#initialize
// According to the spec, there could be requests before the 'initialize' request. Link in comment tells how to handle them.
let initialized = false;

// congrats. A simple UI problem is now a distributed system problem
let stupidFileContentCache: { [key: string]: string } = {
}

let findDirOfFileNearFile = (fileToFind: p.DocumentUri, source: p.DocumentUri,) => {
	let dir = path.dirname(source)
	if (fs.existsSync(path.join(dir, fileToFind))) {
		return dir
	} else {
		if (dir === source) {
			// reached top
			return null
		} else {
			findDirOfFileNearFile(fileToFind, dir)
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
	// TODO: what if there's space in the path?
	let result;
	// library cleans up after itself. No need to manually remove temp file
	let tmpobj = tmp.fileSync();
	let extension = isInterface ? '.resi' : '.res';
	let fileToFormat = tmpobj.name + extension;
	fs.writeFileSync(fileToFormat, code, { encoding: 'utf-8' });
	try {
		result = childProcess.execSync(`${bscPath} -fmt ${fileToFormat}`)
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

process.on('message', (a: (m.RequestMessage | m.NotificationMessage)) => {
	if ((a as m.RequestMessage).id == null) {
		let aa = (a as m.NotificationMessage)
		if (!initialized && aa.method !== 'exit') {
			// From spec: "Notifications should be dropped, except for the exit notification. This will allow the exit of a server without an initialize request"
		} else if (aa.method === 'exit') {
			// nothing to do for now
		} else if (aa.method === DidOpenTextDocumentNotification.method) {
			let params = (aa.params as p.DidOpenTextDocumentParams);
			stupidFileContentCache[params.textDocument.uri] = params.textDocument.text;
		} else if (aa.method === DidChangeTextDocumentNotification.method) {
			let params = (aa.params as p.DidChangeTextDocumentParams);
			let changes = params.contentChanges
			if (changes.length === 0) {
				// no change?
			} else {
				// we currently only support full changes
				stupidFileContentCache[params.textDocument.uri] = changes[changes.length - 1].text;
			}
		} else if (aa.method === DidCloseTextDocumentNotification.method) {
			let params = (aa.params as p.DidCloseTextDocumentParams);
			delete stupidFileContentCache[params.textDocument.uri];
		}
	} else {
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
			(<any>process).send(response);
		} else if (aa.method === 'initialize') {
			// send the list of things we support
			// let param: p.InitializeParams = aa.params
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
			(<any>process).send(response);
		} else if (aa.method === 'initialized') {
			// sent from client after initialize. Nothing to do for now
			let response: m.ResponseMessage = {
				jsonrpc: jsonrpcVersion,
				id: aa.id,
				result: null,
			};
			(<any>process).send(response);
		} else if (aa.method === p.DocumentFormattingRequest.method) {
			let params = (aa.params as p.DocumentFormattingParams)
			let filePath = params.textDocument.uri.replace('file:', '')
			let extension = path.extname(params.textDocument.uri);
			if (extension !== '.res' && extension !== '.resi') {
				let response: m.ResponseMessage = {
					jsonrpc: jsonrpcVersion,
					id: aa.id,
					error: {
						code: m.ErrorCodes.InvalidRequest,
						message: `Not a .res or .resi file.`
					}
				};
				(<any>process).send(response);
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
					(<any>process).send(response);
				} else {
					// file to format potentially doesn't exist anymore because of races. But that's ok, the error from bsc should handle it
					let code = stupidFileContentCache[params.textDocument.uri];
					// TODO: error here?
					if (code === undefined) {
						console.log("wtf can't find file")
					}
					let formattedResult = formatUsingValidBscPath(
						code,
						path.join(nodeModulesParentPath, bscPartialPath),
						extension === '.resi',
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
						(<any>process).send(response);
					} else {
						let response: m.ResponseMessage = {
							jsonrpc: jsonrpcVersion,
							id: aa.id,
							error: {
								code: m.ErrorCodes.ParseError,
								message: formattedResult.error,
							}
						};
						(<any>process).send(response);
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
			(<any>process).send(response);
		}
	}
})
