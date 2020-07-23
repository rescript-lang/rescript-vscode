/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext, tasks, Task, ShellExecution } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
	DefinitionRequest
} from 'vscode-languageclient';

let client: LanguageClient;

// let taskProvider = tasks.registerTaskProvider('bsb', {
// 	provideTasks: () => {
// 		// if (!rakePromise) {
// 		// 	rakePromise = getRakeTasks();
// 		// }
// 		// return rakePromise;
// 		return [
// 			new Task(
// 				{
// 					type: 'asd',
// 					task: 'asd2',
// 				},
// 				definition.task,
// 				'bsb run',
// 				new ShellExecution(
// 					`bsb run this`
// 				),
// 			)
// 		]
// 	},
// 	resolveTask(_task: Task): Task | undefined {
// 		const task = _task.definition.task;
// 		// A Rake task consists of a task and an optional file as specified in RakeTaskDefinition
// 		// Make sure that this looks like a Rake task by checking that there is a task.
// 		if (task) {
// 			// resolveTask requires that the same definition object be used.
// 			const definition: RakeTaskDefinition = <any>_task.definition;
// 			return new Task(
// 				definition,
// 				definition.task,
// 				'rake',
// 				new vscode.ShellExecution(`rake ${definition.task}`)
// 			);
// 		}
// 		return undefined;
// 	}
// });

export function activate(context: ExtensionContext) {
	// The server is implemented in node
	let serverModule = context.asAbsolutePath(
		path.join('server', 'out', 'testserver.js')
	);
	// The debug options for the server
	// --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
	let debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	let serverOptions: ServerOptions = {
		run: { module: serverModule, transport: TransportKind.ipc },
		debug: {
			module: serverModule,
			transport: TransportKind.ipc,
			options: debugOptions
		}
	};

	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [
			{ scheme: 'file', language: 'plaintext' },
			{ scheme: 'file', language: 'bucklescript' },
		],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'languageServerExample',
		'Language Server Example',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
