import * as path from "path";
import { workspace, ExtensionContext, commands, languages } from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

import * as customCommands from "./commands";

let client: LanguageClient;

// let taskProvider = tasks.registerTaskProvider('Run ReScript build', {
// 	provideTasks: () => {
// 		// if (!rakePromise) {
// 		// 	rakePromise = getRakeTasks();
// 		// }
// 		// return rakePromise;

// 		// taskDefinition: TaskDefinition,
// 		// scope: WorkspaceFolder | TaskScope.Global | TaskScope.Workspace,
// 		// name: string,
// 		// source: string,
// 		// execution ?: ProcessExecution | ShellExecution | CustomExecution,
// 		// problemMatchers ?: string | string[]
// 		return [
// 			new Task(
// 				{
// 					type: 'bsb',
// 				},
// 				TaskScope.Workspace,
// 				// definition.task,
// 				'build and watch',
// 				'bsb',
// 				new ShellExecution(
// 					// `./node_modules/.bin/bsb -make-world -w`
// 					`pwd`
// 				),
// 				"Hello"
// 			)
// 		]
// 	},
// 	resolveTask(_task: Task): Task | undefined {
// 		// const task = _task.definition.task;
// 		// // A Rake task consists of a task and an optional file as specified in RakeTaskDefinition
// 		// // Make sure that this looks like a Rake task by checking that there is a task.
// 		// if (task) {
// 		// 	// resolveTask requires that the same definition object be used.
// 		// 	const definition: RakeTaskDefinition = <any>_task.definition;
// 		// 	return new Task(
// 		// 		definition,
// 		// 		definition.task,
// 		// 		'rake',
// 		// 		new vscode.ShellExecution(`rake ${definition.task}`)
// 		// 	);
// 		// }
// 		return undefined;
// 	}
// });

export function activate(context: ExtensionContext) {
  // The server is implemented in node
  let serverModule = context.asAbsolutePath(
    path.join("server", "out", "server.js")
  );
  // The debug options for the server
  // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
  let debugOptions = { execArgv: ["--nolazy", "--inspect=6009"] };

  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  let serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: debugOptions,
    },
  };

  // Options to control the language client
  let clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: "file", language: "rescript" }],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
    },
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    "ReScriptLSP",
    "ReScript Language Server",
    serverOptions,
    clientOptions
  );

  // Create a custom diagnostics collection, for cases where we want to report diagnostics
  // programatically from inside of the extension.
  let diagnosticsCollection = languages.createDiagnosticCollection("rescript");

  // Register custom commands
  commands.registerCommand("rescript-vscode.create_interface", () => {
    customCommands.createInterface(client);
  });

  commands.registerCommand("rescript-vscode.run_dead_code_analysis", () => {
    customCommands.deadCodeAnalysisWithReanalyze(diagnosticsCollection);
  });

  commands.registerCommand(
    "rescript-vscode.clear_dead_code_analysis_results",
    () => {
      diagnosticsCollection.clear();
    }
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
