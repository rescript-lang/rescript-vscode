import * as lc from "vscode-languageclient/node";
import * as vscode from "vscode";
import * as path from "path";

export async function createClient(context: vscode.ExtensionContext) {
    const serverModule = context.asAbsolutePath(
        path.join("server", "out", "server.js")
    );
    // The debug options for the server
    // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
    let debugOptions = { execArgv: ["--nolazy", "--inspect=6009"] };

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    const serverOptions: lc.ServerOptions = {
        run: { module: serverModule, transport: lc.TransportKind.ipc },
        debug: {
            module: serverModule,
            transport: lc.TransportKind.ipc,
            options: debugOptions,
        },
    };

    // Options to control the language client
    const clientOptions: lc.LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "rescript" }],
        // We'll send the initial configuration in here, but this might be
        // problematic because every consumer of the LS will need to mimic this.
        // We'll leave it like this for now, but might be worth revisiting later on.
        initializationOptions: {
            extensionConfiguration: vscode.workspace.getConfiguration("rescript.settings"),
        },
    };

    const client = new lc.LanguageClient(
        "ReScriptLSP",
        "ReScript Language Server",
        serverOptions,
        clientOptions
    );

    client.registerProposedFeatures()

    return client;
}