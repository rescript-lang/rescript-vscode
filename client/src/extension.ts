import * as path from "path";
import * as lc from "vscode-languageclient/node";
import * as vscode from "vscode";

import * as customCommands from "./commands";
import { DiagnosticsResultCodeActionsMap } from "./commands/code_analysis";
import { createClient } from "./client";

let client: lc.LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext) {
  client = await createClient(context);
  
  await client.start()

  await initCommonContext(context, client)

  return client
}

async function registerCommand(ctx: vscode.ExtensionContext, name:string, cmd: (...args: any[]) => unknown) {
  const fullName = `rescript-vscode.${name}`;
  const register = vscode.commands.registerCommand(fullName, cmd)
  ctx.subscriptions.push(register)
}

async function initCommonContext(ctx: vscode.ExtensionContext, client: lc.LanguageClient) {

  // Register custom commands
  // @see https://github.com/microsoft/vscode/issues/45774#issuecomment-373423895
  registerCommand(ctx, "restart_language_server", async () => {
    await deactivate();
    while (ctx.subscriptions.length > 0) {
      try {
          ctx.subscriptions.pop()!.dispose();
      } catch (err) {
          console.error("Dispose error:", err);
      }
    }
    await activate(ctx);
    void vscode.window.showInformationMessage("rescript-vscode reloaded");
  })

  ctx.subscriptions.push(
    client.onNotification("rescript/compilationFinished", () => {
      if (inCodeAnalysisState.active) {
        customCommands.codeAnalysisWithReanalyze(
          inCodeAnalysisState.activatedFromDirectory,
          diagnosticsCollection,
          diagnosticsResultCodeActions
        );
      }
    })
  )
  
  registerCommand(ctx, "create_interface", async () => customCommands.createInterface(client));
  registerCommand(ctx, "open_compiled", async () => customCommands.openCompiled(client));
  registerCommand(ctx, "switch-impl-intf",async () => customCommands.switchImplIntf(client));


  const diagnosticsCollection = vscode.languages.createDiagnosticCollection("rescript");
  const diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap = new Map();
  const codeAnalysisRunningStatusBarItem = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Right
  );
  let inCodeAnalysisState: {
    active: boolean;
    activatedFromDirectory: string | null;
  } = { active: false, activatedFromDirectory: null };

  // Starts the code analysis mode.
  registerCommand(ctx, "start_code_analysis", async () => {
    // Save the directory this first ran from, and re-use that when continuously
    // running the analysis. This is so that the target of the analysis does not
    // change on subsequent runs, if there are multiple ReScript projects open
    // in the editor.
    let currentDocument = vscode.window.activeTextEditor.document;

    inCodeAnalysisState.active = true;

    // Pointing reanalyze to the dir of the current file path is fine, because
    // reanalyze will walk upwards looking for a bsconfig.json in order to find
    // the correct project root.
    inCodeAnalysisState.activatedFromDirectory = path.dirname(
      currentDocument.uri.fsPath
    );

    codeAnalysisRunningStatusBarItem.command =
      "rescript-vscode.stop_code_analysis";
    codeAnalysisRunningStatusBarItem.show();
    codeAnalysisRunningStatusBarItem.text = "$(debug-stop) Stop Code Analyzer";

    customCommands.codeAnalysisWithReanalyze(
      inCodeAnalysisState.activatedFromDirectory,
      diagnosticsCollection,
      diagnosticsResultCodeActions
    );
  });
  
  registerCommand(ctx, "stop_code_analysis", async () => {
    inCodeAnalysisState.active = false;
    inCodeAnalysisState.activatedFromDirectory = null;

    diagnosticsCollection.clear();
    diagnosticsResultCodeActions.clear();

    codeAnalysisRunningStatusBarItem.hide();
  });

  // Code Actions
  vscode.languages.registerCodeActionsProvider("rescript", {
    async provideCodeActions(document, rangeOrSelection) {
      const availableActions =
        diagnosticsResultCodeActions.get(document.uri.fsPath) ?? [];

      return availableActions
        .filter(
          ({ range }) =>
            range.contains(rangeOrSelection) || range.isEqual(rangeOrSelection)
        )
        .map(({ codeAction }) => codeAction);
    },
  })

  if (vscode.workspace.getConfiguration("rescript.settings").get<boolean>("autoRunCodeAnalysis")) {
    vscode.commands.executeCommand("rescript-vscode.start_code_analysis")
  }
}

export async function deactivate() {
  await client?.stop();
  client = undefined;
}