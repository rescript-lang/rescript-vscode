import * as fs from "fs";
import { window, DiagnosticCollection } from "vscode";
import { LanguageClient, RequestType } from "vscode-languageclient/node";
import {
  DiagnosticsResultCodeActionsMap,
  runDeadCodeAnalysisWithReanalyze,
} from "./commands/dead_code_analysis";

interface CreateInterfaceRequestParams {
  uri: string;
}

let createInterfaceRequest = new RequestType<
  CreateInterfaceRequestParams,
  string,
  void
>("rescript-vscode.create_interface");

export const createInterface = (client: LanguageClient) => {
  if (!client) {
    return window.showInformationMessage("Language server not running");
  }

  const editor = window.activeTextEditor;

  if (!editor) {
    return window.showInformationMessage("No active editor");
  }

  if (fs.existsSync(editor.document.uri.fsPath + "i")) {
    return window.showInformationMessage("Interface file already exists");
  }

  client.sendRequest(createInterfaceRequest, {
    uri: editor.document.uri.toString(),
  });
};

export const deadCodeAnalysisWithReanalyze = (
  targetDir: string | null,
  diagnosticsCollection: DiagnosticCollection,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap
) => {
  runDeadCodeAnalysisWithReanalyze(
    targetDir,
    diagnosticsCollection,
    diagnosticsResultCodeActions
  );
};
