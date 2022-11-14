import * as fs from "fs";
import * as p from "vscode-languageserver-protocol";
import { LanguageClient, RequestType } from "vscode-languageclient/node";
import { window } from "vscode";

export const createInterfaceRequest = new RequestType<
  p.TextDocumentIdentifier,
  p.TextDocumentIdentifier,
  void
>("textDocument/createInterface");

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
