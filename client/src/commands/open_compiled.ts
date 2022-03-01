import * as fs from "fs";
import { window, Uri, ViewColumn } from "vscode";
import { LanguageClient, RequestType } from "vscode-languageclient/node";

interface OpenCompiledFileRequestParams {
  uri: string;
}

interface OpenCompiledFileResponseParams {
  uri: string;
}

let openCompiledFileRequest = new RequestType<
  OpenCompiledFileRequestParams,
  OpenCompiledFileResponseParams,
  void
>("rescript-vscode.open_compiled");

export const openCompiled = (client: LanguageClient) => {
  if (!client) {
    return window.showInformationMessage("Language server not running");
  }

  const editor = window.activeTextEditor;

  if (!editor) {
    return window.showInformationMessage("No active editor");
  }

  if (!fs.existsSync(editor.document.uri.fsPath)) {
    return window.showInformationMessage("Compiled file does not exist");
  }

  client
    .sendRequest(openCompiledFileRequest, {
      uri: editor.document.uri.toString(),
    })
    .then((response) => {
      const document = Uri.file(response.uri);

      return window.showTextDocument(document, {
        viewColumn: ViewColumn.Beside,
      });
    });
};
