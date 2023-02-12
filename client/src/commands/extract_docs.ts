import * as p from "vscode-languageserver-protocol";
import { LanguageClient, RequestType } from "vscode-languageclient/node";
import { Position, Uri, window, workspace, WorkspaceEdit } from "vscode";
import path = require("path");

export const extractDocsRequest = new RequestType<
  p.TextDocumentIdentifier,
  string,
  void
>("textDocument/extractDocs");

export const extractDocs = async (client: LanguageClient) => {
  if (!client) {
    return window.showInformationMessage("Language server not running");
  }

  const editor = window.activeTextEditor;

  if (!editor) {
    return window.showInformationMessage("No active editor");
  }

  try {
    const docUri = editor.document.uri.toString();
    const res = await client.sendRequest(extractDocsRequest, {
      uri: docUri,
    });

    const newFile = Uri.parse(
      "untitled:" +
        path.join(
          workspace.workspaceFolders[0].uri.fsPath,
          `${path.basename(docUri)}.json`
        )
    );
    workspace.openTextDocument(newFile).then((document) => {
      const edit = new WorkspaceEdit();
      edit.insert(newFile, new Position(0, 0), JSON.stringify(res, null, 2));
      return workspace.applyEdit(edit).then((success) => {
        if (success) {
          window.showTextDocument(document);
        } else {
          window.showInformationMessage("Error!");
        }
      });
    });
  } catch (e) {
    console.error("failed", e);
  }
};
