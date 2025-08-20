import * as fs from "fs";
import { LanguageClient } from "vscode-languageclient/node";
import { window } from "vscode";
import { createInterfaceRequest } from "./create_interface";

export const switchImplIntf = async (client: LanguageClient) => {
  if (!client) {
    return window.showInformationMessage("Language server not running");
  }

  const editor = window.activeTextEditor;

  if (!editor) {
    return window.showInformationMessage("No active editor");
  }

  const isIntf = editor.document.uri.path.endsWith(".resi");
  const isImpl = editor.document.uri.path.endsWith(".res");

  if (!(isIntf || isImpl)) {
    await window.showInformationMessage(
      "This command only can run on *.res or *.resi files.",
    );
    return;
  }

  if (isIntf) {
    // *.res
    const newUri = editor.document.uri.with({
      path: editor.document.uri.path.slice(0, -1),
    });
    await window.showTextDocument(newUri, { preview: false });
    return;
  }

  if (!fs.existsSync(editor.document.uri.fsPath + "i")) {
    // if interface doesn't exist, ask the user before creating.
    const selection = await window.showInformationMessage(
      "Do you want to create an interface *.resi?",
      ...["No", "Yes"],
    );

    if (selection !== "Yes") return;

    // create interface
    await client.sendRequest(createInterfaceRequest, {
      uri: editor.document.uri.toString(),
    });
  }

  // *.resi
  const newUri = editor.document.uri.with({
    path: editor.document.uri.path + "i",
  });
  await window.showTextDocument(newUri, { preview: false });
  return;
};
