import {
  ExtensionContext,
  StatusBarItem,
  Uri,
  ViewColumn,
  window,
} from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import * as fs from "fs";
import { createFileInTempDir } from "../utils";

export async function dumpServerState(
  client: LanguageClient,
  _context?: ExtensionContext,
  _statusBarItem?: StatusBarItem,
) {
  try {
    const result = await client.sendRequest("rescript/dumpServerState");
    const outputFile = createFileInTempDir("server_state", ".json");

    // Pretty-print JSON with stable ordering where possible
    const replacer = (_key: string, value: any) => {
      if (value instanceof Map) return Object.fromEntries(value);
      if (value instanceof Set) return Array.from(value);
      return value;
    };

    const json = JSON.stringify(result, replacer, 2);
    fs.writeFileSync(outputFile, json, { encoding: "utf-8" });

    await window.showTextDocument(Uri.parse(outputFile), {
      viewColumn: ViewColumn.Beside,
      preview: false,
    });
  } catch (e) {
    window.showErrorMessage(`Failed to dump server state: ${String(e)}`);
  }
}
