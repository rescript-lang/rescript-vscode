import * as vscode from "vscode";
import * as path from "path";
import * as fs from "fs";
import { findProjectRootOfFileInDir } from "./utils";

export async function registerDynamicJsonValidation(
  outputChannel?: vscode.OutputChannel,
): Promise<vscode.Disposable> {
  // Use workspace text document change events to detect when ReScript config files are opened
  const disposable = vscode.workspace.onDidOpenTextDocument(
    async (document) => {
      if (document.languageId === "json") {
        const fileName = path.basename(document.uri.fsPath);
        if (fileName === "rescript.json" || fileName === "bsconfig.json") {
          await tryEnableValidationForFile(document.uri, outputChannel);
        }
      }
    },
  );

  // Also check for already open documents
  const openDocuments = vscode.workspace.textDocuments;
  for (const document of openDocuments) {
    if (document.languageId === "json") {
      const fileName = path.basename(document.uri.fsPath);
      if (fileName === "rescript.json" || fileName === "bsconfig.json") {
        await tryEnableValidationForFile(document.uri, outputChannel);
      }
    }
  }

  return disposable;
}

async function tryEnableValidationForFile(
  uri: vscode.Uri,
  outputChannel?: vscode.OutputChannel,
): Promise<void> {
  try {
    const projectRootDir = findProjectRootOfFileInDir(uri.fsPath);

    if (projectRootDir) {
      const schemaPath = path.join(
        projectRootDir,
        "node_modules",
        "rescript",
        "docs",
        "docson",
        "build-schema.json",
      );

      if (fs.existsSync(schemaPath)) {
        const absoluteSchemaPath = "file://" + schemaPath;
        const fileName = path.basename(uri.fsPath);

        // Add this schema to the user's JSON validation settings
        const config = vscode.workspace.getConfiguration();
        const jsonSchemas =
          config.get<Array<{ fileMatch: string[]; url: string }>>(
            "json.schemas",
          ) || [];

        // Remove existing ReScript schemas for this specific file
        const filteredSchemas = jsonSchemas.filter(
          (schema) => !schema.fileMatch || !schema.fileMatch.includes(fileName),
        );

        // Add our new schema configuration
        const updatedSchemas = [
          ...filteredSchemas,
          {
            fileMatch: [fileName],
            url: absoluteSchemaPath,
          },
        ];

        // Update the configuration globally to avoid workspace pollution
        await config.update(
          "json.schemas",
          updatedSchemas,
          vscode.ConfigurationTarget.Global,
        );

        if (outputChannel) {
          outputChannel.appendLine(`JSON validation enabled for ${fileName}`);
        }
      }
    }
  } catch (error) {
    // Silently ignore errors to avoid annoying the user
    if (outputChannel) {
      outputChannel.appendLine(
        `Failed to enable JSON validation for ${uri.fsPath}: ${error}`,
      );
    }
  }
}

