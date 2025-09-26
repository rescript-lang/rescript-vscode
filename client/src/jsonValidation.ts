import * as vscode from "vscode";
import * as path from "path";
import * as fs from "fs";
import { findProjectRootOfFileInDir } from "./utils";

export async function registerDynamicJsonValidation(
  outputChannel?: vscode.OutputChannel,
): Promise<vscode.Disposable> {
  // Ensure JSON extension is activated (so jsonDefaults is available)
  const jsonExt = vscode.extensions.getExtension(
    "vscode.json-language-features",
  );
  await jsonExt?.activate();

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

  // Also check already open documents
  for (const document of vscode.workspace.textDocuments) {
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
    if (!projectRootDir) {
      return;
    }

    const schemaPath = path.join(
      projectRootDir,
      "node_modules",
      "rescript",
      "docs",
      "docson",
      "build-schema.json",
    );

    if (!fs.existsSync(schemaPath)) {
      return;
    }

    const absoluteSchemaPath = "file://" + schemaPath;
    const fileName = path.basename(uri.fsPath);

    const jsonExt = vscode.extensions.getExtension(
      "vscode.json-language-features",
    );

    const jsonApi: any = await jsonExt?.activate();

    if (jsonApi?.jsonDefaults) {
      // Preferred path: directly update JSON diagnostics options
      jsonApi.jsonDefaults.setDiagnosticsOptions({
        validate: true,
        schemas: [
          {
            fileMatch: [fileName],
            uri: absoluteSchemaPath,
          },
        ],
      });

      outputChannel?.appendLine(
        `Dynamic JSON schema applied for ${fileName}: ${absoluteSchemaPath}`,
      );
    } else {
      // Fallback: update workspace settings if jsonDefaults not available
      const config = vscode.workspace.getConfiguration();
      const jsonSchemas =
        config.get<Array<{ fileMatch: string[]; url: string }>>(
          "json.schemas",
        ) || [];

      const filteredSchemas = jsonSchemas.filter(
        (schema) => !schema.fileMatch || !schema.fileMatch.includes(fileName),
      );

      const updatedSchemas = [
        ...filteredSchemas,
        {
          fileMatch: [fileName],
          url: absoluteSchemaPath,
        },
      ];

      await config.update(
        "json.schemas",
        updatedSchemas,
        vscode.ConfigurationTarget.Workspace,
      );

      outputChannel?.appendLine(
        `Fallback JSON schema configured for ${fileName} via settings: ${absoluteSchemaPath}`,
      );
    }
  } catch (error) {
    outputChannel?.appendLine(
      `Failed to enable JSON validation for ${uri.fsPath}: ${error}`,
    );
  }
}
