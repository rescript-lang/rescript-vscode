import * as fs from "fs";
import * as path from "path";
import Ajv from "ajv/dist/2020.js";
import addFormats from "ajv-formats";
import {
  Diagnostic,
  DiagnosticSeverity,
  Position,
  Range,
  CompletionItem,
  CompletionItemKind,
  Hover,
  MarkupKind,
} from "vscode-languageserver";
import { TextDocument } from "vscode-languageserver-textdocument";
import { findProjectRootOfFile } from "./utils";
import { fileURLToPath } from "url";

const ajv = new Ajv({ allErrors: true });
addFormats(ajv);

interface SchemaInfo {
  schema: any;
  projectRoot: string;
  schemaPath: string;
}

// Cache schemas by project root
const schemaCache = new Map<string, SchemaInfo>();

function loadReScriptSchema(projectRoot: string): SchemaInfo | null {
  console.log(`[JSON_CONFIG] Loading schema for project: ${projectRoot}`);

  // Check cache first
  if (schemaCache.has(projectRoot)) {
    console.log(`[JSON_CONFIG] Schema found in cache`);
    return schemaCache.get(projectRoot)!;
  }

  const schemaPath = path.join(
    projectRoot,
    "node_modules",
    "rescript",
    "docs",
    "docson",
    "build-schema.json",
  );

  console.log(`[JSON_CONFIG] Looking for schema at: ${schemaPath}`);

  if (!fs.existsSync(schemaPath)) {
    console.log(`[JSON_CONFIG] Schema file does not exist`);
    return null;
  }

  try {
    const schemaContent = fs.readFileSync(schemaPath, "utf8");
    console.log(
      `[JSON_CONFIG] Schema content preview: ${schemaContent.substring(0, 500)}...`,
    );
    const schema = JSON.parse(schemaContent);

    const schemaInfo: SchemaInfo = {
      schema,
      projectRoot,
      schemaPath,
    };

    schemaCache.set(projectRoot, schemaInfo);
    return schemaInfo;
  } catch (error) {
    console.error(`Failed to load schema from ${schemaPath}:`, error);
    return null;
  }
}

export function isConfigFile(filePath: string): boolean {
  const fileName = path.basename(filePath);
  return fileName === "rescript.json" || fileName === "bsconfig.json";
}

export function validateConfig(document: TextDocument): Diagnostic[] {
  const filePath = document.uri;
  console.log(`[JSON_CONFIG] Validating config: ${filePath}`);

  // Convert file URI to filesystem path for project root detection
  let fsPath: string;
  try {
    fsPath = fileURLToPath(filePath);
    console.log(`[JSON_CONFIG] Converted to filesystem path: ${fsPath}`);
  } catch (error) {
    console.log(`[JSON_CONFIG] Failed to convert file URI to path: ${error}`);
    return [];
  }

  const projectRoot = findProjectRootOfFile(fsPath);
  console.log(`[JSON_CONFIG] Found project root: ${projectRoot}`);

  if (!projectRoot) {
    console.log(
      `[JSON_CONFIG] No project root found, returning empty diagnostics`,
    );
    return [];
  }

  const schemaInfo = loadReScriptSchema(projectRoot);
  console.log(
    `[JSON_CONFIG] Schema info: ${schemaInfo ? "found" : "not found"}`,
  );

  if (!schemaInfo) {
    console.log(`[JSON_CONFIG] No schema found, returning empty diagnostics`);
    return [];
  }

  try {
    const jsonContent = document.getText();
    console.log(
      `[JSON_CONFIG] JSON content: ${jsonContent.substring(0, 200)}...`,
    );
    const config = JSON.parse(jsonContent);

    let validate;
    try {
      validate = ajv.compile(schemaInfo.schema);
    } catch (schemaError) {
      console.error(`[JSON_CONFIG] Failed to compile schema:`, schemaError);
      return [];
    }

    const valid = validate(config);
    console.log(`[JSON_CONFIG] Validation result: ${valid}`);

    if (!valid && validate.errors) {
      console.log(
        `[JSON_CONFIG] Validation errors:`,
        JSON.stringify(validate.errors, null, 2),
      );
    }

    if (valid) {
      console.log(`[JSON_CONFIG] Valid JSON, returning empty diagnostics`);
      return [];
    }

    const diagnostics = (validate.errors || []).map((error) => {
      // Convert JSON pointer to line/column
      const lines = jsonContent.split("\n");
      let line = 0;
      let column = 0;
      let endColumn = 0;
      let propertyName = "";

      if (error.instancePath) {
        // Simple heuristic to find the location
        const path = error.instancePath.slice(1); // Remove leading '/'
        const pathParts = path.split("/");
        propertyName = pathParts[pathParts.length - 1];

        // Find the line containing the property
        for (let i = 0; i < lines.length; i++) {
          const lineContent = lines[i];
          const match = lineContent.match(new RegExp(`"${propertyName}"\\s*:`));

          if (match && match.index !== undefined) {
            line = i;
            column = match.index;
            endColumn = column + match[0].length;
            console.log(
              `[JSON_CONFIG] Found property "${propertyName}" at line ${line}, col ${column}, match: "${match[0]}"`,
            );
            break;
          }
        }
      } else if (
        error.keyword === "additionalProperties" &&
        error.params &&
        error.params.additionalProperty
      ) {
        // Handle additionalProperties error - extract the invalid property name
        propertyName = error.params.additionalProperty;
        console.log(
          `[JSON_CONFIG] Found additionalProperties error for property: "${propertyName}"`,
        );

        // Find the line containing the invalid property
        for (let i = 0; i < lines.length; i++) {
          const lineContent = lines[i];
          const match = lineContent.match(new RegExp(`"${propertyName}"\\s*:`));

          if (match && match.index !== undefined) {
            line = i;
            column = match.index;
            endColumn = column + match[0].length;
            console.log(
              `[JSON_CONFIG] Found invalid property "${propertyName}" at line ${line}, col ${column}, match: "${match[0]}"`,
            );
            break;
          }
        }
      }

      // Create a better error message
      let message = `${error.keyword}: ${error.message}`;
      if (error.keyword === "additionalProperties" && propertyName) {
        message = `Property "${propertyName}" is not allowed in the schema`;
      }

      const diagnostic = {
        severity:
          error.keyword === "additionalProperties"
            ? DiagnosticSeverity.Warning
            : DiagnosticSeverity.Error,
        range: {
          start: { line, character: column },
          end: { line, character: endColumn },
        },
        message,
        source: "rescript-json-config-schema",
      };
      console.log(
        `[JSON_CONFIG] Created diagnostic for property "${propertyName}":`,
        diagnostic,
      );
      console.log(
        `[JSON_CONFIG] Diagnostic details - Line: ${line}, Column: ${column}, EndColumn: ${endColumn}, Message: ${message}`,
      );
      return diagnostic;
    });

    console.log(
      `[JSON_CONFIG] Total diagnostics created: ${diagnostics.length}`,
    );
    if (diagnostics.length > 0) {
      console.log(`[JSON_CONFIG] First diagnostic:`, diagnostics[0]);
    }

    return diagnostics;
  } catch (error) {
    // Handle JSON parsing errors
    if (error instanceof SyntaxError) {
      const match = error.message.match(/position (\d+)/);
      if (match) {
        const position = parseInt(match[1]);
        const content = document.getText();
        const lines = content.substring(0, position).split("\n");
        const line = lines.length - 1;
        const character = lines[lines.length - 1].length;

        return [
          {
            severity: DiagnosticSeverity.Error,
            range: {
              start: { line, character },
              end: { line, character: character + 1 },
            },
            message: `JSON syntax error: ${error.message.replace(/.*: /, "")}`,
            source: "rescript-json-config-schema",
          },
        ];
      } else {
        return [
          {
            severity: DiagnosticSeverity.Error,
            range: {
              start: { line: 0, character: 0 },
              end: { line: 0, character: 1 },
            },
            message: `JSON syntax error: ${error.message}`,
            source: "rescript-json-config-schema",
          },
        ];
      }
    }

    return [
      {
        severity: DiagnosticSeverity.Error,
        range: {
          start: { line: 0, character: 0 },
          end: { line: 0, character: 1 },
        },
        message: `Failed to parse JSON: ${error}`,
        source: "rescript-json-config-schema",
      },
    ];
  }
}

export function getConfigCompletions(document: TextDocument): CompletionItem[] {
  const filePath = document.uri;
  let fsPath: string;
  try {
    fsPath = fileURLToPath(filePath);
  } catch (error) {
    console.log(
      `[JSON_CONFIG] Failed to convert file URI to path for completions: ${error}`,
    );
    return [];
  }
  const projectRoot = findProjectRootOfFile(fsPath);

  if (!projectRoot) {
    return [];
  }

  const schemaInfo = loadReScriptSchema(projectRoot);
  if (!schemaInfo?.schema?.properties) {
    return [];
  }

  return Object.entries(schemaInfo.schema.properties).map(
    ([key, prop]: [string, any]) => {
      const item: CompletionItem = {
        label: key,
        kind: CompletionItemKind.Property,
        detail: prop.description || key,
        insertText: `"${key}": `,
      };

      if (prop.type === "boolean") {
        item.insertText = `"${key}": ${prop.default !== undefined ? prop.default : false}`;
      } else if (prop.type === "array" && prop.items?.enum) {
        item.insertText = `"${key}": [\n  ${prop.items.enum.map((v: string) => `"${v}"`).join(",\n  ")}\n]`;
      } else if (prop.enum) {
        item.insertText = `"${key}": "${prop.default || prop.enum[0]}"`;
      }

      return item;
    },
  );
}

export function getConfigHover(
  document: TextDocument,
  position: Position,
): Hover | null {
  const filePath = document.uri;
  let fsPath: string;
  try {
    fsPath = fileURLToPath(filePath);
  } catch (error) {
    console.log(
      `[JSON_CONFIG] Failed to convert file URI to path for hover: ${error}`,
    );
    return null;
  }
  const projectRoot = findProjectRootOfFile(fsPath);

  if (!projectRoot) {
    return null;
  }

  const schemaInfo = loadReScriptSchema(projectRoot);
  if (!schemaInfo?.schema?.properties) {
    return null;
  }

  const line = document.getText(
    Range.create(position.line, 0, position.line, 1000),
  );
  const match = line.match(/"([^"]+)"/);

  if (!match) {
    return null;
  }

  const propertyName = match[1];
  const property = schemaInfo.schema.properties[propertyName];

  if (property?.description) {
    return {
      contents: {
        kind: MarkupKind.Markdown,
        value: property.description,
      },
    };
  }

  return null;
}

export function clearSchemaCache(): void {
  schemaCache.clear();
}
