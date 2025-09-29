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
  // Check cache first
  if (schemaCache.has(projectRoot)) {
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

  if (!fs.existsSync(schemaPath)) {
    return null;
  }

  try {
    const schemaContent = fs.readFileSync(schemaPath, "utf8");
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

  // Convert file URI to filesystem path for project root detection
  let fsPath: string;
  try {
    fsPath = fileURLToPath(filePath);
  } catch (error) {
    console.error(`[JSON_CONFIG] Failed to convert file URI to path: ${error}`);
    return [];
  }

  const projectRoot = findProjectRootOfFile(fsPath);

  if (!projectRoot) {
    return [];
  }

  const schemaInfo = loadReScriptSchema(projectRoot);

  if (!schemaInfo) {
    return [];
  }

  try {
    const jsonContent = document.getText();
    const config = JSON.parse(jsonContent);

    let validate;
    try {
      // Create a copy of the schema and modify $ref to use Draft 07 instead of Draft 04
      const modifiedSchema = JSON.parse(JSON.stringify(schemaInfo.schema));
      if (
        modifiedSchema.$schema === "http://json-schema.org/draft-04/schema#"
      ) {
        modifiedSchema.$schema = "http://json-schema.org/draft-07/schema#";
      }
      validate = ajv.compile(modifiedSchema);
    } catch (schemaError) {
      console.error(`[JSON_CONFIG] Failed to compile schema:`, schemaError);
      return [];
    }

    const valid = validate(config);

    if (!valid && validate.errors) {
      console.error(
        `[JSON_CONFIG] Validation errors:`,
        JSON.stringify(validate.errors, null, 2),
      );
    }

    if (valid) {
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

        // Find the line containing the invalid property
        for (let i = 0; i < lines.length; i++) {
          const lineContent = lines[i];
          const match = lineContent.match(new RegExp(`"${propertyName}"\\s*:`));

          if (match && match.index !== undefined) {
            line = i;
            column = match.index;
            endColumn = column + match[0].length;
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
      return diagnostic;
    });

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

// Helper function to detect if JSON is minified (single line without meaningful whitespace)
function isMinifiedJson(jsonContent: string): boolean {
  const lineCount = jsonContent.split("\n").length;
  const trimmed = jsonContent.trim();

  // Consider it minified if it's just one line, or if it's very compact
  return lineCount === 1 || (lineCount <= 3 && trimmed.length < 200);
}

// Helper function to normalize JSON content for position calculations
// Only formats if the JSON appears to be truly minified (single line)
function normalizeJsonContent(jsonContent: string): string {
  try {
    // Only format if it's clearly minified (single line with no meaningful line breaks)
    if (isMinifiedJson(jsonContent)) {
      const parsed = JSON.parse(jsonContent);
      return JSON.stringify(parsed, null, 2);
    }

    // Otherwise, use original content to preserve manual formatting
    return jsonContent;
  } catch {
    // If parsing fails, return original content
    return jsonContent;
  }
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

  // Normalize the JSON content for position calculations
  const originalContent = document.getText();
  const normalizedContent = normalizeJsonContent(originalContent);

  // Split into lines for position calculation
  const formattedLines = normalizedContent.split("\n");

  // Make sure the position is valid
  if (position.line >= formattedLines.length) {
    return null;
  }

  const line = formattedLines[position.line];

  // Find all quoted strings on the line with their positions
  const quotes = [];
  const regex = /"([^"]+)"/g;
  let match;
  while ((match = regex.exec(line)) !== null) {
    quotes.push({
      text: match[0],
      value: match[1],
      start: match.index,
      end: match.index + match[0].length,
    });
  }

  // Find which quote contains the cursor position
  const cursorQuote = quotes.find(
    (q) => position.character >= q.start && position.character <= q.end,
  );

  if (!cursorQuote) {
    return null;
  }

  // Check if this is a property key (followed by colon) or property value
  const isPropertyKey = line.substring(cursorQuote.end).trim().startsWith(":");

  let propertyName;
  if (isPropertyKey) {
    // This is a property key, use it directly
    propertyName = cursorQuote.value;
  } else {
    // This is a property value, try to find the corresponding key
    // Look backwards to find the property key
    const beforeCursor = line.substring(0, cursorQuote.start);
    const keyMatch = beforeCursor.match(/"([^"]+)"\s*:\s*[^:]*$/);
    if (keyMatch) {
      propertyName = keyMatch[1];
    } else {
      // If we can't find the key, this might be an array value or nested property
      return null;
    }
  }

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
