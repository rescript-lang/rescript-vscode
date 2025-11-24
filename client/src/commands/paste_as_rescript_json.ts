import { env, window, Position, Selection, TextDocument } from "vscode";

const INDENT_SIZE = 2;
const INDENT_UNIT = " ".repeat(INDENT_SIZE);

const indent = (level: number) => INDENT_UNIT.repeat(level);

const isLikelyJson = (text: string): boolean => {
  const trimmed = text.trim();
  if (trimmed.length === 0) {
    return false;
  }
  const first = trimmed[0];
  return first === "{" || first === "[";
};

const ensureFloatString = (value: number): string => {
  const raw = Number.isFinite(value) ? String(value) : "0";
  if (raw.includes(".") || raw.includes("e") || raw.includes("E")) {
    return raw;
  }
  return `${raw}.`;
};

const formatJsonValue = (value: unknown, level = 0): string => {
  if (value === null) {
    return "JSON.Null";
  }

  switch (typeof value) {
    case "string":
      return `JSON.String(${JSON.stringify(value)})`;
    case "number":
      return `JSON.Number(${ensureFloatString(value)})`;
    case "boolean":
      return `JSON.Boolean(${value})`;
    case "object":
      if (Array.isArray(value)) {
        return formatArray(value, level);
      }
      return formatObject(value as Record<string, unknown>, level);
    default:
      return "JSON.Null";
  }
};

const formatObject = (
  value: Record<string, unknown>,
  level: number,
): string => {
  const entries = Object.entries(value);
  if (entries.length === 0) {
    return "JSON.Object(dict{})";
  }
  const nextLevel = level + 1;
  const lines = entries.map(
    ([key, val]) =>
      `${indent(nextLevel)}${JSON.stringify(key)}: ${formatJsonValue(
        val,
        nextLevel,
      )}`,
  );
  return `JSON.Object(dict{\n${lines.join(",\n")}\n${indent(level)}})`;
};

const formatArray = (values: unknown[], level: number): string => {
  if (values.length === 0) {
    return "JSON.Array([])";
  }
  const nextLevel = level + 1;
  const lines = values.map(
    (item) => `${indent(nextLevel)}${formatJsonValue(item, nextLevel)}`,
  );
  return `JSON.Array([\n${lines.join(",\n")}\n${indent(level)}])`;
};

export type JsonConversionResult =
  | { kind: "success"; formatted: string }
  | { kind: "notJson" }
  | { kind: "error"; errorMessage: string };

export const convertPlainTextToJsonT = (text: string): JsonConversionResult => {
  if (!isLikelyJson(text)) {
    return { kind: "notJson" };
  }

  try {
    const parsed = JSON.parse(text);
    // Only convert objects and arrays, not primitive values
    if (typeof parsed !== "object" || parsed === null) {
      return { kind: "notJson" };
    }
    return { kind: "success", formatted: formatJsonValue(parsed) };
  } catch {
    return {
      kind: "error",
      errorMessage: "Clipboard JSON could not be parsed.",
    };
  }
};

export const getBaseIndent = (
  document: TextDocument,
  position: Position,
): string => {
  const linePrefix = document
    .lineAt(position)
    .text.slice(0, position.character);
  return /^\s*$/.test(linePrefix) ? linePrefix : "";
};

export const applyBaseIndent = (formatted: string, baseIndent: string) => {
  if (baseIndent.length === 0) {
    return formatted;
  }

  return formatted
    .split("\n")
    .map((line, index) => (index === 0 ? line : `${baseIndent}${line}`))
    .join("\n");
};

export const buildInsertionText = (
  document: TextDocument,
  position: Position,
  formatted: string,
) => {
  const baseIndent = getBaseIndent(document, position);
  return applyBaseIndent(formatted, baseIndent);
};

const computeEndPosition = (
  insertionStart: Position,
  indentedText: string,
): Position => {
  const lines = indentedText.split("\n");
  if (lines.length === 1) {
    return insertionStart.translate(0, lines[0].length);
  }
  return new Position(
    insertionStart.line + lines.length - 1,
    lines[lines.length - 1].length,
  );
};

export const pasteAsRescriptJson = async () => {
  const editor = window.activeTextEditor;
  if (!editor) {
    window.showInformationMessage(
      "No active editor to paste the ReScript JSON into.",
    );
    return;
  }

  const clipboardText = await env.clipboard.readText();
  const conversion = convertPlainTextToJsonT(clipboardText);

  if (conversion.kind === "notJson") {
    window.showInformationMessage("Clipboard does not appear to contain JSON.");
    return;
  }

  if (conversion.kind === "error") {
    window.showErrorMessage("Clipboard JSON could not be parsed.");
    return;
  }

  const formatted = conversion.formatted;
  const selection = editor.selection;
  const indentedText = buildInsertionText(
    editor.document,
    selection.start,
    formatted,
  );
  const insertionStart = selection.start;
  const didEdit = await editor.edit((editBuilder) => {
    editBuilder.replace(selection, indentedText);
  });

  if (didEdit) {
    const endPosition = computeEndPosition(insertionStart, indentedText);
    editor.selection = new Selection(endPosition, endPosition);
  }
};
