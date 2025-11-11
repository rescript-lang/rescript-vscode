import { env, window, Position, Selection } from "vscode";

import { buildInsertionText } from "./paste_as_rescript_json";
import { transformJsx } from "./transform-jsx";

export type JsxConversionResult =
  | { kind: "success"; formatted: string }
  | { kind: "empty" }
  | { kind: "error"; errorMessage: string };

export const convertPlainTextToRescriptJsx = (
  text: string,
): JsxConversionResult => {
  if (text.trim().length === 0) {
    return { kind: "empty" };
  }

  try {
    const formatted = transformJsx(text);
    return { kind: "success", formatted };
  } catch (error) {
    const errorMessage =
      error instanceof Error ? error.message : "Unknown conversion error.";
    return {
      kind: "error",
      errorMessage,
    };
  }
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

export const pasteAsRescriptJsx = async () => {
  const editor = window.activeTextEditor;
  if (!editor) {
    window.showInformationMessage(
      "No active editor to paste the ReScript JSX into.",
    );
    return;
  }

  const clipboardText = await env.clipboard.readText();
  const conversion = convertPlainTextToRescriptJsx(clipboardText);

  if (conversion.kind === "empty") {
    window.showInformationMessage(
      "Clipboard does not appear to contain any JSX content.",
    );
    return;
  }

  if (conversion.kind === "error") {
    window.showErrorMessage(
      `Clipboard JSX could not be transformed: ${conversion.errorMessage}`,
    );
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
