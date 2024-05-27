import * as cp from "child_process";
import * as path from "path";
import {
  window,
  DiagnosticCollection,
  Diagnostic,
  Range,
  Position,
  DiagnosticSeverity,
  Uri,
  CodeAction,
  CodeActionKind,
  WorkspaceEdit,
  OutputChannel,
} from "vscode";
import { analysisProdPath, getAnalysisBinaryPath } from "../utils";

export type DiagnosticsResultCodeActionsMap = Map<
  string,
  { range: Range; codeAction: CodeAction }[]
>;

export type DiagnosticsResultFormat = Array<{
  name: string;
  kind: string;
  file: string;
  range: [number, number, number, number];
  message: string;
  annotate?: {
    line: number;
    character: number;
    text: string;
    action: string;
  };
}>;

let resultsToDiagnostics = (
  results: DiagnosticsResultFormat,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap
): {
  diagnosticsMap: Map<string, Diagnostic[]>;
} => {
  let diagnosticsMap: Map<string, Diagnostic[]> = new Map();

  results.forEach((item) => {
    {
      let startPos: Position, endPos: Position;
      let [startLine, startCharacter, endLine, endCharacter] = item.range;

      // Detect if this diagnostic is for the entire file. If so, reanalyze will
      // say that the issue is on line -1. This code below ensures
      // that the full file is highlighted, if that's the case.
      if (startLine < 0 || endLine < 0) {
        startPos = new Position(0, 0);
        endPos = new Position(99999, 0);
      } else {
        startPos = new Position(startLine, startCharacter);
        endPos = new Position(endLine, endCharacter);
      }

      let issueLocationRange = new Range(startPos, endPos);
      let diagnosticText = item.message.trim();

      let diagnostic = new Diagnostic(
        issueLocationRange,
        diagnosticText,
        DiagnosticSeverity.Warning
      );

      // Don't show reports about optional arguments.
      if (item.name.toLowerCase().includes("unused argument")) {
        return;
      }

      if (diagnosticsMap.has(item.file)) {
        diagnosticsMap.get(item.file).push(diagnostic);
      } else {
        diagnosticsMap.set(item.file, [diagnostic]);
      }

      // If reanalyze suggests a fix, we'll set that up as a refactor code
      // action in VSCode. This way, it'll be easy to suppress the issue
      // reported if wanted. We also save the range of the issue, so we can
      // leverage that to make looking up the code actions for each cursor
      // position very cheap.
      if (item.annotate != null) {
        {
          let { line, character, text, action } = item.annotate;
          let codeAction = new CodeAction(action);
          codeAction.kind = CodeActionKind.RefactorRewrite;

          let codeActionEdit = new WorkspaceEdit();

          // In the future, it would be cool to have an additional code action
          // here for automatically removing whatever the thing that's dead is.
          codeActionEdit.replace(
            Uri.parse(item.file),
            // Make sure the full line is replaced

            new Range(
              new Position(line, character),
              new Position(line, character)
            ),
            // reanalyze seems to add two extra spaces at the start of the line
            // content to replace.
            text
          );

          codeAction.edit = codeActionEdit;

          if (diagnosticsResultCodeActions.has(item.file)) {
            diagnosticsResultCodeActions
              .get(item.file)
              .push({ range: issueLocationRange, codeAction });
          } else {
            diagnosticsResultCodeActions.set(item.file, [
              { range: issueLocationRange, codeAction },
            ]);
          }
        }
      }

      if (item.message.endsWith(" is never used")) {
        {
          let codeAction = new CodeAction("Remove unused");
          codeAction.kind = CodeActionKind.RefactorRewrite;

          let codeActionEdit = new WorkspaceEdit();

          // In the future, it would be cool to have an additional code action
          // here for automatically removing whatever the thing that's dead is.
          codeActionEdit.replace(
            Uri.parse(item.file),
            // Make sure the full line is replaced

            new Range(
              new Position(item.range[0], item.range[1]),
              new Position(item.range[2], item.range[3])
            ),
            // reanalyze seems to add two extra spaces at the start of the line
            // content to replace.
            ""
          );

          codeAction.edit = codeActionEdit;

          if (diagnosticsResultCodeActions.has(item.file)) {
            diagnosticsResultCodeActions
              .get(item.file)
              .push({ range: issueLocationRange, codeAction });
          } else {
            diagnosticsResultCodeActions.set(item.file, [
              { range: issueLocationRange, codeAction },
            ]);
          }
        }
      }
    }
  });

  return {
    diagnosticsMap,
  };
};

export const runCodeAnalysisWithReanalyze = (
  targetDir: string | null,
  diagnosticsCollection: DiagnosticCollection,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap,
  outputChannel: OutputChannel
) => {
  let currentDocument = window.activeTextEditor.document;
  let cwd = targetDir ?? path.dirname(currentDocument.uri.fsPath);

  let binaryPath = getAnalysisBinaryPath();
  if (binaryPath === null) {
    window.showErrorMessage("Binary executable not found.", analysisProdPath);
    return;
  }

  let opts = ["reanalyze", "-json"];
  let p = cp.spawn(binaryPath, opts, {
    cwd,
  });

  if (p.stdout == null) {
    window.showErrorMessage("Something went wrong.");
    return;
  }

  let data = "";

  p.stdout.on("data", (d) => {
    data += d;
  });

  p.stderr?.on("data", (e) => {
    // Sometimes the compiler artifacts has been corrupted in some way, and
    // reanalyze will spit out a "End_of_file" exception. The solution is to
    // clean and rebuild the ReScript project, which we can tell the user about
    // here.
    if (e.includes("End_of_file")) {
      window.showErrorMessage(
        `Something went wrong trying to run reanalyze. Please try cleaning and rebuilding your ReScript project.`
      );
    } else {
      window.showErrorMessage(
        `Something went wrong trying to run reanalyze: '${e}'`
      );
    }
  });

  p.on("close", () => {
    diagnosticsResultCodeActions.clear();

    let json: DiagnosticsResultFormat | null = null;

    try {
      json = JSON.parse(data);
    } catch (e) {
      window
        .showErrorMessage(
          `Something went wrong when running the code analyzer.`,
          "See details in error log"
        )
        .then((_choice) => {
          outputChannel.show();
        });

      outputChannel.appendLine("\n\n>>>>");
      outputChannel.appendLine(
        "Parsing JSON from reanalyze failed. The raw, invalid JSON can be reproduced by following the instructions below. Please run that command and report the issue + failing JSON on the extension bug tracker: https://github.com/rescript-lang/rescript-vscode/issues"
      );
      outputChannel.appendLine(
        `> To reproduce, run "${binaryPath} ${opts.join(
          " "
        )}" in directory: "${cwd}"`
      );
      outputChannel.appendLine("\n");
    }

    if (json == null) {
      // If reanalyze failed for some reason we'll clear the diagnostics.
      diagnosticsCollection.clear();
      return;
    }

    let { diagnosticsMap } = resultsToDiagnostics(
      json,
      diagnosticsResultCodeActions
    );

    // This smoothens the experience of the diagnostics updating a bit by
    // clearing only the visible diagnostics that has been fixed after the
    // updated diagnostics has been applied.
    diagnosticsCollection.forEach((uri, _) => {
      if (!diagnosticsMap.has(uri.fsPath)) {
        diagnosticsCollection.delete(uri);
      }
    });

    diagnosticsMap.forEach((diagnostics, filePath) => {
      diagnosticsCollection.set(Uri.parse(filePath), diagnostics);
    });
  });
};
