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
  DiagnosticTag,
} from "vscode";

export type DiagnosticsResultCodeActionsMap = Map<
  string,
  { range: Range; codeAction: CodeAction }[]
>;

let fileInfoRegex = /File "(.+)", line (\d)+, characters ([\d-]+)/g;

let extractFileInfo = (
  fileInfo: string
): {
  filePath: string;
  line: string;
  characters: string;
} | null => {
  let m;

  let filePath: string | null = null;
  let line: string | null = null;
  let characters: string | null = null;

  while ((m = fileInfoRegex.exec(fileInfo)) !== null) {
    if (m.index === fileInfoRegex.lastIndex) {
      fileInfoRegex.lastIndex++;
    }

    m.forEach((match: string, groupIndex: number) => {
      switch (groupIndex) {
        case 1: {
          filePath = match;
          break;
        }
        case 2: {
          line = match;
          break;
        }
        case 3: {
          characters = match;
          break;
        }
      }
    });
  }

  if (filePath != null && line != null && characters != null) {
    return {
      filePath,
      line,
      characters,
    };
  }

  return null;
};

let dceTextToDiagnostics = (
  dceText: string,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap
): {
  diagnosticsMap: Map<string, Diagnostic[]>;
} => {
  let diagnosticsMap: Map<string, Diagnostic[]> = new Map();

  // Each section with a single issue found is seprated by two line breaks in
  // the reanalyze output. The section contains information about the issue
  // itself, what line/char and in what file it was found, as well as a
  // suggestion for what you can replace the line containing the issue with to
  // suppress the issue reported.
  //
  // Here's an example of how a section typically looks:
  //
  // Warning Dead Value
  // File "/Users/zth/git/rescript-intro/src/Machine.res", line 2, characters 0-205
  // +use is never used
  // <-- line 2
  // @dead("+use") let use = (initialState: 'a, handleEvent: ('a, 'b) => 'a) => {
  dceText.split("\n\n").forEach((chunk) => {
    let [
      title,
      fileInfo,
      text,

      // These, if they exist, will power code actions for inserting the "fixed"
      // line that reanalyze might suggest.
      lineNumToReplace,
      lineContentToReplace,
    ] = chunk.split("\n");

    let processedFileInfo = extractFileInfo(fileInfo);

    if (processedFileInfo != null) {
      let [startCharacter, endCharacter] =
        processedFileInfo.characters.split("-");

      let startPos = new Position(
        // reanalyze reports lines as index 1 based, while VSCode wants them
        // index 0 based. reanalyze reports diagnostics for an entire file on
        // line 0 (and chars 0-0). So, we need to ensure that we don't give
        // VSCode a negative line index, or it'll be sad.
        Math.max(0, parseInt(processedFileInfo.line, 10) - 1),
        Math.max(0, parseInt(startCharacter, 10))
      );

      let endPos = new Position(
        Math.max(0, parseInt(processedFileInfo.line, 10) - 1),
        Math.max(0, parseInt(endCharacter, 10))
      );

      // Detect if this is a dead module diagnostic. If so, highlight the
      // entire file.
      if (title === "Warning Dead Module") {
        startPos = new Position(0, 0);
        endPos = new Position(99999, 0);
      }

      let issueLocationRange = new Range(startPos, endPos);

      let diagnostic = new Diagnostic(
        issueLocationRange,
        text.trim(),
        DiagnosticSeverity.Warning
      );

      // This will render the part of the code as unused
      diagnostic.tags = [DiagnosticTag.Unnecessary];

      if (diagnosticsMap.has(processedFileInfo.filePath)) {
        diagnosticsMap.get(processedFileInfo.filePath).push(diagnostic);
      } else {
        diagnosticsMap.set(processedFileInfo.filePath, [diagnostic]);
      }

      // If reanalyze suggests a fix, we'll set that up as a refactor code
      // action in VSCode. This way, it'll be easy to suppress the issue
      // reported if wanted. We also save the range of the issue, so we can
      // leverage that to make looking up the code actions for each cursor
      // position very cheap.
      if (lineNumToReplace != null && lineContentToReplace != null) {
        let actualLineToReplaceStr = lineNumToReplace.split("<-- line ").pop();

        if (actualLineToReplaceStr != null) {
          let codeAction = new CodeAction(`Suppress dead code warning`);
          codeAction.kind = CodeActionKind.RefactorRewrite;

          let codeActionEdit = new WorkspaceEdit();

          codeActionEdit.replace(
            Uri.parse(processedFileInfo.filePath),
            // Make sure the full line is replaced
            new Range(
              new Position(issueLocationRange.start.line, 0),
              new Position(issueLocationRange.start.line, 999999)
            ),
            lineContentToReplace.trim()
          );

          codeAction.edit = codeActionEdit;

          if (diagnosticsResultCodeActions.has(processedFileInfo.filePath)) {
            diagnosticsResultCodeActions
              .get(processedFileInfo.filePath)
              .push({ range: issueLocationRange, codeAction });
          } else {
            diagnosticsResultCodeActions.set(processedFileInfo.filePath, [
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

export const runDeadCodeAnalysisWithReanalyze = (
  targetDir: string | null,
  diagnosticsCollection: DiagnosticCollection,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap
) => {
  // Clear old diagnostics and code actions state.
  diagnosticsCollection.clear();
  diagnosticsResultCodeActions.clear();

  let currentDocument = window.activeTextEditor.document;
  let cwd = targetDir ?? path.dirname(currentDocument.uri.fsPath);

  let p = cp.spawn("npx", ["reanalyze", "-dce"], {
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
    let { diagnosticsMap } = dceTextToDiagnostics(
      data,
      diagnosticsResultCodeActions
    );

    diagnosticsMap.forEach((diagnostics, filePath) => {
      diagnosticsCollection.set(Uri.parse(filePath), diagnostics);
    });
  });
};
