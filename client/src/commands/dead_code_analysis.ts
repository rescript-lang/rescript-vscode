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
  // the reanalyze output. Here's an example of how a section typically looks:
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

      // These aren't in use yet, but can power code actions that can
      // automatically add the @dead annotations reanalyze is suggesting to us.
      lineNumToReplace,
      lineContentToReplace,

      ..._rest
    ] = chunk.split("\n");

    let processedFileInfo = extractFileInfo(fileInfo);

    if (processedFileInfo != null) {
      // reanalyze prints the severity first in the title, and then the rest of
      // the title after.
      let [severityRaw, ...issueTitleParts] = title.split(" ");
      let issueTitle = issueTitleParts.join(" ");

      let [startCharacter, endCharacter] =
        processedFileInfo.characters.split("-");

      let startPos = new Position(
        // reanalyze reports lines as index 1 based, while VSCode wants them
        // index 0 based.
        parseInt(processedFileInfo.line, 10) - 1,
        parseInt(startCharacter, 10)
      );

      // This isn't correct, and will only highlight a single line, even if the
      // highlight should in fact span multiple lines. This is because reanalyze
      // gives us a line start, and a character interval. But, VSCode wants a
      // line for the end of the selection too. And, to figure that out, we'd
      // need to read the entire file with the issues present and calculate it.
      // So, postponing that for now. Maybe there's a better way.
      let endPos = new Position(
        parseInt(processedFileInfo.line, 10) - 1,
        parseInt(endCharacter, 10)
      );

      let issueLocationRange = new Range(startPos, endPos);

      let severity =
        severityRaw === "Error"
          ? DiagnosticSeverity.Error
          : DiagnosticSeverity.Warning;

      let diagnostic = new Diagnostic(
        issueLocationRange,
        `${issueTitle}: ${text}`,
        severity
      );

      if (diagnosticsMap.has(processedFileInfo.filePath)) {
        diagnosticsMap.get(processedFileInfo.filePath).push(diagnostic);
      } else {
        diagnosticsMap.set(processedFileInfo.filePath, [diagnostic]);
      }

      // Let's see if there's a valid code action that can be produced from this diagnostic.
      if (lineNumToReplace != null && lineContentToReplace != null) {
        let actualLineToReplaceStr = lineNumToReplace.split("<-- line ").pop();

        if (actualLineToReplaceStr != null) {
          let codeAction = new CodeAction(`Annotate with @dead`);
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
  diagnosticsCollection: DiagnosticCollection,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap
) => {
  // Clear old diagnostics and code actions state.
  diagnosticsCollection.clear();
  diagnosticsResultCodeActions.clear();

  let currentDocument = window.activeTextEditor.document;

  let p = cp.spawn("npx", ["reanalyze", "-dce"], {
    // Pointing reanalyze to the dir of the current file path is fine,
    // because reanalyze will walk upwards looking for a bsconfig.json in
    // order to find the correct project root.
    cwd: path.dirname(currentDocument.uri.fsPath),
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
