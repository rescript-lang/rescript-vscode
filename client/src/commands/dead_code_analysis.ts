import * as fs from "fs";
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
} from "vscode";
import { DocumentUri } from "vscode-languageclient";

let findProjectRootOfFile = (source: DocumentUri): null | DocumentUri => {
  let dir = path.dirname(source);
  if (fs.existsSync(path.join(dir, "bsconfig.json"))) {
    return dir;
  } else {
    if (dir === source) {
      // reached top
      return null;
    } else {
      return findProjectRootOfFile(dir);
    }
  }
};

let fileInfoRegex = /File "(.+)", line (\d)+, characters ([\d-]+)/;

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
  dceText: string
): {
  diagnosticsMap: Map<string, Diagnostic[]>;
  hasMoreDiagnostics: boolean;
  totalDiagnosticsCount: number;
  savedDiagnostics: number;
} => {
  let diagnosticsMap: Map<string, Diagnostic[]> = new Map();
  let savedDiagnostics = 0;
  let totalDiagnosticsCount = 0;

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
      _lineNumToReplace,
      _lineContentToReplace,

      ..._rest
    ] = chunk.split("\n");

    let processedFileInfo = extractFileInfo(fileInfo);

    if (processedFileInfo != null) {
      // We'll limit the amount of diagnostics to display somewhat. This is also
      // in part because we don't "watch" for changes with reanalyze, so the
      // user will need to re-run the command fairly often anyway as issues are
      // fixed, in order to get rid of the stale problems reported.
      if (savedDiagnostics > 20) {
        totalDiagnosticsCount++;
        return;
      }

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

      let severity =
        severityRaw === "Error"
          ? DiagnosticSeverity.Error
          : DiagnosticSeverity.Warning;

      let diagnostic = new Diagnostic(
        new Range(startPos, endPos),
        `${issueTitle}: ${text}`,
        severity
      );

      savedDiagnostics++;

      if (diagnosticsMap.has(processedFileInfo.filePath)) {
        diagnosticsMap.get(processedFileInfo.filePath).push(diagnostic);
      } else {
        diagnosticsMap.set(processedFileInfo.filePath, [diagnostic]);
      }
    }
  });

  return {
    diagnosticsMap,
    hasMoreDiagnostics: totalDiagnosticsCount > savedDiagnostics,
    savedDiagnostics,
    totalDiagnosticsCount,
  };
};

export const runDeadCodeAnalysisWithReanalyze = (
  diagnosticsCollection: DiagnosticCollection
) => {
  diagnosticsCollection.clear();
  let currentDocument = window.activeTextEditor.document;
  let projectRootOfFile = findProjectRootOfFile(currentDocument.uri.fsPath);

  if (!projectRootOfFile) {
    window.showWarningMessage(
      "Could not determine project root of current file."
    );
    return;
  }

  const p = cp.spawn("npx", ["reanalyze", "-dce"], {
    cwd: projectRootOfFile,
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
        `Something went wrong trying to run reanalyze. Please try cleaning and rebuilding your ReScript project, and then run this command again.`
      );
    } else {
      window.showErrorMessage(
        `Something went wrong trying to run reanalyze: '${e}'`
      );
    }
  });

  p.on("close", () => {
    let {
      diagnosticsMap,
      hasMoreDiagnostics,
      savedDiagnostics,
      totalDiagnosticsCount,
    } = dceTextToDiagnostics(data);

    diagnosticsMap.forEach((diagnostics, filePath) => {
      diagnosticsCollection.set(Uri.parse(filePath), diagnostics);
    });

    if (hasMoreDiagnostics) {
      window.showInformationMessage(
        `Showing ${savedDiagnostics} of in total ${totalDiagnosticsCount} issues found. Re-run this command again as you fix issues.`
      );
    }
  });
};
