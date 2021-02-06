import * as c from "./constants";
import * as childProcess from "child_process";
import * as p from "vscode-languageserver-protocol";
import * as path from "path";
import * as t from "vscode-languageserver-types";
import fs from "fs";
import * as os from "os";

let tempFilePrefix = "rescript_format_file_" + process.pid + "_";
let tempFileId = 0;

export let createFileInTempDir = (extension = "") => {
  let tempFileName = tempFilePrefix + tempFileId + extension;
  tempFileId = tempFileId + 1;
  return path.join(os.tmpdir(), tempFileName);
};

// TODO: races here?
// TODO: this doesn't handle file:/// scheme
export let findProjectRootOfFile = (
  source: p.DocumentUri
): null | p.DocumentUri => {
  let dir = path.dirname(source);
  if (fs.existsSync(path.join(dir, c.bsconfigPartialPath))) {
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

// TODO: races here?
// TODO: this doesn't handle file:/// scheme

// We need to recursively search for bs-platform/{platform}/bsc.exe upward from
// the project's root, because in some setups, such as yarn workspace/monorepo,
// the node_modules/bs-platform package might be hoisted up instead of alongside
// the project root.
// Also, if someone's ever formatting a regular project setup's dependency
// (which is weird but whatever), they'll at least find an upward bs-platform
// from the dependent.
export let findBscExeDirOfFile = (
  source: p.DocumentUri
): null | p.DocumentUri => {
  let dir = path.dirname(source);
  let bscPath = path.join(dir, c.bscExePartialPath);
  if (fs.existsSync(bscPath)) {
    return dir;
  } else {
    if (dir === source) {
      // reached the top
      return null;
    } else {
      return findBscExeDirOfFile(dir);
    }
  }
};

type execResult =
  | {
    kind: "success";
    result: string;
  }
  | {
    kind: "error";
    error: string;
  };
export let formatUsingValidBscPath = (
  code: string,
  bscPath: p.DocumentUri,
  isInterface: boolean
): execResult => {
  let extension = isInterface ? c.resiExt : c.resExt;
  let formatTempFileFullPath = createFileInTempDir(extension);
  fs.writeFileSync(formatTempFileFullPath, code, {
    encoding: "utf-8",
  });
  try {
    let result = childProcess.execFileSync(
      bscPath,
      ["-color", "never", "-format", formatTempFileFullPath],
    );
    return {
      kind: "success",
      result: result.toString(),
    };
  } catch (e) {
    return {
      kind: "error",
      error: e.message,
    };
  } finally {
    // async close is fine. We don't use this file name again
    fs.unlink(formatTempFileFullPath, () => null);
  }
};

export let runBsbWatcherUsingValidBsbPath = (
  bsbPath: p.DocumentUri,
  projectRootPath: p.DocumentUri
) => {
  if (process.platform === "win32") {
    return childProcess.exec(`${bsbPath}.cmd -w`, {
      cwd: projectRootPath,
    });
  } else {
    return childProcess.execFile(bsbPath, ["-w"], {
      cwd: projectRootPath,
    });
  }
};

// Logic for parsing .compiler.log
/* example .compiler.log content:

#Start(1600519680823)

  Syntax error!
  /Users/chenglou/github/reason-react/src/test.res:1:8-2:3

  1 │ let a =
  2 │ let b =
  3 │

  This let-binding misses an expression


  Warning number 8
  /Users/chenglou/github/reason-react/src/test.res:3:5-8

  1 │ let a = j`😀`
  2 │ let b = `😀`
  3 │ let None = None
  4 │ let bla: int = "
  5 │   hi

  You forgot to handle a possible case here, for example:
  Some _


  We've found a bug for you!
  /Users/chenglou/github/reason-react/src/test.res:3:9

  1 │ let a = 1
  2 │ let b = "hi"
  3 │ let a = b + 1

  This has type: string
  Somewhere wanted: int

#Done(1600519680836)
*/

// parser helpers
let normalizeFileForWindows = (file: string) => {
  return process.platform === "win32" ? `file:\\\\\\${file}` : file;
};
let parseFileAndRange = (fileAndRange: string) => {
  // https://github.com/rescript-lang/rescript-compiler/blob/0a3f4bb32ca81e89cefd5a912b8795878836f883/jscomp/super_errors/super_location.ml#L15-L25
  /* The file + location format can be:
    a/b.res <- fallback, no location available (usually due to bad ppx...)
    a/b.res:10:20
    a/b.res:10:20-21     <- last number here is the end char of line 10
    a/b.res:10:20-30:11
  */
  let regex = /(.+)\:(\d+)\:(\d+)(-(\d+)(\:(\d+))?)?$/;
  /*            ^^ file
                      ^^^ start line
                             ^^^ start character
                                  ^ optional range
                                    ^^^ end line or chararacter
                                            ^^^ end character
  */
  // for the trimming, see https://github.com/rescript-lang/rescript-vscode/pull/71#issuecomment-769160576
  let trimmedFileAndRange = fileAndRange.trim();
  let match = trimmedFileAndRange.match(regex);
  if (match === null) {
    // no location! Though LSP insist that we provide at least a dummy location
    return {
      file: normalizeFileForWindows(trimmedFileAndRange),
      range: {
        start: { line: 0, character: 0 },
        end: { line: 0, character: 0 },
      },
    };
  }

  let [
    _source,
    file,
    startLine,
    startChar,
    optionalEndGroup,
    endLineOrChar,
    _colonPlusEndCharOrNothing,
    endCharOrNothing,
  ] = match;

  // language-server position is 0-based. Ours is 1-based. Convert
  // also, our end character is inclusive. Language-server's is exclusive
  let range;
  if (optionalEndGroup == null) {
    let start = {
      line: parseInt(startLine) - 1,
      character: parseInt(startChar),
    };
    range = {
      start: start,
      end: start,
    };
  } else {
    let isSingleLine = endCharOrNothing == null;
    let [endLine, endChar] = isSingleLine
      ? [startLine, endLineOrChar]
      : [endLineOrChar, endCharOrNothing];
    range = {
      start: {
        line: parseInt(startLine) - 1,
        character: parseInt(startChar) - 1,
      },
      end: { line: parseInt(endLine) - 1, character: parseInt(endChar) },
    };
  }
  return {
    file: normalizeFileForWindows(file),
    range,
  };
};

// main parsing logic
type filesDiagnostics = {
  [key: string]: p.Diagnostic[];
};
type parsedCompilerLogResult = {
  done: boolean;
  result: filesDiagnostics;
};
export let parseCompilerLogOutput = (
  content: string
): parsedCompilerLogResult => {
  type parsedDiagnostic = {
    code: number | undefined;
    severity: t.DiagnosticSeverity;
    tag: t.DiagnosticTag | undefined;
    content: string[];
  };
  let parsedDiagnostics: parsedDiagnostic[] = [];
  let lines = content.split(os.EOL);
  let done = false;

  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];
    if (line.startsWith("  We've found a bug for you!")) {
      parsedDiagnostics.push({
        code: undefined,
        severity: t.DiagnosticSeverity.Error,
        tag: undefined,
        content: [],
      });
    } else if (line.startsWith("  Warning number ")) {
      let warningNumber = parseInt(line.slice("  Warning number ".length));
      let tag: t.DiagnosticTag | undefined = undefined;
      switch (warningNumber) {
        case 11:
        case 20:
        case 26:
        case 27:
        case 32:
        case 33:
        case 34:
        case 35:
        case 36:
        case 37:
        case 38:
        case 39:
        case 60:
        case 66:
        case 67:
        case 101:
          tag = t.DiagnosticTag.Unnecessary;
          break;
        case 3:
          tag = t.DiagnosticTag.Deprecated;
          break;
      }
      parsedDiagnostics.push({
        code: Number.isNaN(warningNumber) ? undefined : warningNumber,
        severity: t.DiagnosticSeverity.Warning,
        tag: tag,
        content: [],
      });
    } else if (line.startsWith("  Syntax error!")) {
      parsedDiagnostics.push({
        code: undefined,
        severity: t.DiagnosticSeverity.Error,
        tag: undefined,
        content: [],
      });
    } else if (line.startsWith("#Done(")) {
      done = true;
    } else if (/^  +[0-9]+ /.test(line)) {
      // code display. Swallow
    } else if (line.startsWith("  ")) {
      parsedDiagnostics[parsedDiagnostics.length - 1].content.push(line);
    }
  }

  let result: filesDiagnostics = {};
  parsedDiagnostics.forEach((parsedDiagnostic) => {
    let [fileAndRangeLine, ...diagnosticMessage] = parsedDiagnostic.content;
    let { file, range } = parseFileAndRange(fileAndRangeLine);

    if (result[file] == null) {
      result[file] = [];
    }
    let cleanedUpDiagnostic =
      diagnosticMessage
        .map((line) => {
          // remove the spaces in front
          return line.slice(2);
        })
        .join("\n")
        // remove start and end whitespaces/newlines
        .trim() + "\n";
    result[file].push({
      severity: parsedDiagnostic.severity,
      tags: parsedDiagnostic.tag === undefined ? [] : [parsedDiagnostic.tag],
      code: parsedDiagnostic.code,
      range,
      source: "ReScript",
      message: cleanedUpDiagnostic,
    });
  });

  return { done, result };
};
