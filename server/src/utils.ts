import * as childProcess from "child_process";
import * as p from "vscode-languageserver-protocol";
import * as path from "path";
import * as t from "vscode-languageserver-types";
import {
  RequestMessage,
  ResponseMessage,
} from "vscode-languageserver-protocol";
import fs from "fs";
import * as os from "os";

import * as codeActions from "./codeActions";
import * as c from "./constants";
import * as lookup from "./lookup";

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
  if (
    fs.existsSync(path.join(dir, c.rescriptJsonPartialPath)) ||
    fs.existsSync(path.join(dir, c.bsconfigPartialPath))
  ) {
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

// Check if binaryName exists inside binaryDirPath and return the joined path.
export let findBinary = (
  binaryDirPath: p.DocumentUri | null,
  binaryName: string
): p.DocumentUri | null => {
  if (binaryDirPath == null) {
    return null;
  }
  let binaryPath: p.DocumentUri = path.join(binaryDirPath, binaryName);
  if (fs.existsSync(binaryPath)) {
    return binaryPath;
  } else {
    return null;
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

type formatCodeResult =
  | execResult
  | {
      kind: "blocked-using-built-in-formatter";
    };

export let formatCode = (
  bscPath: p.DocumentUri | null,
  filePath: string,
  code: string,
  allowBuiltInFormatter: boolean
): formatCodeResult => {
  let extension = path.extname(filePath);
  let formatTempFileFullPath = createFileInTempDir(extension);
  fs.writeFileSync(formatTempFileFullPath, code, {
    encoding: "utf-8",
  });
  try {
    // It will try to use the user formatting binary.
    // If not, use the one we ship with the analysis binary in the extension itself.
    if (bscPath != null) {
      let result = childProcess.execFileSync(bscPath, [
        "-color",
        "never",
        "-format",
        formatTempFileFullPath,
      ]);
      return {
        kind: "success",
        result: result.toString(),
      };
    } else {
      if (!allowBuiltInFormatter) {
        return {
          kind: "blocked-using-built-in-formatter",
        };
      }

      let result = runAnalysisAfterSanityCheck(
        formatTempFileFullPath,
        ["format", formatTempFileFullPath],
        false
      );

      // The formatter returning an empty string means it couldn't format the
      // sources, probably because of errors. In that case, we bail from
      // formatting by returning the unformatted content.
      if (result === "") {
        result = code;
      }

      return {
        kind: "success",
        result,
      };
    }
  } catch (e) {
    return {
      kind: "error",
      error: e instanceof Error ? e.message : String(e),
    };
  } finally {
    // async close is fine. We don't use this file name again
    fs.unlink(formatTempFileFullPath, () => null);
  }
};

export let runAnalysisAfterSanityCheck = (
  filePath: p.DocumentUri,
  args: Array<any>,
  projectRequired = false
) => {
  let binaryPath;
  if (fs.existsSync(c.analysisDevPath)) {
    binaryPath = c.analysisDevPath;
  } else if (fs.existsSync(c.analysisProdPath)) {
    binaryPath = c.analysisProdPath;
  } else {
    return null;
  }

  let projectRootPath = findProjectRootOfFile(filePath);
  if (projectRootPath == null && projectRequired) {
    return null;
  }
  let options: childProcess.ExecFileSyncOptions = {
    cwd: projectRootPath || undefined,
    maxBuffer: Infinity,
  };
  let stdout = childProcess.execFileSync(binaryPath, args, options);

  return JSON.parse(stdout.toString());
};

export let runAnalysisCommand = (
  filePath: p.DocumentUri,
  args: Array<any>,
  msg: RequestMessage,
  projectRequired = true
) => {
  let result = runAnalysisAfterSanityCheck(filePath, args, projectRequired);
  let response: ResponseMessage = {
    jsonrpc: c.jsonrpcVersion,
    id: msg.id,
    result,
  };
  return response;
};

export const toCamelCase = (text: string): string => {
  return text
    .replace(/(?:^\w|[A-Z]|\b\w)/g, (s: string) => s.toUpperCase())
    .replace(/(\s|-)+/g, "");
};

export const getNamespaceNameFromConfigFile = (
  projDir: p.DocumentUri
): execResult => {
  let config = lookup.readConfig(projDir);
  let result = "";

  if (!config) {
    return {
      kind: "error",
      error: "Could not read ReScript config file",
    };
  }

  if (config.namespace === true) {
    result = toCamelCase(config.name);
  } else if (typeof config.namespace === "string") {
    result = toCamelCase(config.namespace);
  }

  return {
    kind: "success",
    result,
  };
};

export let getCompiledFilePath = (
  filePath: string,
  projDir: string
): execResult => {
  let error: execResult = {
    kind: "error",
    error: "Could not read ReScript config file",
  };
  let partialFilePath = filePath.split(projDir)[1];
  let compiledPath = lookup.getFilenameFromBsconfig(projDir, partialFilePath);

  if (!compiledPath) {
    return error;
  }

  let result = compiledPath;

  // If the file is not found, lookup a possible root bsconfig that may contain
  // info about the possible location of the file.
  if (!fs.existsSync(result)) {
    let compiledPath = lookup.getFilenameFromRootBsconfig(
      projDir,
      partialFilePath
    );

    if (!compiledPath) {
      return error;
    }

    result = compiledPath;
  }

  return {
    kind: "success",
    result,
  };
};

export let runBuildWatcherUsingValidBuildPath = (
  buildPath: p.DocumentUri,
  projectRootPath: p.DocumentUri
) => {
  let cwdEnv = {
    cwd: projectRootPath,
  };
  if (process.platform === "win32") {
    /*
      - a node.js script in node_modules/.bin on windows is wrapped in a
        batch script wrapper (there's also a regular binary of the same name on
        windows, but that one's a shell script wrapper for cygwin). More info:
        https://github.com/npm/cmd-shim/blob/c5118da34126e6639361fe9706a5ff07e726ed45/index.js#L1
      - a batch script adds the suffix .cmd to the script
      - you can't call batch scripts through the regular `execFile`:
        https://nodejs.org/api/child_process.html#child_process_spawning_bat_and_cmd_files_on_windows
      - So you have to use `exec` instead, and make sure you quote the path
        (since the path might have spaces), which `execFile` would have done
        for you under the hood
    */
    return childProcess.exec(`"${buildPath}".cmd build -w`, cwdEnv);
  } else {
    return childProcess.execFile(buildPath, ["build", "-w"], cwdEnv);
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
export let pathToURI = (file: string) => {
  return process.platform === "win32" ? `file:\\\\\\${file}` : `file://${file}`;
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
      file: pathToURI(trimmedFileAndRange),
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
    file: pathToURI(file),
    range,
  };
};

// main parsing logic
export type filesDiagnostics = {
  [key: string]: p.Diagnostic[];
};
type parsedCompilerLogResult = {
  done: boolean;
  result: filesDiagnostics;
  codeActions: codeActions.filesCodeActions;
  linesWithParseErrors: string[];
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
  let linesWithParseErrors: string[] = [];
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
    } else if (line.startsWith("FAILED:")) {
      // File with a self cycle
      parsedDiagnostics.push({
        code: undefined,
        severity: t.DiagnosticSeverity.Error,
        tag: undefined,
        content: [line],
      });
    } else if (line.startsWith("Fatal error:")) {
      parsedDiagnostics.push({
        code: undefined,
        severity: t.DiagnosticSeverity.Error,
        tag: undefined,
        content: [line],
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
      let severity = line.includes("(configured as error)")
        ? t.DiagnosticSeverity.Error
        : t.DiagnosticSeverity.Warning;
      parsedDiagnostics.push({
        code: Number.isNaN(warningNumber) ? undefined : warningNumber,
        severity,
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
    } else if (line.startsWith("  Warning genType")) {
      parsedDiagnostics.push({
        code: undefined,
        severity: t.DiagnosticSeverity.Error,
        tag: undefined,
        content: [],
      });
    } else if (line.startsWith("#Start(")) {
      // do nothing for now
    } else if (line.startsWith("#Done(")) {
      done = true;
    } else if (
      line.startsWith("File ") &&
      i + 1 < lines.length &&
      lines[i + 1].startsWith("Warning ")
    ) {
      // OCaml warning: skip
      i++;
    } else if (
      line.startsWith("File ") &&
      i + 1 < lines.length &&
      lines[i + 1].startsWith("Error: Syntax error")
    ) {
      // OCaml Syntax Error
      parsedDiagnostics.push({
        code: undefined,
        severity: t.DiagnosticSeverity.Error,
        tag: undefined,
        content: [lines[i], lines[i + 1]],
      });
      i++;
    } else if (/^  +([0-9]+| +|\.) (│|┆)/.test(line)) {
      //         ^^ indent
      //           ^^^^^^^^^^^^^^^ gutter
      //                           ^^^^^   separator
      // swallow code display. Examples:
      //   10 │
      //    . │
      //      │
      //   10 ┆
    } else if (line.startsWith("  ")) {
      // part of the actual diagnostics message
      if (parsedDiagnostics[parsedDiagnostics.length - 1] == null) {
        linesWithParseErrors.push(line);
      } else {
        parsedDiagnostics[parsedDiagnostics.length - 1].content.push(
          line.slice(2)
        );
      }
    } else if (line.trim() != "") {
      // We'll assume that everything else is also part of the diagnostics too.
      // Most of these should have been indented 2 spaces; sadly, some of them
      // aren't (e.g. outcome printer printing badly, and certain old ocaml type
      // messages not printing with indent). We used to get bug reports and fix
      // the messages, but that strategy turned out too slow. One day we should
      // revert to not having this branch...
      if (parsedDiagnostics[parsedDiagnostics.length - 1] == null) {
        linesWithParseErrors.push(line);
      } else {
        parsedDiagnostics[parsedDiagnostics.length - 1].content.push(line);
      }
    }
  }

  let result: filesDiagnostics = {};
  let foundCodeActions: codeActions.filesCodeActions = {};

  parsedDiagnostics.forEach((parsedDiagnostic) => {
    let [fileAndRangeLine, ...diagnosticMessage] = parsedDiagnostic.content;
    let { file, range } = parseFileAndRange(fileAndRangeLine);

    if (result[file] == null) {
      result[file] = [];
    }

    let diagnostic: p.Diagnostic = {
      severity: parsedDiagnostic.severity,
      tags: parsedDiagnostic.tag === undefined ? [] : [parsedDiagnostic.tag],
      code: parsedDiagnostic.code,
      range,
      source: "ReScript",
      // remove start and end whitespaces/newlines
      message: diagnosticMessage.join("\n").trim(),
    };

    // Check for potential code actions
    codeActions.findCodeActionsInDiagnosticsMessage({
      addFoundActionsHere: foundCodeActions,
      diagnostic,
      diagnosticMessage,
      file,
      range,
    });

    result[file].push(diagnostic);
  });

  return {
    done,
    result,
    codeActions: foundCodeActions,
    linesWithParseErrors,
  };
};

export let rangeContainsRange = (
  range: p.Range,
  otherRange: p.Range
): boolean => {
  if (
    otherRange.start.line < range.start.line ||
    otherRange.end.line < range.start.line
  ) {
    return false;
  }
  if (
    otherRange.start.line > range.end.line ||
    otherRange.end.line > range.end.line
  ) {
    return false;
  }
  if (
    otherRange.start.line === range.start.line &&
    otherRange.start.character < range.start.character
  ) {
    return false;
  }
  if (
    otherRange.end.line === range.end.line &&
    otherRange.end.character > range.end.character
  ) {
    return false;
  }
  return true;
};
