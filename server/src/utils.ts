import * as childProcess from "child_process";
import * as p from "vscode-languageserver-protocol";
import * as path from "path";
import * as t from "vscode-languageserver-types";
import {
  RequestMessage,
  ResponseMessage,
} from "vscode-languageserver-protocol";
import fs from "fs";
import fsAsync from "fs/promises";
import * as os from "os";
import semver from "semver";

import * as codeActions from "./codeActions";
import * as c from "./constants";
import * as lookup from "./lookup";
import { reportError } from "./errorReporter";
import config from "./config";
import { filesDiagnostics, projectsFiles } from "./projectFiles";

let tempFilePrefix = "rescript_format_file_" + process.pid + "_";
let tempFileId = 0;

export let createFileInTempDir = (extension = "") => {
  let tempFileName = tempFilePrefix + tempFileId + extension;
  tempFileId = tempFileId + 1;
  return path.join(os.tmpdir(), tempFileName);
};

let findProjectRootOfFileInDir = (
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
      return findProjectRootOfFileInDir(dir);
    }
  }
};

// TODO: races here?
// TODO: this doesn't handle file:/// scheme
export let findProjectRootOfFile = (
  source: p.DocumentUri,
  allowDir?: boolean
): null | p.DocumentUri => {
  // First look in project files
  let foundRootFromProjectFiles: string | null = null;
  for (const rootPath of projectsFiles.keys()) {
    if (source.startsWith(rootPath) && (!allowDir || source !== rootPath)) {
      // Prefer the longest path (most nested)
      if (
        foundRootFromProjectFiles == null ||
        rootPath.length > foundRootFromProjectFiles.length
      ) {
        foundRootFromProjectFiles = rootPath;
      }
    }
  }

  if (foundRootFromProjectFiles != null) {
    return foundRootFromProjectFiles;
  } else {
    const isDir = path.extname(source) === "";
    return findProjectRootOfFileInDir(
      isDir && !allowDir ? path.join(source, "dummy.res") : source
    );
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

let findPlatformPath = (projectRootPath: p.DocumentUri | null) => {
  if (config.extensionConfiguration.platformPath != null) {
    return config.extensionConfiguration.platformPath;
  }

  let rescriptDir = lookup.findFilePathFromProjectRoot(
    projectRootPath,
    path.join("node_modules", "rescript")
  );
  if (rescriptDir == null) {
    return null;
  }

  let platformPath = path.join(rescriptDir, c.platformDir);

  // Binaries have been split into optional platform-specific dependencies
  // since v12.0.0-alpha.13
  if (!fs.existsSync(platformPath)) {
    platformPath = path.join(
      rescriptDir,
      "..",
      `@rescript/${process.platform}-${process.arch}/bin`
    )
  }

  // Workaround for darwinarm64 which has no folder yet in ReScript <= 9.1.4
  if (
    process.platform == "darwin" &&
    process.arch == "arm64" &&
    !fs.existsSync(platformPath)
  ) {
    platformPath = path.join(rescriptDir, process.platform);
  }

  return platformPath;
};

// If ReScript < 12.0.0-alpha.13, then we want `{project_root}/node_modules/rescript/{c.platformDir}/{binary}`.
// Otherwise, we want to dynamically import `{project_root}/node_modules/rescript` and from `binPaths` get the relevant binary.
// We won't know which version is in the project root until we read and parse `{project_root}/node_modules/rescript/package.json`
let findBinaryAsync = async (
  projectRootPath: p.DocumentUri | null,
  binary: "bsc.exe" | "rescript-editor-analysis.exe"
) => {
  if (config.extensionConfiguration.platformPath != null) {
    return path.join(config.extensionConfiguration.platformPath, binary);
  }

  const rescriptDir = lookup.findFilePathFromProjectRoot(
    projectRootPath,
    path.join("node_modules", "rescript")
  );
  if (rescriptDir == null) {
    return null;
  }

  let rescriptVersion = null;
  try {
    const rescriptPackageJSONPath = path.join(rescriptDir, "package.json");
    const rescriptPackageJSON = JSON.parse(await fsAsync.readFile(rescriptPackageJSONPath, "utf-8"));
    rescriptVersion = rescriptPackageJSON.version
  } catch (error) {
    return null
  }

  let binaryPath: string | null = null
  if (semver.gte(rescriptVersion, "12.0.0-alpha.13")) {
    // TODO: export `binPaths` from `rescript` package so that we don't need to
    // copy the logic for figuring out `target`.
    const target = `${process.platform}-${process.arch}`;
    const targetPackagePath = path.join(rescriptDir, "..", `@rescript/${target}`)
    const { binPaths } = await import(targetPackagePath);

    if (binary == "bsc.exe") {
      binaryPath = binPaths.bsc_exe
    } else if (binary == "rescript-editor-analysis.exe") {
      binaryPath = binPaths.rescript_editor_analysis_exe
    }
  } else {
    binaryPath = path.join(rescriptDir, c.platformDir, binary)
  }

  if (binaryPath != null && fs.existsSync(binaryPath)) {
    return binaryPath
  } else {
    return null
  }
}

export let findBscExeBinary = (projectRootPath: p.DocumentUri | null) =>
  findBinaryAsync(projectRootPath, "bsc.exe");

export let findEditorAnalysisBinary = (projectRootPath: p.DocumentUri | null) =>
  findBinaryAsync(projectRootPath, "rescript-editor-analysis.exe");

type execResult =
  | {
      kind: "success";
      result: string;
    }
  | {
      kind: "error";
      error: string;
    };

type formatCodeResult = execResult;

export let formatCode = (
  bscPath: p.DocumentUri | null,
  filePath: string,
  code: string
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
      throw new Error("Could not find ReScript compiler for project.");
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

export let findReScriptVersion = (
  filePath: p.DocumentUri
): string | undefined => {
  let projectRoot = findProjectRootOfFile(filePath);
  if (projectRoot == null) {
    return undefined;
  }

  const bscExe = findBinary(findPlatformPath(projectRoot), c.bscExeName);

  if (bscExe == null) {
    return undefined;
  }

  try {
    let version = childProcess.execSync(`${bscExe} -v`);
    return version.toString().replace(/rescript/gi, "").trim();
  } catch (e) {
    console.error("rescrip binary failed", e);
    return undefined;
  }
};

export function findReScriptVersionForProjectRoot(projectRootPath:string) : string | undefined {
  const bscExe = findBinary(findPlatformPath(projectRootPath), c.bscExeName);

  if (bscExe == null) {
    return undefined;
  }

  try {
    let version = childProcess.execSync(`${bscExe} -v`);
    return version.toString().replace(/rescript/gi, "").trim();
  } catch (e) {
    return undefined;
  }
}

// This is the path for the _builtin_ legacy analysis, that works for versions 11 and below.
let builtinBinaryPath: string | null = null;
if (fs.existsSync(c.builtinAnalysisDevPath)) {
  builtinBinaryPath = c.builtinAnalysisDevPath;
} else if (fs.existsSync(c.builtinAnalysisProdPath)) {
  builtinBinaryPath = c.builtinAnalysisProdPath;
}

export let runAnalysisAfterSanityCheck = (
  filePath: p.DocumentUri,
  args: Array<any>,
  projectRequired = false
) => {
  let projectRootPath = findProjectRootOfFile(filePath);
  if (projectRootPath == null && projectRequired) {
    return null;
  }
  let rescriptVersion =
    projectsFiles.get(projectRootPath ?? "")?.rescriptVersion ??
    findReScriptVersion(filePath);

  let binaryPath = builtinBinaryPath;

  let project = projectRootPath ? projectsFiles.get(projectRootPath) : null;

  /**
   * All versions including 12.0.0-alpha.5 and above should use the analysis binary
   * that now ships with the compiler. Previous versions use the legacy one we ship
   * with the extension itself.
   */
  let shouldUseBuiltinAnalysis =
    rescriptVersion?.startsWith("9.") ||
    rescriptVersion?.startsWith("10.") ||
    rescriptVersion?.startsWith("11.") ||
    [
      "12.0.0-alpha.1",
      "12.0.0-alpha.2",
      "12.0.0-alpha.3",
      "12.0.0-alpha.4",
    ].includes(rescriptVersion ?? "");

  if (!shouldUseBuiltinAnalysis && project != null) {
    binaryPath = project.editorAnalysisLocation;
  } else if (!shouldUseBuiltinAnalysis && project == null) {
    // TODO: Warn user about broken state?
    return null;
  } else {
    binaryPath = builtinBinaryPath;
  }

  let options: childProcess.ExecFileSyncOptions = {
    cwd: projectRootPath || undefined,
    maxBuffer: Infinity,
    env: {
      ...process.env,
      RESCRIPT_VERSION: rescriptVersion,
      RESCRIPT_INCREMENTAL_TYPECHECKING:
        config.extensionConfiguration.incrementalTypechecking?.enable === true
          ? "true"
          : undefined,
      RESCRIPT_PROJECT_CONFIG_CACHE:
        config.extensionConfiguration.cache?.projectConfig?.enable === true
          ? "true"
          : undefined,
    },
  };

  if (binaryPath == null) {
    return null;
  }

  let stdout = "";
  try {
    stdout = childProcess.execFileSync(binaryPath, args, options).toString();
    return JSON.parse(stdout);
  } catch (e) {
    console.error(e);
    console.error("Original response: ", stdout);
    console.error("Args: ", args);
    // Element 0 is the action we're performing
    reportError(String(args[0]), String(e));
    return null;
  }
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

export let getReferencesForPosition = (
  filePath: p.DocumentUri,
  position: p.Position
) =>
  runAnalysisAfterSanityCheck(filePath, [
    "references",
    filePath,
    position.line,
    position.character,
  ]);

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
