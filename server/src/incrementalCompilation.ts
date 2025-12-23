import * as path from "path";
import fs from "fs";
import * as utils from "./utils";
import { performance } from "perf_hooks";
import * as p from "vscode-languageserver-protocol";
import * as cp from "node:child_process";
import { promisify } from "node:util";
import semver from "semver";
import * as os from "os";
import config, { send } from "./config";
import * as c from "./constants";
import { fileCodeActions } from "./codeActions";
import { projectsFiles } from "./projectFiles";
import { getRewatchBscArgs, RewatchCompilerArgs } from "./bsc-args/rewatch";
import { BsbCompilerArgs, getBsbBscArgs } from "./bsc-args/bsb";
import { getCurrentCompilerDiagnosticsForFile } from "./server";
import { NormalizedPath } from "./utils";
import { getLogger } from "./logger";

const execFilePromise = promisify(cp.execFile);

const INCREMENTAL_FOLDER_NAME = "___incremental";
const INCREMENTAL_FILE_FOLDER_LOCATION = path.join(
  c.compilerDirPartialPath,
  INCREMENTAL_FOLDER_NAME,
);

export type IncrementallyCompiledFileInfo = {
  file: {
    /** File type. */
    extension: ".res" | ".resi";
    /** Path to the source file (normalized). */
    sourceFilePath: NormalizedPath;
    /** Name of the source file. */
    sourceFileName: string;
    /** Module name of the source file. */
    moduleName: string;
    /** Namespaced module name of the source file. */
    moduleNameNamespaced: string;
    /** Path to where the incremental file is saved. */
    incrementalFilePath: NormalizedPath;
    /** Location of the original type file. */
    originalTypeFileLocation: NormalizedPath;
  };
  buildSystem: "bsb" | "rewatch";
  /** Cache for build.ninja assets. */
  buildNinja: {
    /** When build.ninja was last modified. Used as a cache key. */
    fileMtime: number;
    /** The raw, extracted needed info from build.ninja. Needs processing. */
    rawExtracted: BsbCompilerArgs;
  } | null;
  /** Cache for rewatch compiler args. */
  buildRewatch: {
    lastFile: NormalizedPath;
    compilerArgs: RewatchCompilerArgs;
  } | null;
  /** Info of the currently active incremental compilation. `null` if no incremental compilation is active. */
  compilation: {
    /** The timeout of the currently active compilation for this incremental file. */
    timeout: NodeJS.Timeout;
    /** The trigger token for the currently active compilation. */
    triggerToken: number;
  } | null;
  /** Mechanism to kill the currently active compilation. */
  abortCompilation: (() => void) | null;
  /** Project specific information. */
  project: {
    /** The root path of the project (normalized to match projectsFiles keys). */
    rootPath: NormalizedPath;
    /** The root path of the workspace (if a monorepo) */
    workspaceRootPath: NormalizedPath;
    /** Computed location of bsc. */
    bscBinaryLocation: NormalizedPath;
    /** The arguments needed for bsc, derived from the project configuration/build.ninja. */
    callArgs: Promise<Array<string> | null>;
    /** The location of the incremental folder for this project. */
    incrementalFolderPath: NormalizedPath;
  };
  /** Any code actions for this incremental file. */
  codeActions: Array<fileCodeActions>;
};

const incrementallyCompiledFileInfo: Map<
  NormalizedPath,
  IncrementallyCompiledFileInfo
> = new Map();
const hasReportedFeatureFailedError: Set<NormalizedPath> = new Set();
const originalTypeFileToFilePath: Map<NormalizedPath, NormalizedPath> =
  new Map();

/**
 * Cancels the currently active compilation for an entry.
 * Clears the timeout, aborts the compilation, and resets state.
 */
function cancelActiveCompilation(entry: IncrementallyCompiledFileInfo): void {
  if (entry.compilation != null) {
    clearTimeout(entry.compilation.timeout);
    entry.abortCompilation?.();
    entry.compilation = null;
    entry.abortCompilation = null;
  }
}

export function incrementalCompilationFileChanged(changedPath: NormalizedPath) {
  const filePath = originalTypeFileToFilePath.get(changedPath);
  if (filePath != null) {
    const entry = incrementallyCompiledFileInfo.get(filePath);
    if (entry != null) {
      getLogger().log(
        "[watcher] Cleaning up incremental files for " + filePath,
      );
      if (entry.compilation != null) {
        getLogger().log("[watcher] Was compiling, killing");
        cancelActiveCompilation(entry);
      }
      cleanUpIncrementalFiles(
        entry.file.sourceFilePath,
        entry.project.rootPath,
      );
    }
  }
}

export function removeIncrementalFileFolder(
  projectRootPath: NormalizedPath,
  onAfterRemove?: () => void,
) {
  fs.rm(
    path.resolve(projectRootPath, INCREMENTAL_FILE_FOLDER_LOCATION),
    { force: true, recursive: true },
    (_) => {
      onAfterRemove?.();
    },
  );
}

export function recreateIncrementalFileFolder(projectRootPath: NormalizedPath) {
  getLogger().log("Recreating incremental file folder");
  removeIncrementalFileFolder(projectRootPath, () => {
    fs.mkdir(
      path.resolve(projectRootPath, INCREMENTAL_FILE_FOLDER_LOCATION),
      { recursive: true },
      (_) => {},
    );
  });
}

export function cleanUpIncrementalFiles(
  filePath: NormalizedPath,
  projectRootPath: NormalizedPath,
) {
  const ext = filePath.endsWith(".resi") ? ".resi" : ".res";
  const namespace = utils.getNamespaceNameFromConfigFile(projectRootPath);
  const fileNameNoExt = path.basename(filePath, ext);
  const moduleNameNamespaced =
    namespace.kind === "success" && namespace.result !== ""
      ? `${fileNameNoExt}-${namespace.result}`
      : fileNameNoExt;

  getLogger().log("Cleaning up incremental file assets for: " + fileNameNoExt);

  fs.unlink(
    path.resolve(
      projectRootPath,
      INCREMENTAL_FILE_FOLDER_LOCATION,
      path.basename(filePath),
    ),
    (_) => {},
  );

  [
    moduleNameNamespaced + ".ast",
    moduleNameNamespaced + ".cmt",
    moduleNameNamespaced + ".cmti",
    moduleNameNamespaced + ".cmi",
    moduleNameNamespaced + ".cmj",
  ].forEach((file) => {
    fs.unlink(
      path.resolve(projectRootPath, INCREMENTAL_FILE_FOLDER_LOCATION, file),
      (_) => {},
    );
  });
}

export async function getBscArgs(
  send: (msg: p.Message) => void,
  entry: IncrementallyCompiledFileInfo,
): Promise<BsbCompilerArgs | RewatchCompilerArgs | null> {
  return entry.buildSystem === "bsb"
    ? await getBsbBscArgs(entry)
    : await getRewatchBscArgs(
        send,
        entry.project.bscBinaryLocation,
        projectsFiles,
        entry,
      );
}

function argCouples(argList: string[]): string[][] {
  let args: string[][] = [];
  for (let i = 0; i <= argList.length - 1; i++) {
    const item = argList[i];
    const nextIndex = i + 1;
    const nextItem = argList[nextIndex] ?? "";
    if (item.startsWith("-") && nextItem.startsWith("-")) {
      // Single entry arg
      args.push([item]);
    } else if (item.startsWith("-") && nextItem.startsWith("'")) {
      // Quoted arg, take until ending '
      const arg = [nextItem.slice(1)];
      for (let x = nextIndex + 1; x <= argList.length - 1; x++) {
        let subItem = argList[x];
        let break_ = false;
        if (subItem.endsWith("'")) {
          subItem = subItem.slice(0, subItem.length - 1);
          i = x;
          break_ = true;
        }
        arg.push(subItem);
        if (break_) {
          break;
        }
      }
      args.push([item, arg.join(" ")]);
    } else if (item.startsWith("-")) {
      args.push([item, nextItem]);
    }
  }
  return args;
}

function argsFromCommandString(cmdString: string): Array<Array<string>> {
  const argList = cmdString
    .trim()
    .split("command = ")[1]
    .split(" ")
    .map((v) => v.trim())
    .filter((v) => v !== "");

  return argCouples(argList);
}
function removeAnsiCodes(s: string): string {
  const ansiEscape = /\x1B[@-_][0-?]*[ -/]*[@-~]/g;
  return s.replace(ansiEscape, "");
}
function triggerIncrementalCompilationOfFile(
  filePath: NormalizedPath,
  fileContent: string,
  send: send,
  onCompilationFinished?: () => void,
) {
  let incrementalFileCacheEntry = incrementallyCompiledFileInfo.get(filePath);
  if (incrementalFileCacheEntry == null) {
    // New file
    const projectRootPath = utils.findProjectRootOfFile(filePath);
    if (projectRootPath == null) {
      getLogger().log("Did not find project root path for " + filePath);
      return;
    }
    // projectRootPath is already normalized (NormalizedPath) from findProjectRootOfFile
    // Use getProjectFile to verify the project exists
    const project = utils.getProjectFile(projectRootPath);
    if (project == null) {
      getLogger().log("Did not find open project for " + filePath);
      return;
    }

    // computeWorkspaceRootPathFromLockfile returns null if lockfile found (local package) or if no parent found
    const computedWorkspaceRoot =
      utils.computeWorkspaceRootPathFromLockfile(projectRootPath);
    // If null, it means either a lockfile was found (local package) or no parent project root exists
    // In both cases, we default to projectRootPath
    const workspaceRootPath: NormalizedPath =
      computedWorkspaceRoot ?? projectRootPath;

    // Determine if lockfile was found for debug logging
    // If computedWorkspaceRoot is null and projectRootPath is not null, check if parent exists
    const foundRewatchLockfileInProjectRoot =
      computedWorkspaceRoot == null &&
      projectRootPath != null &&
      utils.findProjectRootOfDir(projectRootPath) != null;

    if (foundRewatchLockfileInProjectRoot) {
      getLogger().log(
        `Found rewatch/rescript lockfile in project root, treating as local package in workspace`,
      );
    } else {
      getLogger().log(
        `Did not find rewatch/rescript lockfile in project root, assuming bsb`,
      );
    }

    const bscBinaryLocation = project.bscBinaryLocation;
    if (bscBinaryLocation == null) {
      getLogger().log("Could not find bsc binary location for " + filePath);
      return;
    }
    const ext = filePath.endsWith(".resi") ? ".resi" : ".res";
    const moduleName = path.basename(filePath, ext);
    const moduleNameNamespaced =
      project.namespaceName != null
        ? `${moduleName}-${project.namespaceName}`
        : moduleName;

    // projectRootPath is already NormalizedPath, appending a constant string still makes it a NormalizedPath
    const incrementalFolderPath: NormalizedPath = path.join(
      projectRootPath,
      INCREMENTAL_FILE_FOLDER_LOCATION,
    ) as NormalizedPath;

    const relSourcePath = path.relative(projectRootPath, filePath);
    const relSourceDir = path.dirname(relSourcePath);
    const typeExt = ext === ".res" ? ".cmt" : ".cmti";
    const originalTypeFileName =
      project.namespaceName != null
        ? `${moduleName}-${project.namespaceName}${typeExt}`
        : `${moduleName}${typeExt}`;
    // projectRootPath is already NormalizedPath, appending constant strings still yields a NormalizedPath
    const originalTypeFileLocation = path.resolve(
      projectRootPath,
      c.compilerDirPartialPath,
      relSourceDir,
      originalTypeFileName,
    ) as NormalizedPath;

    incrementalFileCacheEntry = {
      file: {
        originalTypeFileLocation,
        extension: ext,
        moduleName,
        moduleNameNamespaced,
        sourceFileName: moduleName + ext,
        sourceFilePath: filePath,
        // As incrementalFolderPath was a NormalizedPath, path.join ensures we can assume string is now NormalizedPath
        incrementalFilePath: path.join(
          incrementalFolderPath,
          moduleName + ext,
        ) as NormalizedPath,
      },
      project: {
        workspaceRootPath,
        rootPath: projectRootPath,
        callArgs: Promise.resolve([]),
        bscBinaryLocation,
        incrementalFolderPath,
      },
      buildSystem: foundRewatchLockfileInProjectRoot ? "rewatch" : "bsb",
      buildRewatch: null,
      buildNinja: null,
      compilation: null,
      abortCompilation: null,
      codeActions: [],
    };

    incrementalFileCacheEntry.project.callArgs = figureOutBscArgs(
      send,
      incrementalFileCacheEntry,
    );
    originalTypeFileToFilePath.set(
      incrementalFileCacheEntry.file.originalTypeFileLocation,
      incrementalFileCacheEntry.file.sourceFilePath,
    );
    incrementallyCompiledFileInfo.set(filePath, incrementalFileCacheEntry);
  }

  if (incrementalFileCacheEntry == null) return;
  const entry = incrementalFileCacheEntry;
  cancelActiveCompilation(entry);
  const triggerToken = performance.now();
  const timeout = setTimeout(() => {
    compileContents(entry, fileContent, send, onCompilationFinished);
  }, 20);

  entry.compilation = {
    timeout,
    triggerToken,
  };
}
function verifyTriggerToken(
  filePath: NormalizedPath,
  triggerToken: number,
): boolean {
  return (
    incrementallyCompiledFileInfo.get(filePath)?.compilation?.triggerToken ===
    triggerToken
  );
}

const isWindows = os.platform() === "win32";

async function figureOutBscArgs(
  send: (msg: p.Message) => void,
  entry: IncrementallyCompiledFileInfo,
) {
  const project = projectsFiles.get(entry.project.rootPath);
  if (project?.rescriptVersion == null) {
    getLogger().log(
      "Found no project (or ReScript version) for " + entry.file.sourceFilePath,
    );
    return null;
  }
  const res = await getBscArgs(send, entry);
  if (res == null) return null;
  let astArgs: Array<Array<string>> = [];
  let buildArgs: Array<Array<string>> = [];
  let isBsb = Array.isArray(res);
  if (Array.isArray(res)) {
    const [astBuildCommand, fullBuildCommand] = res;
    astArgs = argsFromCommandString(astBuildCommand);
    buildArgs = argsFromCommandString(fullBuildCommand);
  } else {
    astArgs = argCouples(res.parser_args);
    buildArgs = argCouples(res.compiler_args);
  }
  let callArgs: Array<string> = [];

  if (config.extensionConfiguration.incrementalTypechecking?.acrossFiles) {
    callArgs.push(
      "-I",
      path.resolve(entry.project.rootPath, INCREMENTAL_FILE_FOLDER_LOCATION),
    );
  }

  buildArgs.forEach(([key, value]: Array<string>) => {
    if (key === "-I") {
      if (isBsb) {
        // On Windows, the value could be wrapped in quotes.
        value =
          value.startsWith('"') && value.endsWith('"')
            ? value.substring(1, value.length - 1)
            : value;
        /*build.ninja could have quoted full paths
        Example:
rule mij
  command = "C:\Users\moi\Projects\my-project\node_modules\rescript\win32\bsc.exe" -I src -I "C:\Users\moi\Projects\my-project\node_modules\@rescript\core\lib\ocaml" -open RescriptCore  -uncurried -bs-package-name rewindow -bs-package-output esmodule:$in_d:.res.mjs -bs-v $g_finger $i
        */
        if (isWindows && value.includes(":\\")) {
          callArgs.push("-I", value);
        } else {
          callArgs.push(
            "-I",
            path.resolve(
              entry.project.rootPath,
              c.compilerDirPartialPath,
              value,
            ),
          );
        }
      } else {
        // TODO: once ReScript v12 is out we can remove this check for `.`
        if (value === ".") {
          callArgs.push(
            "-I",
            path.resolve(entry.project.rootPath, c.compilerOcamlDirPartialPath),
          );
        } else {
          callArgs.push("-I", value);
        }
      }
    } else if (key === "-bs-v") {
      callArgs.push("-bs-v", Date.now().toString());
    } else if (key === "-bs-package-output") {
      return;
    } else if (value == null || value === "") {
      callArgs.push(key);
    } else {
      callArgs.push(key, value);
    }
  });

  astArgs.forEach(([key, value]: Array<string>) => {
    if (key.startsWith("-bs-jsx")) {
      callArgs.push(key, value);
    } else if (key.startsWith("-ppx")) {
      callArgs.push(key, value);
    }
  });

  callArgs.push("-color", "never");
  // Only available in v11+
  if (
    semver.valid(project.rescriptVersion) &&
    semver.satisfies(project.rescriptVersion as string, ">=11", {
      includePrerelease: true,
    })
  ) {
    callArgs.push("-ignore-parse-errors");
  }

  callArgs = callArgs.filter((v) => v != null && v !== "");
  callArgs.push(entry.file.incrementalFilePath);
  return callArgs;
}

/**
 * Remaps code action file paths from the incremental temp file to the actual source file.
 */
function remapCodeActionsToSourceFile(
  codeActions: Record<string, fileCodeActions[]>,
  sourceFilePath: NormalizedPath,
): fileCodeActions[] {
  const actions = Object.values(codeActions)[0] ?? [];

  // Code actions will point to the locally saved incremental file, so we must remap
  // them so the editor understands it's supposed to apply them to the unsaved doc,
  // not the saved "dummy" incremental file.
  actions.forEach((ca) => {
    if (ca.codeAction.edit != null && ca.codeAction.edit.changes != null) {
      const change = Object.values(ca.codeAction.edit.changes)[0];

      ca.codeAction.edit.changes = {
        [utils.pathToURI(sourceFilePath)]: change,
      };
    }
  });

  return actions;
}

/**
 * Filters diagnostics to remove unwanted parser errors from incremental compilation.
 */
function filterIncrementalDiagnostics(
  diagnostics: p.Diagnostic[],
  sourceFileName: string,
): { filtered: p.Diagnostic[]; hasIgnoredMessages: boolean } {
  let hasIgnoredMessages = false;

  const filtered = diagnostics
    .map((d) => ({
      ...d,
      message: removeAnsiCodes(d.message),
    }))
    // Filter out a few unwanted parser errors since we run the parser in ignore mode
    .filter((d) => {
      if (
        !d.message.startsWith("Uninterpreted extension 'rescript.") &&
        (!d.message.includes(`/${INCREMENTAL_FOLDER_NAME}/${sourceFileName}`) ||
          // The `Multiple definition of the <kind> name <name>` type error's
          // message includes the filepath with LOC of the duplicate definition
          d.message.startsWith("Multiple definition of the") ||
          // The signature mismatch, with mismatch and ill typed applicative functor
          // type errors all include the filepath with LOC
          d.message.startsWith("Signature mismatch") ||
          d.message.startsWith("In this `with' constraint") ||
          d.message.startsWith("This `with' constraint on"))
      ) {
        hasIgnoredMessages = true;
        return true;
      }
      return false;
    });

  return { filtered, hasIgnoredMessages };
}

/**
 * Logs an error when incremental compilation produces unexpected output.
 */
function logIncrementalCompilationError(
  entry: IncrementallyCompiledFileInfo,
  stderr: string,
  callArgs: string[] | null,
  send: (msg: p.Message) => void,
): void {
  hasReportedFeatureFailedError.add(entry.project.rootPath);
  const logfile = path.resolve(
    entry.project.incrementalFolderPath,
    "error.log",
  );

  try {
    fs.writeFileSync(
      logfile,
      `== BSC ARGS ==\n${callArgs?.join(" ")}\n\n== OUTPUT ==\n${stderr}`,
    );

    const params: p.ShowMessageParams = {
      type: p.MessageType.Warning,
      message: `[Incremental typechecking] Something might have gone wrong with incremental type checking. Check out the [error log](file://${logfile}) and report this issue please.`,
    };

    const message: p.NotificationMessage = {
      jsonrpc: c.jsonrpcVersion,
      method: "window/showMessage",
      params: params,
    };

    send(message);
  } catch (e) {
    console.error(e);
  }
}

/**
 * Processes compilation results and publishes diagnostics to the LSP client.
 */
function processAndPublishDiagnostics(
  entry: IncrementallyCompiledFileInfo,
  result: Record<string, p.Diagnostic[]>,
  codeActions: Record<string, fileCodeActions[]>,
  stderr: string,
  callArgs: string[] | null,
  send: (msg: p.Message) => void,
): void {
  // Remap code actions to source file
  const actions = remapCodeActionsToSourceFile(
    codeActions,
    entry.file.sourceFilePath,
  );
  entry.codeActions = actions;

  // Filter diagnostics
  const rawDiagnostics = Object.values(result)[0] ?? [];
  const { filtered: res, hasIgnoredMessages } = filterIncrementalDiagnostics(
    rawDiagnostics,
    entry.file.sourceFileName,
  );

  // Log error if compilation produced unexpected output
  if (
    res.length === 0 &&
    stderr !== "" &&
    !hasIgnoredMessages &&
    !hasReportedFeatureFailedError.has(entry.project.rootPath)
  ) {
    logIncrementalCompilationError(entry, stderr, callArgs, send);
  }

  const fileUri = utils.pathToURI(entry.file.sourceFilePath);

  // Update filesWithDiagnostics to track this file
  // entry.project.rootPath is guaranteed to match a key in projectsFiles
  // (see triggerIncrementalCompilationOfFile where the entry is created)
  const projectFile = projectsFiles.get(entry.project.rootPath);

  if (projectFile != null) {
    if (res.length > 0) {
      projectFile.filesWithDiagnostics.add(fileUri);
    } else {
      // Only remove if there are no diagnostics at all
      projectFile.filesWithDiagnostics.delete(fileUri);
    }
  }

  const notification: p.NotificationMessage = {
    jsonrpc: c.jsonrpcVersion,
    method: "textDocument/publishDiagnostics",
    params: {
      uri: fileUri,
      diagnostics: res,
    },
  };
  send(notification);
}

async function compileContents(
  entry: IncrementallyCompiledFileInfo,
  fileContent: string,
  send: (msg: p.Message) => void,
  onCompilationFinished?: () => void,
) {
  const triggerToken = entry.compilation?.triggerToken;
  let callArgs = await entry.project.callArgs;
  if (callArgs == null) {
    const callArgsRetried = await figureOutBscArgs(send, entry);
    if (callArgsRetried != null) {
      callArgs = callArgsRetried;
      entry.project.callArgs = Promise.resolve(callArgsRetried);
    } else {
      getLogger().log(
        "Could not figure out call args. Maybe build.ninja does not exist yet?",
      );
      return;
    }
  }

  const startTime = performance.now();
  if (!fs.existsSync(entry.project.incrementalFolderPath)) {
    try {
      fs.mkdirSync(entry.project.incrementalFolderPath, { recursive: true });
    } catch {}
  }

  try {
    fs.writeFileSync(entry.file.incrementalFilePath, fileContent);
    let cwd =
      entry.buildSystem === "bsb"
        ? entry.project.rootPath
        : path.resolve(entry.project.rootPath, c.compilerDirPartialPath);

    getLogger().log(
      `About to invoke bsc from \"${cwd}\", used ${entry.buildSystem}`,
    );
    getLogger().log(
      `${entry.project.bscBinaryLocation} ${callArgs.map((c) => `"${c}"`).join(" ")}`,
    );

    // Create AbortController for this compilation
    const abortController = new AbortController();
    const { signal } = abortController;

    // Store abort function directly on the entry
    entry.abortCompilation = () => {
      getLogger().log(`Aborting compilation of ${entry.file.sourceFileName}`);
      abortController.abort();
    };

    try {
      const { stderr } = await execFilePromise(
        entry.project.bscBinaryLocation,
        callArgs,
        { cwd, signal },
      );

      getLogger().log(
        `Recompiled ${entry.file.sourceFileName} in ${
          (performance.now() - startTime) / 1000
        }s`,
      );

      // Verify token after async operation
      if (
        triggerToken != null &&
        !verifyTriggerToken(entry.file.sourceFilePath, triggerToken)
      ) {
        getLogger().log(
          `Discarding stale compilation results for ${entry.file.sourceFileName} (token changed)`,
        );
        return;
      }

      const { result, codeActions } = await utils.parseCompilerLogOutput(
        `${stderr}\n#Done()`,
      );

      // Re-verify again after second async operation
      if (
        triggerToken != null &&
        !verifyTriggerToken(entry.file.sourceFilePath, triggerToken)
      ) {
        getLogger().log(
          `Discarding stale compilation results for ${entry.file.sourceFileName} (token changed after parsing)`,
        );
        return;
      }

      processAndPublishDiagnostics(
        entry,
        result,
        codeActions,
        stderr,
        callArgs,
        send,
      );
    } catch (error: any) {
      if (error.name === "AbortError") {
        getLogger().log(
          `Compilation of ${entry.file.sourceFileName} was aborted.`,
        );
      } else {
        getLogger().error(
          `Unexpected error during compilation of ${entry.file.sourceFileName}: ${error}`,
        );
        throw error;
      }
    } finally {
      // Only clean up if this is still the active compilation
      if (entry.compilation?.triggerToken === triggerToken) {
        getLogger().log("Cleaning up compilation status.");
        entry.compilation = null;
        entry.abortCompilation = null;
      }
    }
  } finally {
    onCompilationFinished?.();
  }
}

export function handleUpdateOpenedFile(
  filePath: utils.NormalizedPath,
  fileContent: string,
  send: send,
  onCompilationFinished?: () => void,
) {
  getLogger().log("Updated: " + filePath);
  triggerIncrementalCompilationOfFile(
    filePath,
    fileContent,
    send,
    onCompilationFinished,
  );
}

export function handleClosedFile(filePath: NormalizedPath) {
  getLogger().log("Closed: " + filePath);
  const entry = incrementallyCompiledFileInfo.get(filePath);
  if (entry == null) return;
  cleanUpIncrementalFiles(filePath, entry.project.rootPath);
  incrementallyCompiledFileInfo.delete(filePath);
  originalTypeFileToFilePath.delete(entry.file.originalTypeFileLocation);
}

export function getCodeActionsFromIncrementalCompilation(
  filePath: NormalizedPath,
): Array<fileCodeActions> | null {
  const entry = incrementallyCompiledFileInfo.get(filePath);
  if (entry != null) {
    return entry.codeActions;
  }

  return null;
}
