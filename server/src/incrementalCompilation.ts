import * as path from "path";
import fs from "fs";
import * as utils from "./utils";
import { pathToFileURL } from "url";
import readline from "readline";
import { performance } from "perf_hooks";
import * as p from "vscode-languageserver-protocol";
import * as cp from "node:child_process";
import config, { send } from "./config";
import * as c from "./constants";
import * as chokidar from "chokidar";
import { fileCodeActions } from "./codeActions";

function debug() {
  return (
    config.extensionConfiguration.incrementalTypechecking?.debugLogging ?? false
  );
}

const INCREMENTAL_FOLDER_NAME = "___incremental";
const INCREMENTAL_FILE_FOLDER_LOCATION = `lib/bs/${INCREMENTAL_FOLDER_NAME}`;

type IncrementallyCompiledFileInfo = {
  file: {
    /** File type. */
    extension: ".res" | ".resi";
    /** Path to the source file. */
    sourceFilePath: string;
    /** Name of the source file. */
    sourceFileName: string;
    /** Module name of the source file. */
    moduleName: string;
    /** Namespaced module name of the source file. */
    moduleNameNamespaced: string;
    /** Path to where the incremental file is saved. */
    incrementalFilePath: string;
    /** Location of the original type file. */
    originalTypeFileLocation: string;
  };
  /** Cache for build.ninja assets. */
  buildNinja: {
    /** When build.ninja was last modified. Used as a cache key. */
    fileMtime: number;
    /** The raw, extracted needed info from build.ninja. Needs processing. */
    rawExtracted: Array<string>;
  } | null;
  /** Info of the currently active incremental compilation. `null` if no incremental compilation is active. */
  compilation: {
    /** The timeout of the currently active compilation for this incremental file. */
    timeout: NodeJS.Timeout;
    /** The trigger token for the currently active compilation. */
    triggerToken: number;
  } | null;
  /** Listeners for when compilation of this file is killed. List always cleared after each invocation. */
  killCompilationListeners: Array<() => void>;
  /** Project specific information. */
  project: {
    /** The root path of the project. */
    rootPath: string;
    /** Computed location of bsc. */
    bscBinaryLocation: string;
    /** The arguments needed for bsc, derived from the project configuration/build.ninja. */
    callArgs: Promise<Array<string> | null>;
    /** The location of the incremental folder for this project. */
    incrementalFolderPath: string;
    /** The ReScript version. */
    rescriptVersion: string;
  };
  /** Any code actions for this incremental file. */
  codeActions: Array<fileCodeActions>;
};

const incrementallyCompiledFileInfo: Map<
  string,
  IncrementallyCompiledFileInfo
> = new Map();
const hasReportedFeatureFailedError: Set<string> = new Set();
const originalTypeFileToFilePath: Map<string, string> = new Map();

let incrementalFilesWatcher = chokidar
  .watch([], {
    awaitWriteFinish: {
      stabilityThreshold: 1,
    },
  })
  .on("all", (e, changedPath) => {
    if (e !== "change" && e !== "unlink") return;
    const filePath = originalTypeFileToFilePath.get(changedPath);
    if (filePath != null) {
      const entry = incrementallyCompiledFileInfo.get(filePath);
      if (entry != null) {
        if (debug()) {
          console.log(
            "[watcher] Cleaning up incremental files for " + filePath
          );
        }
        if (entry.compilation != null) {
          if (debug()) {
            console.log("[watcher] Was compiling, killing");
          }
          clearTimeout(entry.compilation.timeout);
          entry.killCompilationListeners.forEach((cb) => cb());
          entry.compilation = null;
        }
        cleanUpIncrementalFiles(
          entry.file.sourceFilePath,
          entry.project.rootPath
        );
      }
    }
  });

export function removeIncrementalFileFolder(
  projectRootPath: string,
  onAfterRemove?: () => void
) {
  fs.rm(
    path.resolve(projectRootPath, INCREMENTAL_FILE_FOLDER_LOCATION),
    { force: true, recursive: true },
    (_) => {
      onAfterRemove?.();
    }
  );
}

export function recreateIncrementalFileFolder(projectRootPath: string) {
  if (debug()) {
    console.log("Recreating incremental file folder");
  }
  removeIncrementalFileFolder(projectRootPath, () => {
    fs.mkdir(
      path.resolve(projectRootPath, INCREMENTAL_FILE_FOLDER_LOCATION),
      { recursive: true },
      (_) => {}
    );
  });
}

export function cleanUpIncrementalFiles(
  filePath: string,
  projectRootPath: string
) {
  const ext = filePath.endsWith(".resi") ? ".resi" : ".res";
  const namespace = utils.getNamespaceNameFromConfigFile(projectRootPath);
  const fileNameNoExt = path.basename(filePath, ext);
  const moduleNameNamespaced =
    namespace.kind === "success" && namespace.result !== ""
      ? `${fileNameNoExt}-${namespace.result}`
      : fileNameNoExt;

  if (debug()) {
    console.log("Cleaning up incremental file assets for: " + fileNameNoExt);
  }

  fs.unlink(
    path.resolve(
      projectRootPath,
      INCREMENTAL_FILE_FOLDER_LOCATION,
      path.basename(filePath)
    ),
    (_) => {}
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
      (_) => {}
    );
  });
}
function getBscArgs(
  entry: IncrementallyCompiledFileInfo
): Promise<Array<string> | null> {
  const buildNinjaPath = path.resolve(
    entry.project.rootPath,
    "lib/bs/build.ninja"
  );
  let stat: fs.Stats | null = null;
  try {
    stat = fs.statSync(buildNinjaPath);
  } catch {
    if (debug()) {
      console.log("Did not find build.ninja, cannot proceed..");
    }
    return Promise.resolve(null);
  }
  const cacheEntry = entry.buildNinja;
  if (
    cacheEntry != null &&
    stat != null &&
    cacheEntry.fileMtime >= stat.mtimeMs
  ) {
    return Promise.resolve(cacheEntry.rawExtracted);
  }
  return new Promise((resolve, _reject) => {
    function resolveResult(result: Array<string>) {
      if (stat != null) {
        entry.buildNinja = {
          fileMtime: stat.mtimeMs,
          rawExtracted: result,
        };
      }
      resolve(result);
    }
    const fileStream = fs.createReadStream(buildNinjaPath);
    const rl = readline.createInterface({
      input: fileStream,
      crlfDelay: Infinity,
    });
    let captureNextLine = false;
    let done = false;
    let stopped = false;
    const captured: Array<string> = [];
    rl.on("line", (line) => {
      if (stopped) {
        return;
      }
      if (captureNextLine) {
        captured.push(line);
        captureNextLine = false;
      }
      if (done) {
        fileStream.destroy();
        rl.close();
        resolveResult(captured);
        stopped = true;
        return;
      }
      if (line.startsWith("rule astj")) {
        captureNextLine = true;
      }
      if (line.startsWith("rule mij")) {
        captureNextLine = true;
        done = true;
      }
    });
    rl.on("close", () => {
      resolveResult(captured);
    });
  });
}
function argsFromCommandString(cmdString: string): Array<Array<string>> {
  const s = cmdString
    .trim()
    .split("command = ")[1]
    .split(" ")
    .map((v) => v.trim())
    .filter((v) => v !== "");
  const args: Array<Array<string>> = [];

  for (let i = 0; i <= s.length - 1; i++) {
    const item = s[i];
    const nextIndex = i + 1;
    const nextItem = s[nextIndex] ?? "";
    if (item.startsWith("-") && nextItem.startsWith("-")) {
      // Single entry arg
      args.push([item]);
    } else if (item.startsWith("-") && nextItem.startsWith("'")) {
      // Quoted arg, take until ending '
      const arg = [nextItem.slice(1)];
      for (let x = nextIndex + 1; x <= s.length - 1; x++) {
        let subItem = s[x];
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
function removeAnsiCodes(s: string): string {
  const ansiEscape = /\x1B[@-_][0-?]*[ -/]*[@-~]/g;
  return s.replace(ansiEscape, "");
}
function triggerIncrementalCompilationOfFile(
  filePath: string,
  fileContent: string,
  send: send,
  onCompilationFinished?: () => void
) {
  let incrementalFileCacheEntry = incrementallyCompiledFileInfo.get(filePath);
  if (incrementalFileCacheEntry == null) {
    // New file
    const projectRootPath = utils.findProjectRootOfFile(filePath);
    if (projectRootPath == null) {
      if (debug())
        console.log("Did not find project root path for " + filePath);
      return;
    }
    const namespaceName = utils.getNamespaceNameFromConfigFile(projectRootPath);
    if (namespaceName.kind === "error") {
      if (debug())
        console.log("Getting namespace config errored for " + filePath);
      return;
    }
    const bscBinaryLocation = utils.findBscExeBinary(projectRootPath);
    if (bscBinaryLocation == null) {
      if (debug())
        console.log("Could not find bsc binary location for " + filePath);
      return;
    }
    const ext = filePath.endsWith(".resi") ? ".resi" : ".res";
    const moduleName = path.basename(filePath, ext);
    const moduleNameNamespaced =
      namespaceName.result !== ""
        ? `${moduleName}-${namespaceName.result}`
        : moduleName;

    const incrementalFolderPath = path.join(
      projectRootPath,
      INCREMENTAL_FILE_FOLDER_LOCATION
    );

    let rescriptVersion = "";
    try {
      rescriptVersion = cp
        .execFileSync(bscBinaryLocation, ["-version"])
        .toString()
        .trim();
    } catch (e) {
      console.error(e);
    }
    if (rescriptVersion.startsWith("ReScript ")) {
      rescriptVersion = rescriptVersion.replace("ReScript ", "");
    }

    let originalTypeFileLocation = path.resolve(
      projectRootPath,
      "lib/bs",
      path.relative(projectRootPath, filePath)
    );

    const parsed = path.parse(originalTypeFileLocation);
    parsed.ext = ext === ".res" ? ".cmt" : ".cmti";
    parsed.base = "";
    originalTypeFileLocation = path.format(parsed);

    incrementalFileCacheEntry = {
      file: {
        originalTypeFileLocation,
        extension: ext,
        moduleName,
        moduleNameNamespaced,
        sourceFileName: moduleName + ext,
        sourceFilePath: filePath,
        incrementalFilePath: path.join(incrementalFolderPath, moduleName + ext),
      },
      project: {
        rootPath: projectRootPath,
        callArgs: Promise.resolve([]),
        bscBinaryLocation,
        incrementalFolderPath,
        rescriptVersion,
      },
      buildNinja: null,
      compilation: null,
      killCompilationListeners: [],
      codeActions: [],
    };

    incrementalFileCacheEntry.project.callArgs = figureOutBscArgs(
      incrementalFileCacheEntry
    );
    // Set up watcher for relevant cmt/cmti
    incrementalFilesWatcher.add([
      incrementalFileCacheEntry.file.originalTypeFileLocation,
    ]);
    originalTypeFileToFilePath.set(
      incrementalFileCacheEntry.file.originalTypeFileLocation,
      incrementalFileCacheEntry.file.sourceFilePath
    );
    incrementallyCompiledFileInfo.set(filePath, incrementalFileCacheEntry);
  }

  if (incrementalFileCacheEntry == null) return;
  const entry = incrementalFileCacheEntry;
  if (entry.compilation != null) {
    clearTimeout(entry.compilation.timeout);
    entry.killCompilationListeners.forEach((cb) => cb());
    entry.killCompilationListeners = [];
  }
  const triggerToken = performance.now();
  const timeout = setTimeout(() => {
    compileContents(entry, fileContent, send, onCompilationFinished);
  }, 20);

  if (entry.compilation != null) {
    entry.compilation.timeout = timeout;
    entry.compilation.triggerToken = triggerToken;
  } else {
    entry.compilation = {
      timeout,
      triggerToken,
    };
  }
}
function verifyTriggerToken(filePath: string, triggerToken: number): boolean {
  return (
    incrementallyCompiledFileInfo.get(filePath)?.compilation?.triggerToken ===
    triggerToken
  );
}
async function figureOutBscArgs(entry: IncrementallyCompiledFileInfo) {
  const res = await getBscArgs(entry);
  if (res == null) return null;
  const [astBuildCommand, fullBuildCommand] = res;

  const astArgs = argsFromCommandString(astBuildCommand);
  const buildArgs = argsFromCommandString(fullBuildCommand);

  let callArgs: Array<string> = [];

  if (config.extensionConfiguration.incrementalTypechecking?.acrossFiles) {
    callArgs.push(
      "-I",
      path.resolve(entry.project.rootPath, INCREMENTAL_FILE_FOLDER_LOCATION)
    );
  }

  buildArgs.forEach(([key, value]: Array<string>) => {
    if (key === "-I") {
      callArgs.push(
        "-I",
        path.resolve(entry.project.rootPath, "lib/bs", value)
      );
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
  if (parseInt(entry.project.rescriptVersion.split(".")[0] ?? "10") >= 11) {
    // Only available in v11+
    callArgs.push("-ignore-parse-errors");
  }

  callArgs = callArgs.filter((v) => v != null && v !== "");
  callArgs.push(entry.file.incrementalFilePath);
  return callArgs;
}
async function compileContents(
  entry: IncrementallyCompiledFileInfo,
  fileContent: string,
  send: (msg: p.Message) => void,
  onCompilationFinished?: () => void
) {
  const triggerToken = entry.compilation?.triggerToken;
  let callArgs = await entry.project.callArgs;
  if (callArgs == null) {
    const callArgsRetried = await figureOutBscArgs(entry);
    if (callArgsRetried != null) {
      callArgs = callArgsRetried;
      entry.project.callArgs = Promise.resolve(callArgsRetried);
    } else {
      if (debug()) {
        console.log(
          "Could not figure out call args. Maybe build.ninja does not exist yet?"
        );
      }
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

    const process = cp.execFile(
      entry.project.bscBinaryLocation,
      callArgs,
      { cwd: entry.project.rootPath },
      (error, _stdout, stderr) => {
        if (!error?.killed) {
          if (debug())
            console.log(
              `Recompiled ${entry.file.sourceFileName} in ${
                (performance.now() - startTime) / 1000
              }s`
            );
        } else {
          if (debug())
            console.log(
              `Compilation of ${entry.file.sourceFileName} was killed.`
            );
        }
        let hasIgnoredErrorMessages = false;
        if (
          !error?.killed &&
          triggerToken != null &&
          verifyTriggerToken(entry.file.sourceFilePath, triggerToken)
        ) {
          if (debug()) {
            console.log("Resetting compilation status.");
          }
          // Reset compilation status as this compilation finished
          entry.compilation = null;
          const { result, codeActions } = utils.parseCompilerLogOutput(
            `${stderr}\n#Done()`
          );

          const actions = Object.values(codeActions)[0] ?? [];

          // Code actions will point to the locally saved incremental file, so we must remap
          // them so the editor understand it's supposed to apply them to the unsaved doc,
          // not the saved "dummy" incremental file.
          actions.forEach((ca) => {
            if (
              ca.codeAction.edit != null &&
              ca.codeAction.edit.changes != null
            ) {
              const change = Object.values(ca.codeAction.edit.changes)[0];

              ca.codeAction.edit.changes = {
                [pathToFileURL(entry.file.sourceFilePath).toString()]: change,
              };
            }
          });

          entry.codeActions = actions;

          const res = (Object.values(result)[0] ?? [])
            .map((d) => ({
              ...d,
              message: removeAnsiCodes(d.message),
            }))
            // Filter out a few unwanted parser errors since we run the parser in ignore mode
            .filter((d) => {
              if (
                !d.message.startsWith("Uninterpreted extension 'rescript.") &&
                !d.message.includes(
                  `/${INCREMENTAL_FOLDER_NAME}/${entry.file.sourceFileName}`
                )
              ) {
                hasIgnoredErrorMessages = true;
                return true;
              }
              return false;
            });

          if (
            res.length === 0 &&
            stderr !== "" &&
            !hasIgnoredErrorMessages &&
            !hasReportedFeatureFailedError.has(entry.project.rootPath)
          ) {
            try {
              hasReportedFeatureFailedError.add(entry.project.rootPath);
              const logfile = path.resolve(
                entry.project.incrementalFolderPath,
                "error.log"
              );
              fs.writeFileSync(
                logfile,
                `== BSC ARGS ==\n${callArgs?.join(
                  " "
                )}\n\n== OUTPUT ==\n${stderr}`
              );
              let params: p.ShowMessageParams = {
                type: p.MessageType.Warning,
                message: `[Incremental typechecking] Something might have gone wrong with incremental type checking. Check out the [error log](file://${logfile}) and report this issue please.`,
              };
              let message: p.NotificationMessage = {
                jsonrpc: c.jsonrpcVersion,
                method: "window/showMessage",
                params: params,
              };
              send(message);
            } catch (e) {
              console.error(e);
            }
          }

          const notification: p.NotificationMessage = {
            jsonrpc: c.jsonrpcVersion,
            method: "textDocument/publishDiagnostics",
            params: {
              uri: pathToFileURL(entry.file.sourceFilePath),
              diagnostics: res,
            },
          };
          send(notification);
        }
        onCompilationFinished?.();
      }
    );
    entry.killCompilationListeners.push(() => {
      process.kill("SIGKILL");
    });
  } catch (e) {
    console.error(e);
  }
}

export function handleUpdateOpenedFile(
  filePath: string,
  fileContent: string,
  send: send,
  onCompilationFinished?: () => void
) {
  if (debug()) {
    console.log("Updated: " + filePath);
  }
  triggerIncrementalCompilationOfFile(
    filePath,
    fileContent,
    send,
    onCompilationFinished
  );
}

export function handleClosedFile(filePath: string) {
  if (debug()) {
    console.log("Closed: " + filePath);
  }
  const entry = incrementallyCompiledFileInfo.get(filePath);
  if (entry == null) return;
  cleanUpIncrementalFiles(filePath, entry.project.rootPath);
  incrementallyCompiledFileInfo.delete(filePath);
  originalTypeFileToFilePath.delete(entry.file.originalTypeFileLocation);
  incrementalFilesWatcher.unwatch([entry.file.originalTypeFileLocation]);
}

export function getCodeActionsFromIncrementalCompilation(
  filePath: string
): Array<fileCodeActions> | null {
  const entry = incrementallyCompiledFileInfo.get(filePath);
  if (entry != null) {
    return entry.codeActions;
  }

  return null;
}
