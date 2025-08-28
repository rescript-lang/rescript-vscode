import * as path from "path";
import fs from "fs";
import * as utils from "./utils";
import { pathToFileURL } from "url";
import readline from "readline";
import { performance } from "perf_hooks";
import * as p from "vscode-languageserver-protocol";
import * as cp from "node:child_process";
import semver from "semver";
import * as os from "os";
import config, { send } from "./config";
import * as c from "./constants";
import { fileCodeActions } from "./codeActions";
import { projectsFiles } from "./projectFiles";

function debug() {
  return (
    config.extensionConfiguration.incrementalTypechecking?.debugLogging ?? false
  );
}

const INCREMENTAL_FOLDER_NAME = "___incremental";
const INCREMENTAL_FILE_FOLDER_LOCATION = path.join(
  c.compilerDirPartialPath,
  INCREMENTAL_FOLDER_NAME,
);

type RewatchCompilerArgs = {
  compiler_args: Array<string>;
  parser_args: Array<string>;
};

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
  buildSystem: "bsb" | "rewatch";
  /** Cache for build.ninja assets. */
  buildNinja: {
    /** When build.ninja was last modified. Used as a cache key. */
    fileMtime: number;
    /** The raw, extracted needed info from build.ninja. Needs processing. */
    rawExtracted: Array<string>;
  } | null;
  /** Cache for rewatch compiler args. */
  buildRewatch: {
    lastFile: string;
    compilerArgs: RewatchCompilerArgs;
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
    /** The root path of the workspace (if a monorepo) */
    workspaceRootPath: string;
    /** Computed location of bsc. */
    bscBinaryLocation: string;
    /** The arguments needed for bsc, derived from the project configuration/build.ninja. */
    callArgs: Promise<Array<string> | null>;
    /** The location of the incremental folder for this project. */
    incrementalFolderPath: string;
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

export function incrementalCompilationFileChanged(changedPath: string) {
  const filePath = originalTypeFileToFilePath.get(changedPath);
  if (filePath != null) {
    const entry = incrementallyCompiledFileInfo.get(filePath);
    if (entry != null) {
      if (debug()) {
        console.log("[watcher] Cleaning up incremental files for " + filePath);
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
        entry.project.rootPath,
      );
    }
  }
}

export function removeIncrementalFileFolder(
  projectRootPath: string,
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

export function recreateIncrementalFileFolder(projectRootPath: string) {
  if (debug()) {
    console.log("Recreating incremental file folder");
  }
  removeIncrementalFileFolder(projectRootPath, () => {
    fs.mkdir(
      path.resolve(projectRootPath, INCREMENTAL_FILE_FOLDER_LOCATION),
      { recursive: true },
      (_) => {},
    );
  });
}

export function cleanUpIncrementalFiles(
  filePath: string,
  projectRootPath: string,
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
function getBscArgs(
  entry: IncrementallyCompiledFileInfo,
): Promise<Array<string> | RewatchCompilerArgs | null> {
  const buildNinjaPath = path.resolve(
    entry.project.rootPath,
    c.buildNinjaPartialPath,
  );
  const rewatchLockfile = path.resolve(
    entry.project.workspaceRootPath,
    c.rewatchLockPartialPath,
  );
  const rescriptLockfile = path.resolve(
    entry.project.workspaceRootPath,
    c.rescriptLockPartialPath,
  );
  let buildSystem: "bsb" | "rewatch" | null = null;

  let stat: fs.Stats | null = null;
  try {
    stat = fs.statSync(buildNinjaPath);
    buildSystem = "bsb";
  } catch {}
  try {
    stat = fs.statSync(rewatchLockfile);
    buildSystem = "rewatch";
  } catch {}
  try {
    stat = fs.statSync(rescriptLockfile);
    buildSystem = "rewatch";
  } catch {}
  if (buildSystem == null) {
    console.log("Did not find build.ninja or rewatch.lock, cannot proceed..");
    return Promise.resolve(null);
  } else if (debug()) {
    console.log(
      `Using build system: ${buildSystem} for ${entry.file.sourceFilePath}`,
    );
  }
  const bsbCacheEntry = entry.buildNinja;
  const rewatchCacheEntry = entry.buildRewatch;

  if (
    buildSystem === "bsb" &&
    bsbCacheEntry != null &&
    stat != null &&
    bsbCacheEntry.fileMtime >= stat.mtimeMs
  ) {
    return Promise.resolve(bsbCacheEntry.rawExtracted);
  }
  if (
    buildSystem === "rewatch" &&
    rewatchCacheEntry != null &&
    rewatchCacheEntry.lastFile === entry.file.sourceFilePath
  ) {
    return Promise.resolve(rewatchCacheEntry.compilerArgs);
  }
  return new Promise(async (resolve, _reject) => {
    function resolveResult(result: Array<string> | RewatchCompilerArgs) {
      if (stat != null && Array.isArray(result)) {
        entry.buildSystem = "bsb";
        entry.buildNinja = {
          fileMtime: stat.mtimeMs,
          rawExtracted: result,
        };
      } else if (!Array.isArray(result)) {
        entry.buildSystem = "rewatch";
        entry.buildRewatch = {
          lastFile: entry.file.sourceFilePath,
          compilerArgs: result,
        };
      }
      resolve(result);
    }

    if (buildSystem === "bsb") {
      const fileStream = fs.createReadStream(buildNinjaPath, {
        encoding: "utf8",
      });
      fileStream.on("error", (err) => {
        console.error("File stream error:", err);
        resolveResult([]);
      });
      const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity,
      });
      let captureNextLine = false;
      let done = false;
      let stopped = false;
      const captured: Array<string> = [];
      rl.on("line", (line) => {
        line = line.trim(); // Normalize line endings
        if (stopped) {
          return;
        }
        if (captureNextLine) {
          captured.push(line);
          captureNextLine = false;
        }
        if (done) {
          // Not sure if fileStream.destroy is necessary, rl.close() will handle it gracefully.
          // fileStream.destroy();
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
      rl.on("error", (err) => {
        console.error("Readline error:", err);
        resolveResult([]);
      });
      rl.on("close", () => {
        resolveResult(captured);
      });
    } else if (buildSystem === "rewatch") {
      try {
        const project = projectsFiles.get(entry.project.rootPath);
        if (project?.rescriptVersion == null) return;
        let rewatchPath = path.resolve(
          entry.project.workspaceRootPath,
          "node_modules/@rolandpeelen/rewatch/rewatch",
        );
        let rescriptRewatchPath = null;
        if (
          semver.valid(project.rescriptVersion) &&
          semver.satisfies(project.rescriptVersion as string, ">11", {
            includePrerelease: true,
          })
        ) {
          rescriptRewatchPath = await utils.findRewatchBinary(
            entry.project.workspaceRootPath,
          );
        }

        if (
          semver.valid(project.rescriptVersion) &&
          semver.satisfies(
            project.rescriptVersion as string,
            ">=12.0.0-beta.1",
            { includePrerelease: true },
          )
        ) {
          rescriptRewatchPath = await utils.findRescriptExeBinary(
            entry.project.workspaceRootPath,
          );
        }

        if (rescriptRewatchPath != null) {
          rewatchPath = rescriptRewatchPath;
          if (debug()) {
            console.log(
              `Found rewatch binary bundled with v12: ${rescriptRewatchPath}`,
            );
          }
        } else {
          if (debug()) {
            console.log("Did not find rewatch binary bundled with v12");
          }
        }

        const rewatchArguments = semver.satisfies(
          project.rescriptVersion,
          ">=12.0.0-beta.2",
          { includePrerelease: true },
        )
          ? ["compiler-args", entry.file.sourceFilePath]
          : [
              "--rescript-version",
              project.rescriptVersion,
              "--compiler-args",
              entry.file.sourceFilePath,
            ];
        const bscExe = await utils.findBscExeBinary(
          entry.project.workspaceRootPath,
        );
        const env = bscExe != null ? { RESCRIPT_BSC_EXE: bscExe } : undefined;
        const compilerArgs = JSON.parse(
          cp
            .execFileSync(rewatchPath, rewatchArguments, { env })
            .toString()
            .trim(),
        ) as RewatchCompilerArgs;
        resolveResult(compilerArgs);
      } catch (e) {
        console.error(e);
      }
    }
  });
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
  filePath: string,
  fileContent: string,
  send: send,
  onCompilationFinished?: () => void,
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
    const project = projectsFiles.get(projectRootPath);
    if (project == null) {
      if (debug()) console.log("Did not find open project for " + filePath);
      return;
    }

    const projectRewatchLockfile = path.resolve(
      projectRootPath,
      c.rewatchLockPartialPath,
    );

    let foundRewatchLockfileInProjectRoot = false;
    if (fs.existsSync(projectRewatchLockfile)) {
      foundRewatchLockfileInProjectRoot = true;
    }

    // if we find a rewatch.lock in the project root, it's a compilation of a local package
    // in the workspace.
    const workspaceRootPath =
      projectRootPath && !foundRewatchLockfileInProjectRoot
        ? utils.findProjectRootOfFile(projectRootPath, true)
        : null;

    const bscBinaryLocation = project.bscBinaryLocation;
    if (bscBinaryLocation == null) {
      if (debug())
        console.log("Could not find bsc binary location for " + filePath);
      return;
    }
    const ext = filePath.endsWith(".resi") ? ".resi" : ".res";
    const moduleName = path.basename(filePath, ext);
    const moduleNameNamespaced =
      project.namespaceName != null
        ? `${moduleName}-${project.namespaceName}`
        : moduleName;

    const incrementalFolderPath = path.join(
      projectRootPath,
      INCREMENTAL_FILE_FOLDER_LOCATION,
    );

    let originalTypeFileLocation = path.resolve(
      projectRootPath,
      c.compilerDirPartialPath,
      path.relative(projectRootPath, filePath),
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
        workspaceRootPath: workspaceRootPath ?? projectRootPath,
        rootPath: projectRootPath,
        callArgs: Promise.resolve([]),
        bscBinaryLocation,
        incrementalFolderPath,
      },
      buildSystem: foundRewatchLockfileInProjectRoot ? "rewatch" : "bsb",
      buildRewatch: null,
      buildNinja: null,
      compilation: null,
      killCompilationListeners: [],
      codeActions: [],
    };

    incrementalFileCacheEntry.project.callArgs = figureOutBscArgs(
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

const isWindows = os.platform() === "win32";

async function figureOutBscArgs(entry: IncrementallyCompiledFileInfo) {
  const project = projectsFiles.get(entry.project.rootPath);
  if (project?.rescriptVersion == null) {
    if (debug()) {
      console.log(
        "Found no project (or ReScript version) for " +
          entry.file.sourceFilePath,
      );
    }
    return null;
  }
  const res = await getBscArgs(entry);
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
async function compileContents(
  entry: IncrementallyCompiledFileInfo,
  fileContent: string,
  send: (msg: p.Message) => void,
  onCompilationFinished?: () => void,
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
          "Could not figure out call args. Maybe build.ninja does not exist yet?",
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
    let cwd =
      entry.buildSystem === "bsb"
        ? entry.project.rootPath
        : path.resolve(entry.project.rootPath, c.compilerDirPartialPath);
    if (debug()) {
      console.log(
        `About to invoke bsc from \"${cwd}\", used ${entry.buildSystem}`,
      );
      console.log(
        `${entry.project.bscBinaryLocation} ${callArgs.map((c) => `"${c}"`).join(" ")}`,
      );
    }
    const process = cp.execFile(
      entry.project.bscBinaryLocation,
      callArgs,
      { cwd },
      async (error, _stdout, stderr) => {
        if (!error?.killed) {
          if (debug())
            console.log(
              `Recompiled ${entry.file.sourceFileName} in ${
                (performance.now() - startTime) / 1000
              }s`,
            );
        } else {
          if (debug())
            console.log(
              `Compilation of ${entry.file.sourceFileName} was killed.`,
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
          const { result, codeActions } = await utils.parseCompilerLogOutput(
            `${stderr}\n#Done()`,
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
                (!d.message.includes(
                  `/${INCREMENTAL_FOLDER_NAME}/${entry.file.sourceFileName}`,
                ) ||
                  // The `Multiple definition of the <kind> name <name>` type error's
                  // message includes the filepath with LOC of the duplicate definition
                  d.message.startsWith("Multiple definition of the") ||
                  // The signature mismatch, with mismatch and ill typed applicative functor
                  // type errors all include the filepath with LOC
                  d.message.startsWith("Signature mismatch") ||
                  d.message.startsWith("In this `with' constraint") ||
                  d.message.startsWith("This `with' constraint on"))
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
                "error.log",
              );
              fs.writeFileSync(
                logfile,
                `== BSC ARGS ==\n${callArgs?.join(
                  " ",
                )}\n\n== OUTPUT ==\n${stderr}`,
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
      },
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
  onCompilationFinished?: () => void,
) {
  if (debug()) {
    console.log("Updated: " + filePath);
  }
  triggerIncrementalCompilationOfFile(
    filePath,
    fileContent,
    send,
    onCompilationFinished,
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
}

export function getCodeActionsFromIncrementalCompilation(
  filePath: string,
): Array<fileCodeActions> | null {
  const entry = incrementallyCompiledFileInfo.get(filePath);
  if (entry != null) {
    return entry.codeActions;
  }

  return null;
}
