import * as path from "path";
import fs from "fs";
import * as utils from "./utils";
import { pathToFileURL } from "url";
import readline from "readline";
import { performance } from "perf_hooks";
import * as p from "vscode-languageserver-protocol";
import * as cp from "node:child_process";
import { send } from "./config";
import * as c from "./constants";

/*
 * TODO CMT stuff
 * - Compile resi
 * - Wait a certain threshold for compilation before using the old cmt
 */

let debug = true;

let buildNinjaCache: Map<string, [number, Array<string>]> = new Map();
let savedIncrementalFiles: Set<string> = new Set();
let compileContentsCache: Map<string, { timeout: any; triggerToken: number }> =
  new Map();
let compileContentsListeners: Map<string, Array<() => void>> = new Map();

export function cleanupIncrementalFilesAfterCompilation(changedPath: string) {
  const projectRootPath = utils.findProjectRootOfFile(changedPath);
  if (projectRootPath != null) {
    savedIncrementalFiles.forEach((filePath) => {
      if (filePath.startsWith(projectRootPath)) {
        cleanUpIncrementalFiles(filePath, projectRootPath);
        savedIncrementalFiles.delete(filePath);
      }
    });
  }
}

export function removeIncrementalFileFolder(
  projectRootPath: string,
  onAfterRemove?: () => void
) {
  fs.rm(
    path.resolve(projectRootPath, "lib/bs/___incremental"),
    { force: true, recursive: true },
    (_) => {
      onAfterRemove?.();
    }
  );
}

export function recreateIncrementalFileFolder(projectRootPath: string) {
  removeIncrementalFileFolder(projectRootPath, () => {
    fs.mkdir(path.resolve(projectRootPath, "lib/bs/___incremental"), (_) => {});
  });
}

export function fileIsIncrementallyCompiled(filePath: string): boolean {
  let projectRootPath = utils.findProjectRootOfFile(filePath);
  let fileName = path.basename(filePath, ".res");
  if (projectRootPath != null) {
    return fs.existsSync(
      path.resolve(projectRootPath, "lib/bs/___incremental", fileName + ".cmt")
    );
  }
  return false;
}

export function cleanUpIncrementalFiles(
  filePath: string,
  projectRootPath: string
) {
  [
    path.basename(filePath, ".res") + ".ast",
    path.basename(filePath, ".res") + ".cmt",
    path.basename(filePath, ".res") + ".cmi",
    path.basename(filePath, ".res") + ".cmj",
    path.basename(filePath),
  ].forEach((file) => {
    fs.unlink(
      path.resolve(projectRootPath, "lib/bs/___incremental", file),
      (_) => {}
    );
  });
}
function getBscArgs(projectRootPath: string): Promise<Array<string>> {
  let buildNinjaPath = path.resolve(projectRootPath, "lib/bs/build.ninja");
  let cacheEntry = buildNinjaCache.get(buildNinjaPath);
  let stat: fs.Stats | null = null;
  if (cacheEntry != null) {
    stat = fs.statSync(buildNinjaPath);
    if (cacheEntry[0] >= stat.mtimeMs) {
      return Promise.resolve(cacheEntry[1]);
    }
  }
  return new Promise((resolve, _reject) => {
    function resolveResult(result: Array<string>) {
      if (stat != null) {
        buildNinjaCache.set(buildNinjaPath, [stat.mtimeMs, result]);
      }
      resolve(result);
    }
    const fileStream = fs.createReadStream(
      path.resolve(projectRootPath, "lib/bs/build.ninja")
    );
    const rl = readline.createInterface({
      input: fileStream,
      crlfDelay: Infinity,
    });
    let captureNextLine = false;
    let done = false;
    let stopped = false;
    let captured: Array<string> = [];
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
  let s = cmdString
    .trim()
    .split("command = ")[1]
    .split(" ")
    .map((v) => v.trim())
    .filter((v) => v !== "");
  let args: Array<Array<string>> = [];

  for (let i = 0; i <= s.length - 1; i++) {
    let item = s[i];
    let nextItem = s[i + 1] ?? "";
    if (item.startsWith("-") && nextItem.startsWith("-")) {
      // Single entry arg
      args.push([item]);
    } else if (item.startsWith("-") && s[i + 1]?.startsWith("'")) {
      let nextIndex = i + 1;
      // Quoted arg, take until ending '
      let arg = [s[nextIndex]];
      for (let x = nextIndex + 1; x <= s.length - 1; x++) {
        let nextItem = s[x];
        arg.push(nextItem);
        if (nextItem.endsWith("'")) {
          i = x + 1;
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
  let cacheEntry = compileContentsCache.get(filePath);
  if (cacheEntry != null) {
    clearTimeout(cacheEntry.timeout);
    compileContentsListeners.get(filePath)?.forEach((cb) => cb());
    compileContentsListeners.delete(filePath);
  }
  let triggerToken = performance.now();
  compileContentsCache.set(filePath, {
    timeout: setTimeout(() => {
      compileContents(
        filePath,
        fileContent,
        triggerToken,
        send,
        onCompilationFinished
      );
    }, 20),
    triggerToken,
  });
}
function verifyTriggerToken(filePath: string, triggerToken: number): boolean {
  return compileContentsCache.get(filePath)?.triggerToken === triggerToken;
}
async function compileContents(
  filePath: string,
  fileContent: string,
  triggerToken: number,
  send: (msg: p.Message) => void,
  onCompilationFinished?: () => void
) {
  let startTime = performance.now();
  let fileName = path.basename(filePath);
  const projectRootPath = utils.findProjectRootOfFile(filePath);
  if (projectRootPath == null) {
    if (debug) console.log("Did not find root project.");
    return;
  }
  let bscExe = utils.findBscExeBinary(projectRootPath);
  if (bscExe == null) {
    if (debug) console.log("Did not find bsc.");
    return;
  }
  let incrementalFilePath = path.resolve(
    projectRootPath,
    "lib/bs/___incremental",
    fileName
  );

  fs.writeFileSync(incrementalFilePath, fileContent);

  try {
    let [astBuildCommand, fullBuildCommand] = await getBscArgs(projectRootPath);

    let astArgs = argsFromCommandString(astBuildCommand);
    let buildArgs = argsFromCommandString(fullBuildCommand);

    let callArgs: Array<string> = [
      "-I",
      path.resolve(projectRootPath, "lib/bs/___incremental"),
    ];

    buildArgs.forEach(([key, value]: Array<string>) => {
      if (key === "-I") {
        callArgs.push("-I", path.resolve(projectRootPath, "lib/bs", value));
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
    callArgs.push("-ignore-parse-errors");

    callArgs = callArgs.filter((v) => v != null && v !== "");
    callArgs.push(incrementalFilePath);

    let process = cp.execFile(
      bscExe,
      callArgs,
      { cwd: projectRootPath },
      (error, _stdout, stderr) => {
        if (!error?.killed) {
          if (debug)
            console.log(
              `Recompiled ${fileName} in ${
                (performance.now() - startTime) / 1000
              }s`
            );
        } else {
          if (debug) console.log(`Compilation of ${fileName} was killed.`);
        }
        onCompilationFinished?.();
        if (!error?.killed && verifyTriggerToken(filePath, triggerToken)) {
          let { result } = utils.parseCompilerLogOutput(`${stderr}\n#Done()`);
          let res = (Object.values(result)[0] ?? [])
            .map((d) => ({
              ...d,
              message: removeAnsiCodes(d.message),
            }))
            // Filter out a few unwanted parser errors since we run the parser in ignore mode
            .filter(
              (d) =>
                !d.message.startsWith("Uninterpreted extension 'rescript.") &&
                !d.message.includes(`/___incremental/${fileName}`)
            );

          let notification: p.NotificationMessage = {
            jsonrpc: c.jsonrpcVersion,
            method: "textDocument/publishDiagnostics",
            params: {
              uri: pathToFileURL(filePath),
              diagnostics: res,
            },
          };
          send(notification);
        }
      }
    );
    let listeners = compileContentsListeners.get(filePath) ?? [];
    listeners.push(() => {
      process.kill("SIGKILL");
    });
    compileContentsListeners.set(filePath, listeners);
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
  savedIncrementalFiles.delete(filePath);
  triggerIncrementalCompilationOfFile(
    filePath,
    fileContent,
    send,
    onCompilationFinished
  );
}

export function handleDidSaveTextDocument(filePath: string) {
  savedIncrementalFiles.add(filePath);
}
