import { fileURLToPath } from "url";
import { RequestMessage } from "vscode-languageserver";
import * as utils from "./utils";
import * as path from "path";
import { execFileSync } from "child_process";
import fs from "fs";

let binaryPath = path.join(
  path.dirname(__dirname),
  process.platform,
  "rescript-editor-support.exe"
);

export let binaryExists = fs.existsSync(binaryPath);

let findExecutable = (uri: string) => {
  let filePath = fileURLToPath(uri);
  let projectRootPath = utils.findProjectRootOfFile(filePath);
  if (projectRootPath == null || !binaryExists) {
    return null;
  } else {
    return {
      binaryPath: binaryPath,
      filePath: filePath,
      cwd: projectRootPath,
    };
  }
};

type dumpCommandResult = {
  hover?: string;
  definition?: { uri?: string; range: any };
};
export function runDumpCommand(msg: RequestMessage): dumpCommandResult | null {
  let executable = findExecutable(msg.params.textDocument.uri);
  if (executable == null) {
    return null;
  }

  let command =
    executable.filePath +
    ":" +
    msg.params.position.line +
    ":" +
    msg.params.position.character;

  try {
    let stdout = execFileSync(executable.binaryPath, ["dump", command], {
      cwd: executable.cwd,
    });
    let parsed = JSON.parse(stdout.toString());
    if (parsed && parsed[0]) {
      return parsed[0];
    } else {
      return null;
    }
  } catch (error) {
    // TODO: @cristianoc any exception possible?
    return null;
  }
}

type completionCommandResult = [{ label: string }];
export function runCompletionCommand(
  msg: RequestMessage,
  code: string
): completionCommandResult | null {
  let executable = findExecutable(msg.params.textDocument.uri);
  if (executable == null) {
    return null;
  }
  let tmpname = utils.createFileInTempDir();
  fs.writeFileSync(tmpname, code, { encoding: "utf-8" });

  let command =
    executable.filePath +
    ":" +
    msg.params.position.line +
    ":" +
    msg.params.position.character;

  try {
    let stdout = execFileSync(
      executable.binaryPath,
      ["complete", command, tmpname],
      { cwd: executable.cwd }
    );
    let parsed = JSON.parse(stdout.toString());
    if (parsed && parsed[0]) {
      return parsed;
    } else {
      return null;
    }
  } catch (error) {
    // TODO: @cristianoc any exception possible?
    return null;
  } finally {
    fs.unlink(tmpname, () => null);
  }
}
