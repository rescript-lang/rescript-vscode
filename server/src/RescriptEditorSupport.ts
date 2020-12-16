import { fileURLToPath } from "url";
import { RequestMessage } from "vscode-languageserver";
import * as utils from "./utils";
import * as project from "./project";
import * as path from "path";
import { exec } from "child_process";
import fs from "fs";

let binaryPath = path.join(
  path.dirname(__dirname),
  process.platform,
  "rescript-editor-support.exe"
);

export let binaryExists = fs.existsSync(binaryPath);

let findExecutable = (uri: string) => {
  let filePath = fileURLToPath(uri);
  let projectRootPath = project.findBscPath(filePath);
  if (projectRootPath == null || !binaryExists) {
    return null;
  } else {
    return { binaryPath, filePath, cwd: projectRootPath };
  }
};

export function runDumpCommand(
  msg: RequestMessage,
  onResult: (
    result: { hover?: string; definition?: { uri?: string; range: any } } | null
  ) => void
) {
  let executable = findExecutable(msg.params.textDocument.uri);
  if (executable == null) {
    onResult(null);
  } else {
    let command =
      executable.binaryPath +
      " dump " +
      executable.filePath +
      ":" +
      msg.params.position.line +
      ":" +
      msg.params.position.character;
    exec(command, { cwd: executable.cwd }, function (_error, stdout, _stderr) {
      let result = JSON.parse(stdout);
      if (result && result[0]) {
        onResult(result[0]);
      } else {
        onResult(null);
      }
    });
  }
}

export function runCompletionCommand(
  msg: RequestMessage,
  code: string,
  onResult: (result: [{ label: string }] | null) => void
) {
  let executable = findExecutable(msg.params.textDocument.uri);
  if (executable == null) {
    onResult(null);
  } else {
    let tmpname = utils.createFileInTempDir();
    fs.writeFileSync(tmpname, code, { encoding: "utf-8" });

    let command =
      executable.binaryPath +
      " complete " +
      executable.filePath +
      ":" +
      msg.params.position.line +
      ":" +
      msg.params.position.character +
      " " +
      tmpname;

    exec(command, { cwd: executable.cwd }, function (_error, stdout, _stderr) {
      // async close is fine. We don't use this file name again
      fs.unlink(tmpname, () => null);
      let result = JSON.parse(stdout);
      if (result && result[0]) {
        onResult(result);
      } else {
        onResult(null);
      }
    });
  }
}
