import { fileURLToPath } from "url";
import { RequestMessage } from "vscode-languageserver";
import * as utils from "./utils";
import * as path from "path";
import { exec } from "child_process";
import * as tmp from "tmp";
import fs from "fs";

let findExecutable = (uri: string) => {
  let filePath = fileURLToPath(uri);
  let projectRootPath = utils.findProjectRootOfFile(filePath);
  if (projectRootPath == null) {
    return null;
  } else {
    // Currently assumes the dump command is "bin.exe" at the project root.
    let binaryPath = path.join(projectRootPath, "bin.exe");
    if (fs.existsSync(binaryPath)) {
      return { binaryPath, filePath, cwd: projectRootPath };
    } else {
      return null;
    }
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
    exec(command, { cwd: executable.cwd }, function (_error, _stdout, stderr) {
      let result = JSON.parse(stderr);
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
    let tmpobj = tmp.fileSync();
    let tmpname = tmpobj.name;
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

    exec(command, { cwd: executable.cwd }, function (_error, _stdout, stderr) {
      tmpobj.removeCallback();
      let result = JSON.parse(stderr);
      if (result && result[0]) {
        onResult(result);
      } else {
        onResult(null);
      }
    });
  }
}
