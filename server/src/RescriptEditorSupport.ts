import { fileURLToPath } from "url";
import { RequestMessage } from "vscode-languageserver";
import {
  CompletionItem,
  Hover,
  Location,
} from "vscode-languageserver-protocol";
import * as utils from "./utils";
import { execFileSync } from "child_process";
import fs from "fs";
import {
  analysisCurrentPlatformBinaryPath,
  analysisProductionBinaryPath,
} from "./constants";

let findBinary = () => {
  if (fs.existsSync(analysisCurrentPlatformBinaryPath)) {
    return analysisCurrentPlatformBinaryPath;
  } else if (fs.existsSync(analysisProductionBinaryPath)) {
    return analysisProductionBinaryPath;
  } else {
    return null;
  }
};

export function runCompletionCommand(
  msg: RequestMessage,
  code: string
): CompletionItem[] | null {
  let filePath = fileURLToPath(msg.params.textDocument.uri);
  let projectRootPath = utils.findProjectRootOfFile(filePath);
  let binaryPath = findBinary();
  if (binaryPath == null || projectRootPath == null) {
    return null;
  }
  let tmpname = utils.createFileInTempDir();
  fs.writeFileSync(tmpname, code, { encoding: "utf-8" });

  let stdout = execFileSync(
    binaryPath,
    [
      "complete",
      filePath,
      msg.params.position.line,
      msg.params.position.character,
      tmpname,
    ],
    { cwd: projectRootPath }
  );
  fs.unlink(tmpname, () => null);
  return JSON.parse(stdout.toString());
}

export function runHoverCommand(msg: RequestMessage): Hover | null {
  let filePath = fileURLToPath(msg.params.textDocument.uri);
  let projectRootPath = utils.findProjectRootOfFile(filePath);
  let binaryPath = findBinary();
  if (binaryPath == null || projectRootPath == null) {
    return null;
  }

  let stdout = execFileSync(
    binaryPath,
    [
      "hover",
      filePath,
      msg.params.position.line,
      msg.params.position.character,
    ],
    { cwd: projectRootPath }
  );
  return JSON.parse(stdout.toString());
}

export function runDefinitionCommand(msg: RequestMessage): Location | null {
  let filePath = fileURLToPath(msg.params.textDocument.uri);
  let projectRootPath = utils.findProjectRootOfFile(filePath);
  let binaryPath = findBinary();
  if (binaryPath == null || projectRootPath == null) {
    return null;
  }

  let stdout = execFileSync(
    binaryPath,
    [
      "definition",
      filePath,
      msg.params.position.line,
      msg.params.position.character,
    ],
    { cwd: projectRootPath }
  );
  return JSON.parse(stdout.toString());
}
