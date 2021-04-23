import { fileURLToPath } from "url";
import { RequestMessage } from "vscode-languageserver";
import { CompletionItem, Hover, Location } from "vscode-languageserver-protocol";
import * as utils from "./utils";
import * as path from "path";
import { execFileSync } from "child_process";
import fs from "fs";

let binariesFolder = path.join(
  path.dirname(__dirname),
  "analysis_binaries"
)

// For local development and CI tests
let currentPlatformBinaryPath = path.join(
  binariesFolder,
  "current-platform.exe"
);
// Platform-specific production binaries manually downloaded from CI
let productionBinaryPath = path.join(
  binariesFolder,
  process.platform + ".exe"
);

let findBinary = () => {
  if (fs.existsSync(currentPlatformBinaryPath)) {
    return currentPlatformBinaryPath
  } else if (fs.existsSync(productionBinaryPath)) {
    return productionBinaryPath
  } else {
    return null
  }
}

// export let binaryExists = fs.existsSync(binaryPath);

// let findExecutable = (uri: string) => {
//   let filePath = fileURLToPath(uri);
//   let projectRootPath = utils.findProjectRootOfFile(filePath);
//   if (projectRootPath == null || !binaryExists) {
//     return null;
//   } else {
//     return {
//       binaryPath: binaryPath,
//       filePath: filePath,
//       cwd: projectRootPath,
//     };
//   }
// };

// type dumpCommandResult = {
//   hover?: string;
//   definition?: { uri?: string; range: any };
// };
// export function runDumpCommand(msg: RequestMessage): dumpCommandResult | null {
//   let executable = findExecutable(msg.params.textDocument.uri);
//   if (executable == null) {
//     return null;
//   }

//   let command =
//     executable.filePath +
//     ":" +
//     msg.params.position.line +
//     ":" +
//     msg.params.position.character;

//   try {
//     let stdout = execFileSync(executable.binaryPath, ["dump", command], {
//       cwd: executable.cwd,
//     });
//     let parsed = JSON.parse(stdout.toString());
//     if (parsed && parsed[0]) {
//       return parsed[0];
//     } else {
//       return null;
//     }
//   } catch (error) {
//     // TODO: @cristianoc any exception possible?
//     return null;
//   }
// }

export function runCompletionCommand(
  msg: RequestMessage,
  code: string
): CompletionItem[] | null {
  let filePath = fileURLToPath(msg.params.textDocument.uri)
  let projectRootPath = utils.findProjectRootOfFile(filePath);
  let binaryPath = findBinary();
  if (binaryPath == null || projectRootPath == null) {
    return null;
  }
  let tmpname = utils.createFileInTempDir();
  fs.writeFileSync(tmpname, code, { encoding: "utf-8" });

  try {
    let stdout = execFileSync(
      binaryPath,
      ["complete", filePath, msg.params.position.line, msg.params.position.character, tmpname],
      { cwd: projectRootPath }
    );
    return JSON.parse(stdout.toString());
  } catch (error) {
    // TODO: @cristianoc any exception possible?
    return null;
  } finally {
    fs.unlink(tmpname, () => null);
  }
}

export function runHoverCommand(
  msg: RequestMessage,
): Hover | null {
  let filePath = fileURLToPath(msg.params.textDocument.uri)
  let projectRootPath = utils.findProjectRootOfFile(filePath);
  let binaryPath = findBinary();
  if (binaryPath == null || projectRootPath == null) {
    return null;
  }

  try {
    let stdout = execFileSync(
      binaryPath,
      ["hover", filePath, msg.params.position.line, msg.params.position.character],
      { cwd: projectRootPath }
    );
    return JSON.parse(stdout.toString());
  } catch (error) {
    // TODO: @cristianoc any exception possible?
    return null;
  }
}

export function runDefinitionCommand(
  msg: RequestMessage,
): Location | null {
  let filePath = fileURLToPath(msg.params.textDocument.uri)
  let projectRootPath = utils.findProjectRootOfFile(filePath);
  let binaryPath = findBinary();
  if (binaryPath == null || projectRootPath == null) {
    return null;
  }

  try {
    let stdout = execFileSync(
      binaryPath,
      ["definition", filePath, msg.params.position.line, msg.params.position.character],
      { cwd: projectRootPath }
    );
    return JSON.parse(stdout.toString());
  } catch (error) {
    // TODO: @cristianoc any exception possible?
    return null;
  }
}
