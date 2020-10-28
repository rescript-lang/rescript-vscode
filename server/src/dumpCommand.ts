import { fileURLToPath } from "url";
import { RequestMessage } from "vscode-languageserver";
import * as utils from "./utils";
import * as path from "path";
import { exec } from "child_process";

export function runDumpCommand(
	msg: RequestMessage,
  onResult: (
    result: { hover?: string; definition?: { uri?: string; range: any } } | null
  ) => void
) {
  let filePath = fileURLToPath(msg.params.textDocument.uri);
  let projectRootPath = utils.findProjectRootOfFile(filePath);
  if (projectRootPath == null) {
    onResult(null);
  } else {
	// Currently assumes the dump command is "bin.exe" at the project root.
    let commandPath = path.join(projectRootPath, "bin.exe");
    let command =
      commandPath +
      " dump " +
      filePath +
      ":" +
      msg.params.position.line +
      ":" +
      msg.params.position.character;
    exec(command, { cwd: projectRootPath }, function (_error, _stdout, stderr) {
      let result = JSON.parse(stderr);
      if (result && result[0]) {
        onResult(result[0]);
      } else {
        onResult(null);
      }
    });
  }
}
