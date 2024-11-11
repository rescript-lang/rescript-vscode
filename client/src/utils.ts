import * as path from "path";
import * as fs from "fs";
import * as os from "os";
import { DocumentUri } from "vscode-languageclient";

/*
 * Much of the code in here is duplicated from the server code.
 * At some point we should move the functionality powered by this
 * to the server itself.
 */

type binaryName = "rescript-editor-analysis.exe" | "rescript-tools.exe";

const platformDir =
  process.arch === "arm64" ? process.platform + process.arch : process.platform;

const getLegacyBinaryDevPath = (b: binaryName) =>
  path.join(path.dirname(__dirname), "..", "analysis", b);

export const getLegacyBinaryProdPath = (b: binaryName) =>
  path.join(
    path.dirname(__dirname),
    "..",
    "server",
    "analysis_binaries",
    platformDir,
    b
  );

export const getBinaryPath = (
  binaryName: "rescript-editor-analysis.exe" | "rescript-tools.exe",
  projectRootPath: string
): string | null => {
  const binaryFromCompilerPackage = path.join(
    projectRootPath,
    "node_modules",
    "rescript",
    platformDir,
    binaryName
  );

  if (fs.existsSync(binaryFromCompilerPackage)) {
    return binaryFromCompilerPackage;
  } else if (fs.existsSync(getLegacyBinaryDevPath(binaryName))) {
    return getLegacyBinaryDevPath(binaryName);
  } else if (fs.existsSync(getLegacyBinaryProdPath(binaryName))) {
    return getLegacyBinaryProdPath(binaryName);
  } else {
    return null;
  }
};

let tempFilePrefix = "rescript_" + process.pid + "_";
let tempFileId = 0;

export const createFileInTempDir = (prefix = "", extension = "") => {
  let tempFileName = prefix + "_" + tempFilePrefix + tempFileId + extension;
  tempFileId = tempFileId + 1;
  return path.join(os.tmpdir(), tempFileName);
};

export let findProjectRootOfFileInDir = (
  source: DocumentUri
): null | DocumentUri => {
  let dir = path.dirname(source);
  if (
    fs.existsSync(path.join(dir, "rescript.json")) ||
    fs.existsSync(path.join(dir, "bsconfig.json"))
  ) {
    return dir;
  } else {
    if (dir === source) {
      // reached top
      return null;
    } else {
      return findProjectRootOfFileInDir(dir);
    }
  }
};
