import * as path from "path";
import * as fs from "fs";
import * as os from "os";

const platformDir =
  process.arch === "arm64" ? process.platform + process.arch : process.platform;

const analysisDevPath = path.join(
  path.dirname(__dirname),
  "..",
  "analysis",
  "rescript-editor-analysis.exe"
);

export const analysisProdPath = path.join(
  path.dirname(__dirname),
  "..",
  "server",
  "analysis_binaries",
  platformDir,
  "rescript-editor-analysis.exe"
);

export const getAnalysisBinaryPath = (): string | null => {
  if (fs.existsSync(analysisDevPath)) {
    return analysisDevPath;
  } else if (fs.existsSync(analysisProdPath)) {
    return analysisProdPath;
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
