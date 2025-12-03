import * as path from "path";
import * as fs from "fs";
import * as os from "os";
import { DocumentUri } from "vscode-languageclient";

/*
 * Much of the code in here is duplicated from the server code.
 * At some point we should move the functionality powered by this
 * to the server itself.
 */

/**
 * Branded type for normalized file paths.
 *
 * All paths should be normalized to ensure consistent lookups and prevent
 * path format mismatches (e.g., trailing slashes, relative vs absolute paths).
 *
 * Use `normalizePath()` to convert a regular path to a `NormalizedPath`.
 */
export type NormalizedPath = string & { __brand: "NormalizedPath" };

/**
 * Normalizes a file path and returns it as a `NormalizedPath`.
 *
 * @param filePath - The path to normalize (can be null)
 * @returns The normalized path, or null if input was null
 */
export function normalizePath(filePath: string | null): NormalizedPath | null {
  // `path.normalize` ensures we can assume string is now NormalizedPath
  return filePath != null ? (path.normalize(filePath) as NormalizedPath) : null;
}

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
    b,
  );

export const getBinaryPath = (
  binaryName: "rescript-editor-analysis.exe" | "rescript-tools.exe",
  projectRootPath: NormalizedPath | null = null,
): string | null => {
  const binaryFromCompilerPackage = path.join(
    projectRootPath ?? "",
    "node_modules",
    "rescript",
    platformDir,
    binaryName,
  );

  if (projectRootPath != null && fs.existsSync(binaryFromCompilerPackage)) {
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
  source: string,
): NormalizedPath | null => {
  const normalizedSource = normalizePath(source);
  if (normalizedSource == null) {
    return null;
  }
  const dir = normalizePath(path.dirname(normalizedSource));
  if (dir == null) {
    return null;
  }
  if (
    fs.existsSync(path.join(dir, "rescript.json")) ||
    fs.existsSync(path.join(dir, "bsconfig.json"))
  ) {
    return dir;
  } else {
    if (dir === normalizedSource) {
      // reached top
      return null;
    } else {
      return findProjectRootOfFileInDir(dir);
    }
  }
};
