import * as path from "path";
import * as fs from "fs";
import * as os from "os";
import { DocumentUri } from "vscode-languageclient";
import * as semver from "semver";
import { getBinaryPathLegacy } from "./utils-legacy";

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

// v12+ format: with hyphen (e.g., "darwin-arm64")
const platformTarget = `${process.platform}-${process.arch}`;

// ============================================================================
// Version Detection
// ============================================================================

/**
 * Finds the ReScript version from package.json in the project.
 */
export const findReScriptVersion = (
  projectRootPath: NormalizedPath | null,
): string | null => {
  if (projectRootPath == null) {
    return null;
  }
  try {
    const packageJsonPath = path.join(
      projectRootPath,
      "node_modules",
      "rescript",
      "package.json",
    );
    if (!fs.existsSync(packageJsonPath)) {
      return null;
    }
    const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, "utf-8"));
    return packageJson.version ?? null;
  } catch {
    return null;
  }
};

// ============================================================================
// ReScript 12+ Binary Finding (Clean, self-contained)
// ============================================================================

/**
 * Finds binaries for ReScript 12+ using @rescript/${target}/bin.js structure.
 * This is the single source of truth for binary locations in v12+.
 * Returns null if binary not found, throws on critical errors.
 */
const getBinaryPathReScript12 = (
  projectRootPath: NormalizedPath,
  binaryName: binaryName,
): string | null => {
  const binJsPath = path.join(
    projectRootPath,
    "node_modules",
    "@rescript",
    platformTarget,
    "bin.js",
  );

  if (!fs.existsSync(binJsPath)) {
    return null;
  }

  // Read bin.js and extract the binary path
  // bin.js exports binPaths object with paths to binaries
  const binDir = path.join(
    projectRootPath,
    "node_modules",
    "@rescript",
    platformTarget,
    "bin",
  );

  let binaryPath: string | null = null;
  if (binaryName === "rescript-tools.exe") {
    binaryPath = path.join(binDir, "rescript-tools.exe");
  } else if (binaryName === "rescript-editor-analysis.exe") {
    binaryPath = path.join(binDir, "rescript-editor-analysis.exe");
  }

  if (binaryPath != null && fs.existsSync(binaryPath)) {
    return binaryPath;
  }
  return null;
};

// ============================================================================
// Main Binary Finding Function (Routes to v12 or legacy)
// ============================================================================

/**
 * Finds a ReScript binary, routing to v12+ or legacy implementation.
 * Top-level if separates the two code paths completely.
 */
export const getBinaryPath = (
  binaryName: "rescript-editor-analysis.exe" | "rescript-tools.exe",
  projectRootPath: NormalizedPath | null = null,
): string | null => {
  const rescriptVersion = findReScriptVersion(projectRootPath);
  const isReScript12OrHigher =
    rescriptVersion != null &&
    semver.valid(rescriptVersion) &&
    semver.gte(rescriptVersion, "12.0.0");

  // Top-level separation: v12+ or legacy
  if (isReScript12OrHigher && projectRootPath != null) {
    return getBinaryPathReScript12(projectRootPath, binaryName);
  } else {
    return getBinaryPathLegacy(projectRootPath, binaryName);
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
