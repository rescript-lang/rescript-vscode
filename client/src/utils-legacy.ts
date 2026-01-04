/**
 * Legacy binary finding for ReScript < 12.
 * This code is kept separate to avoid polluting the main utils with pre-v12 complexity.
 */

import * as fs from "fs";
import * as path from "path";
import { NormalizedPath } from "./utils";

type binaryName = "rescript-editor-analysis.exe" | "rescript-tools.exe";

// Legacy format: no hyphen (e.g., "darwinarm64")
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

/**
 * Finds binaries for ReScript < 12 using old path structure.
 * Tries project binary first, then falls back to builtin binaries.
 */
export const getBinaryPathLegacy = (
  projectRootPath: NormalizedPath | null,
  binaryName: binaryName,
): string | null => {
  // Try project binary first
  if (projectRootPath != null) {
    const binaryFromCompilerPackage = path.join(
      projectRootPath,
      "node_modules",
      "rescript",
      platformDir,
      binaryName,
    );
    if (fs.existsSync(binaryFromCompilerPackage)) {
      return binaryFromCompilerPackage;
    }
  }

  // Fall back to builtin binaries
  if (fs.existsSync(getLegacyBinaryDevPath(binaryName))) {
    return getLegacyBinaryDevPath(binaryName);
  } else if (fs.existsSync(getLegacyBinaryProdPath(binaryName))) {
    return getLegacyBinaryProdPath(binaryName);
  }

  return null;
};
