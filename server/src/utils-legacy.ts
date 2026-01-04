/**
 * Legacy binary finding for ReScript < 12.
 * This code is kept separate to avoid polluting the main utils with pre-v12 complexity.
 */

import * as fs from "fs";
import * as path from "path";
import * as c from "./constants";
import { normalizePath, NormalizedPath } from "./utils";

const fsAsync = fs.promises;

/**
 * Finds binaries for ReScript < 12 using the old path structure.
 * Checks compiler-info.json first, then falls back to node_modules/rescript/${platformDir}/.
 * NOTE: This preserves the original behavior exactly - do not add existence checks
 * to the compiler-info.json branch as the original code returned immediately.
 */
export let findBinaryLegacy = async (
  projectRootPath: NormalizedPath | null,
  rescriptDir: string,
  binary:
    | "bsc.exe"
    | "rescript-editor-analysis.exe"
    | "rescript"
    | "rewatch.exe"
    | "rescript.exe"
    | "rescript-tools.exe",
): Promise<NormalizedPath | null> => {
  // Check compiler-info.json first (original behavior: return immediately if found)
  if (projectRootPath !== null) {
    try {
      const compilerInfo = path.resolve(
        projectRootPath,
        c.compilerInfoPartialPath,
      );
      const contents = await fsAsync.readFile(compilerInfo, "utf8");
      const compileInfo = JSON.parse(contents);
      if (compileInfo && compileInfo.bsc_path) {
        const bsc_path = compileInfo.bsc_path;
        if (binary === "bsc.exe") {
          return normalizePath(bsc_path);
        } else {
          const binaryPath = path.join(path.dirname(bsc_path), binary);
          return normalizePath(binaryPath);
        }
      }
    } catch {}
  }

  // Fallback to old path structure (with existence check, as in original)
  const binaryPath = path.join(rescriptDir, c.platformDir, binary);
  if (fs.existsSync(binaryPath)) {
    return normalizePath(binaryPath);
  } else {
    return null;
  }
};
