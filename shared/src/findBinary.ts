import * as fs from "fs";
import * as fsAsync from "fs/promises";
import * as path from "path";
import * as semver from "semver";

export type BinaryName =
  | "bsc.exe"
  | "rescript-editor-analysis.exe"
  | "rescript-tools.exe"
  | "rescript"
  | "rewatch.exe"
  | "rescript.exe";

type FindBinaryOptions = {
  projectRootPath: string | null;
  binary: BinaryName;
  platformPath?: string | null;
};

const compilerInfoPartialPath = path.join("lib", "bs", "compiler-info.json");
// For arm64, try the arm64-specific directory first (e.g., darwinarm64),
// then fall back to the generic platform directory (e.g., darwin) for older ReScript versions
const platformDirArm64 =
  process.arch === "arm64" ? process.platform + process.arch : null;
const platformDirGeneric = process.platform;

const normalizePath = (filePath: string | null): string | null => {
  return filePath != null ? path.normalize(filePath) : null;
};

const findFilePathFromProjectRoot = (
  directory: string | null,
  filePartialPath: string,
): string | null => {
  if (directory == null) {
    return null;
  }

  const filePath = path.join(directory, filePartialPath);
  if (fs.existsSync(filePath)) {
    return normalizePath(filePath);
  }

  const parentDirStr = path.dirname(directory);
  if (parentDirStr === directory) {
    return null;
  }

  return findFilePathFromProjectRoot(
    normalizePath(parentDirStr),
    filePartialPath,
  );
};

export const findBinary = async ({
  projectRootPath,
  binary,
  platformPath,
}: FindBinaryOptions): Promise<string | null> => {
  if (platformPath != null) {
    const result = path.join(platformPath, binary);
    return normalizePath(result);
  }

  if (projectRootPath !== null) {
    try {
      const compilerInfo = path.resolve(
        projectRootPath,
        compilerInfoPartialPath,
      );
      const contents = await fsAsync.readFile(compilerInfo, "utf8");
      const compileInfo = JSON.parse(contents);
      if (compileInfo && compileInfo.bsc_path) {
        const bscPath = compileInfo.bsc_path;
        if (binary === "bsc.exe") {
          return normalizePath(bscPath);
        } else if (binary !== "rescript") {
          // For native binaries (not "rescript" JS wrapper), use the bsc_path directory
          const binaryPath = path.join(path.dirname(bscPath), binary);
          return normalizePath(binaryPath);
        }
        // For "rescript", fall through to find the JS wrapper below
      }
    } catch {}
  }

  const rescriptDir = findFilePathFromProjectRoot(
    projectRootPath,
    path.join("node_modules", "rescript"),
  );
  if (rescriptDir == null) {
    return null;
  }

  let rescriptVersion = null;
  let rescriptJSWrapperPath = null;
  try {
    const rescriptPackageJSONPath = path.join(rescriptDir, "package.json");
    const rescriptPackageJSON = JSON.parse(
      await fsAsync.readFile(rescriptPackageJSONPath, "utf-8"),
    );
    rescriptVersion = rescriptPackageJSON.version;
    rescriptJSWrapperPath = rescriptPackageJSON.bin.rescript;
  } catch {
    return null;
  }

  let binaryPath: string | null = null;
  if (binary === "rescript") {
    binaryPath = path.join(rescriptDir, rescriptJSWrapperPath);
  } else if (semver.gte(rescriptVersion, "12.0.0-alpha.13")) {
    const target = `${process.platform}-${process.arch}`;
    const targetPackagePath = path.join(
      fs.realpathSync(rescriptDir),
      "..",
      `@rescript/${target}/bin.js`,
    );
    const { binPaths } = await import(targetPackagePath);

    if (binary === "bsc.exe") {
      binaryPath = binPaths.bsc_exe;
    } else if (binary === "rescript-editor-analysis.exe") {
      binaryPath = binPaths.rescript_editor_analysis_exe;
    } else if (binary === "rewatch.exe") {
      binaryPath = binPaths.rewatch_exe;
    } else if (binary === "rescript.exe") {
      binaryPath = binPaths.rescript_exe;
    }
  } else {
    // For older ReScript versions (< 12.0.0-alpha.13), try arm64-specific directory first,
    // then fall back to generic platform directory (older versions don't have arm64 directories)
    if (platformDirArm64 != null) {
      const arm64Path = path.join(rescriptDir, platformDirArm64, binary);
      if (fs.existsSync(arm64Path)) {
        binaryPath = arm64Path;
      }
    }
    if (binaryPath == null) {
      binaryPath = path.join(rescriptDir, platformDirGeneric, binary);
    }
  }

  if (binaryPath != null && fs.existsSync(binaryPath)) {
    return normalizePath(binaryPath);
  }

  return null;
};

/**
 * Derives the monorepo root directory from a binary path.
 * For a path like `/monorepo/node_modules/.bin/rescript`, returns `/monorepo`.
 * This is useful for monorepo support where the binary is in the monorepo root's
 * node_modules, but the project root (nearest rescript.json) might be a subpackage.
 */
export const getMonorepoRootFromBinaryPath = (
  binaryPath: string | null,
): string | null => {
  if (binaryPath == null) {
    return null;
  }
  const match = binaryPath.match(/^(.*?)[\\/]+node_modules[\\/]+/);
  return match ? normalizePath(match[1]) : null;
};
