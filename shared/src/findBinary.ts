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
const platformDir =
  process.arch === "arm64" ? process.platform + process.arch : process.platform;

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
        } else {
          const binaryPath = path.join(path.dirname(bscPath), binary);
          return normalizePath(binaryPath);
        }
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
    binaryPath = path.join(rescriptDir, platformDir, binary);
  }

  if (binaryPath != null && fs.existsSync(binaryPath)) {
    return normalizePath(binaryPath);
  }

  return null;
};
