import * as fs from "fs";
import * as path from "path";

import { BuildSchema, ModuleFormat, ModuleFormatObject } from "./buildSchema";
import * as c from "./constants";
import { NormalizedPath, normalizePath } from "./utils";

const getCompiledFolderName = (moduleFormat: ModuleFormat): string => {
  switch (moduleFormat) {
    case "esmodule":
    case "es6":
      return "es6";
    case "es6-global":
      return "es6_global";
    case "commonjs":
    default:
      return "js";
  }
};

export const replaceFileExtension = <T extends string>(
  filePath: T,
  ext: string,
): T => {
  let name = path.basename(filePath, path.extname(filePath));
  const result = path.format({ dir: path.dirname(filePath), name, ext });
  // If input was NormalizedPath, result is still normalized
  // If input was string, result is string
  return result as T;
};

// Check if filePartialPath exists at directory and return the joined path,
// otherwise recursively check parent directories for it.
export const findFilePathFromProjectRoot = (
  directory: NormalizedPath | null, // This must be a directory and not a file!
  filePartialPath: string,
): NormalizedPath | null => {
  if (directory == null) {
    return null;
  }

  let filePath = path.join(directory, filePartialPath);
  if (fs.existsSync(filePath)) {
    return normalizePath(filePath);
  }

  let parentDirStr = path.dirname(directory);
  if (parentDirStr === directory) {
    // reached the top
    return null;
  }

  const parentDir = normalizePath(parentDirStr);
  if (parentDir == null) {
    return null;
  }

  return findFilePathFromProjectRoot(parentDir, filePartialPath);
};

export const readConfig = (projDir: NormalizedPath): BuildSchema | null => {
  try {
    let rescriptJson = path.join(projDir, c.rescriptJsonPartialPath);
    let bsconfigJson = path.join(projDir, c.bsconfigPartialPath);

    let configFile = fs.readFileSync(
      fs.existsSync(rescriptJson) ? rescriptJson : bsconfigJson,
      { encoding: "utf-8" },
    );

    let result: BuildSchema = JSON.parse(configFile);
    return result;
  } catch (e) {
    return null;
  }
};

// Collect data from bsconfig to be able to find out the correct path of
// the compiled JS artifacts.
export const getSuffixAndPathFragmentFromBsconfig = (bsconfig: BuildSchema) => {
  let pkgSpecs = bsconfig["package-specs"];
  let pathFragment = "";
  let module = c.bsconfigModuleDefault;
  let moduleFormatObj: ModuleFormatObject = { module: module };
  let suffix = c.bsconfigSuffixDefault;

  if (pkgSpecs) {
    if (
      !Array.isArray(pkgSpecs) &&
      typeof pkgSpecs !== "string" &&
      pkgSpecs.module
    ) {
      moduleFormatObj = pkgSpecs;
    } else if (typeof pkgSpecs === "string") {
      module = pkgSpecs;
    } else if (Array.isArray(pkgSpecs) && pkgSpecs[0]) {
      if (typeof pkgSpecs[0] === "string") {
        module = pkgSpecs[0];
      } else {
        moduleFormatObj = pkgSpecs[0];
      }
    }
  }

  if (moduleFormatObj["module"]) {
    module = moduleFormatObj["module"];
  }

  if (!moduleFormatObj["in-source"]) {
    pathFragment = "lib/" + getCompiledFolderName(module);
  }

  if (moduleFormatObj.suffix) {
    suffix = moduleFormatObj.suffix;
  } else if (bsconfig.suffix) {
    suffix = bsconfig.suffix;
  }

  return [suffix, pathFragment];
};

export const getFilenameFromBsconfig = (
  projDir: NormalizedPath,
  partialFilePath: string,
): NormalizedPath | null => {
  let bsconfig = readConfig(projDir);

  if (!bsconfig) {
    return null;
  }

  let [suffix, pathFragment] = getSuffixAndPathFragmentFromBsconfig(bsconfig);

  let compiledPartialPath = replaceFileExtension(partialFilePath, suffix);

  const result = path.join(projDir, pathFragment, compiledPartialPath);
  return normalizePath(result);
};

// Monorepo helpers
export const getFilenameFromRootBsconfig = (
  projDir: NormalizedPath,
  partialFilePath: string,
): NormalizedPath | null => {
  // Start searching from the parent directory of projDir to find the workspace root
  const parentDir = normalizePath(path.dirname(projDir));
  if (parentDir == null) {
    return null;
  }

  let rootConfigPath = findFilePathFromProjectRoot(
    parentDir,
    c.rescriptJsonPartialPath,
  );

  if (!rootConfigPath) {
    rootConfigPath = findFilePathFromProjectRoot(
      parentDir,
      c.bsconfigPartialPath,
    );
  }

  if (!rootConfigPath) {
    return null;
  }

  const rootConfigDir = normalizePath(path.dirname(rootConfigPath));
  if (rootConfigDir == null) {
    return null;
  }
  let rootConfig = readConfig(rootConfigDir);

  if (!rootConfig) {
    return null;
  }

  let [suffix, pathFragment] = getSuffixAndPathFragmentFromBsconfig(rootConfig);

  let compiledPartialPath = replaceFileExtension(partialFilePath, suffix);

  const result = path.join(projDir, pathFragment, compiledPartialPath);
  return normalizePath(result);
};
