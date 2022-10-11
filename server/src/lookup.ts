import * as fs from "fs";
import * as path from "path";
import * as p from "vscode-languageserver-protocol";

import { BuildSchema, ModuleFormat, ModuleFormatObject } from "./buildSchema";
import * as c from "./constants";

const getCompiledFolderName = (moduleFormat: ModuleFormat): string => {
  switch (moduleFormat) {
    case "es6":
      return "es6";
    case "es6-global":
      return "es6_global";
    case "commonjs":
    default:
      return "js";
  }
};

export const replaceFileExtension = (filePath: string, ext: string): string => {
  let name = path.basename(filePath, path.extname(filePath));
  return path.format({ dir: path.dirname(filePath), name, ext });
};

// Check if filePartialPath exists at directory and return the joined path,
// otherwise recursively check parent directories for it.
export const findFilePathFromProjectRoot = (
  directory: p.DocumentUri | null, // This must be a directory and not a file!
  filePartialPath: string
): null | p.DocumentUri => {
  if (directory == null) {
    return null;
  }

  let filePath: p.DocumentUri = path.join(directory, filePartialPath);
  if (fs.existsSync(filePath)) {
    return filePath;
  }

  let parentDir: p.DocumentUri = path.dirname(directory);
  if (parentDir === directory) {
    // reached the top
    return null;
  }

  return findFilePathFromProjectRoot(parentDir, filePartialPath);
};

export const readBsConfig = (projDir: p.DocumentUri): BuildSchema | null => {
  try {
    let bsconfigFile = fs.readFileSync(
      path.join(projDir, c.bsconfigPartialPath),
      { encoding: "utf-8" }
    );

    let result: BuildSchema = JSON.parse(bsconfigFile);
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
  projDir: string,
  partialFilePath: string
): string | null => {
  let bsconfig = readBsConfig(projDir);

  if (!bsconfig) {
    return null;
  }

  let [suffix, pathFragment] = getSuffixAndPathFragmentFromBsconfig(bsconfig);

  let compiledPartialPath = replaceFileExtension(partialFilePath, suffix);

  return path.join(projDir, pathFragment, compiledPartialPath);
};

// Monorepo helpers
export const getFilenameFromRootBsconfig = (
  projDir: string,
  partialFilePath: string
): string | null => {
  let rootBsConfigPath = findFilePathFromProjectRoot(
    path.join("..", projDir),
    c.bsconfigPartialPath
  );

  if (!rootBsConfigPath) {
    return null;
  }

  let rootBsconfig = readBsConfig(path.dirname(rootBsConfigPath));

  if (!rootBsconfig) {
    return null;
  }

  let [suffix, pathFragment] =
    getSuffixAndPathFragmentFromBsconfig(rootBsconfig);

  let compiledPartialPath = replaceFileExtension(partialFilePath, suffix);

  return path.join(projDir, pathFragment, compiledPartialPath);
};
