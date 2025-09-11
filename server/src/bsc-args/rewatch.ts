import * as path from "path";
import * as utils from "../utils";
import * as cp from "node:child_process";
import semver from "semver";
import {
  debug,
  IncrementallyCompiledFileInfo,
} from "../incrementalCompilation";
import type { projectFiles } from "../projectFiles";
import { findRuntime } from "../find-runtime";

export type RewatchCompilerArgs = {
  compiler_args: Array<string>;
  parser_args: Array<string>;
};

export async function getRewatchBscArgs(
  projectsFiles: Map<string, projectFiles>,
  entry: IncrementallyCompiledFileInfo,
): Promise<RewatchCompilerArgs | null> {
  const rewatchCacheEntry = entry.buildRewatch;

  if (
    rewatchCacheEntry != null &&
    rewatchCacheEntry.lastFile === entry.file.sourceFilePath
  ) {
    return Promise.resolve(rewatchCacheEntry.compilerArgs);
  }

  try {
    const project = projectsFiles.get(entry.project.rootPath);
    if (project?.rescriptVersion == null) return null;
    let rewatchPath = path.resolve(
      entry.project.workspaceRootPath,
      "node_modules/@rolandpeelen/rewatch/rewatch",
    );
    let rescriptRewatchPath = null;
    if (
      semver.valid(project.rescriptVersion) &&
      semver.satisfies(project.rescriptVersion as string, ">11", {
        includePrerelease: true,
      })
    ) {
      rescriptRewatchPath = await utils.findRewatchBinary(
        entry.project.workspaceRootPath,
      );
    }

    if (
      semver.valid(project.rescriptVersion) &&
      semver.satisfies(project.rescriptVersion as string, ">=12.0.0-beta.1", {
        includePrerelease: true,
      })
    ) {
      rescriptRewatchPath = await utils.findRescriptExeBinary(
        entry.project.workspaceRootPath,
      );
    }

    if (rescriptRewatchPath != null) {
      rewatchPath = rescriptRewatchPath;
      if (debug()) {
        console.log(
          `Found rewatch binary bundled with v12: ${rescriptRewatchPath}`,
        );
      }
    } else {
      if (debug()) {
        console.log("Did not find rewatch binary bundled with v12");
      }
    }

    const rewatchArguments = semver.satisfies(
      project.rescriptVersion,
      ">=12.0.0-beta.2",
      { includePrerelease: true },
    )
      ? ["compiler-args", entry.file.sourceFilePath]
      : [
          "--rescript-version",
          project.rescriptVersion,
          "--compiler-args",
          entry.file.sourceFilePath,
        ];
    const bscExe = await utils.findBscExeBinary(
      entry.project.workspaceRootPath,
    );
    const env = {};
    if (bscExe != null) {
      (env as any)["RESCRIPT_BSC_EXE"] = bscExe;
    }

    // TODO: We should check a potential configured value
    // Users should be able to provide this themselves if they like.
    const rescriptRuntimes = await findRuntime(entry.project.workspaceRootPath);

    if (debug()) {
      if (rescriptRuntimes.length === 0) {
        console.log(
          `Did not find @rescript/runtime directory for ${entry.project.workspaceRootPath}`,
        );
      } else if (rescriptRuntimes.length > 1) {
        console.warn(
          `Found multiple @rescript/runtime directories, using the first one as RESCRIPT_RUNTIME: ${rescriptRuntimes.join(", ")}`,
        );
      } else {
        console.log(
          `Found @rescript/runtime directory: ${rescriptRuntimes.join(", ")}`,
        );
      }
    }

    if (rescriptRuntimes.length > 0) {
      (env as any)["RESCRIPT_RUNTIME"] = rescriptRuntimes[0];
    } else {
      // TODO: if no runtime was found, we should let the user know
    }

    const compilerArgs = JSON.parse(
      cp.execFileSync(rewatchPath, rewatchArguments, { env }).toString().trim(),
    ) as RewatchCompilerArgs;

    entry.buildRewatch = {
      lastFile: entry.file.sourceFilePath,
      compilerArgs: compilerArgs,
    };

    return compilerArgs;
  } catch (e) {
    console.error(e);
    return null;
  }
}
