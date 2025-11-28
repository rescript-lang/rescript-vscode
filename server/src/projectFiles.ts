import * as cp from "node:child_process";
import * as p from "vscode-languageserver-protocol";
import { NormalizedPath, FileURI } from "./utils";

export type filesDiagnostics = {
  [key: FileURI]: p.Diagnostic[];
};

export interface projectFiles {
  openFiles: Set<NormalizedPath>;
  filesWithDiagnostics: Set<FileURI>;
  filesDiagnostics: filesDiagnostics;
  rescriptVersion: string | undefined;
  bscBinaryLocation: NormalizedPath | null;
  editorAnalysisLocation: string | null;
  namespaceName: string | null;

  bsbWatcherByEditor: null | cp.ChildProcess;

  // This keeps track of whether we've prompted the user to start a build
  // automatically, if there's no build currently running for the project. We
  // only want to prompt the user about this once, or it becomes
  // annoying.
  // The type `never` means that we won't show the prompt if the project is inside node_modules
  hasPromptedToStartBuild: boolean | "never";
}

/**
 * Map of project root paths to their project state.
 *
 * Keys are normalized paths (NormalizedPath) to ensure consistent lookups
 * and prevent path format mismatches. All paths should be normalized using
 * `normalizePath()` before being used as keys.
 */
export let projectsFiles: Map<NormalizedPath, projectFiles> = new Map();
