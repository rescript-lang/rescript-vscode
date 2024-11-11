import * as cp from "node:child_process";
import * as p from "vscode-languageserver-protocol";

export type filesDiagnostics = {
  [key: string]: p.Diagnostic[];
};

interface projectFiles {
  openFiles: Set<string>;
  filesWithDiagnostics: Set<string>;
  filesDiagnostics: filesDiagnostics;
  rescriptVersion: string | undefined;
  bscBinaryLocation: string | null;
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

export let projectsFiles: Map<string, projectFiles> = // project root path
  new Map();
