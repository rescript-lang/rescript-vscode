import { DiagnosticCollection, OutputChannel, StatusBarItem } from "vscode";

import {
  DiagnosticsResultCodeActionsMap,
  runCodeAnalysisWithReanalyze,
  reanalyzeServers,
  stopReanalyzeServer,
  stopAllReanalyzeServers,
  showReanalyzeServerLog,
} from "./commands/code_analysis";

export {
  reanalyzeServers,
  stopReanalyzeServer,
  stopAllReanalyzeServers,
  showReanalyzeServerLog,
};

export { createInterface } from "./commands/create_interface";
export { openCompiled } from "./commands/open_compiled";
export { switchImplIntf } from "./commands/switch_impl_intf";
export { dumpDebug, dumpDebugRetrigger } from "./commands/dump_debug";
export { pasteAsRescriptJson } from "./commands/paste_as_rescript_json";
export { pasteAsRescriptJsx } from "./commands/paste_as_rescript_jsx";

// Returns the monorepo root path if a reanalyze server was started, null otherwise.
export const codeAnalysisWithReanalyze = (
  diagnosticsCollection: DiagnosticCollection,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap,
  outputChannel: OutputChannel,
  codeAnalysisRunningStatusBarItem: StatusBarItem,
): Promise<string | null> => {
  return runCodeAnalysisWithReanalyze(
    diagnosticsCollection,
    diagnosticsResultCodeActions,
    outputChannel,
    codeAnalysisRunningStatusBarItem,
  );
};
