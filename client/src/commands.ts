import { DiagnosticCollection, OutputChannel, StatusBarItem } from "vscode";

import {
  DiagnosticsResultCodeActionsMap,
  runCodeAnalysisWithReanalyze,
} from "./commands/code_analysis";

export { createInterface } from "./commands/create_interface";
export { openCompiled } from "./commands/open_compiled";
export { switchImplIntf } from "./commands/switch_impl_intf";
export { dumpDebug, dumpDebugRetrigger } from "./commands/dump_debug";
export { pasteAsRescriptJson } from "./commands/paste_as_rescript_json";
export { pasteAsRescriptJsx } from "./commands/paste_as_rescript_jsx";

export const codeAnalysisWithReanalyze = (
  diagnosticsCollection: DiagnosticCollection,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap,
  outputChannel: OutputChannel,
  codeAnalysisRunningStatusBarItem: StatusBarItem,
) => {
  runCodeAnalysisWithReanalyze(
    diagnosticsCollection,
    diagnosticsResultCodeActions,
    outputChannel,
    codeAnalysisRunningStatusBarItem,
  );
};
