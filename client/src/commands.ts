import { DiagnosticCollection, OutputChannel, StatusBarItem } from "vscode";

import {
  DiagnosticsResultCodeActionsMap,
  runCodeAnalysisWithReanalyze,
} from "./commands/code_analysis";

export { createInterface } from "./commands/create_interface";
export { openCompiled } from "./commands/open_compiled";
export { switchImplIntf } from "./commands/switch_impl_intf";
export { dumpDebug, dumpDebugRetrigger } from "./commands/dump_debug";
export { dumpServerState } from "./commands/dump_server_state";

export const codeAnalysisWithReanalyze = (
  targetDir: string | null,
  diagnosticsCollection: DiagnosticCollection,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap,
  outputChannel: OutputChannel,
  codeAnalysisRunningStatusBarItem: StatusBarItem,
) => {
  runCodeAnalysisWithReanalyze(
    targetDir,
    diagnosticsCollection,
    diagnosticsResultCodeActions,
    outputChannel,
    codeAnalysisRunningStatusBarItem,
  );
};
