import { DiagnosticCollection, OutputChannel } from "vscode";

import {
  DiagnosticsResultCodeActionsMap,
  runCodeAnalysisWithReanalyze,
} from "./commands/code_analysis";

export { createInterface } from "./commands/create_interface";
export { extractDocs } from "./commands/extract_docs";
export { openCompiled } from "./commands/open_compiled";
export { switchImplIntf } from "./commands/switch_impl_intf";

export const codeAnalysisWithReanalyze = (
  targetDir: string | null,
  diagnosticsCollection: DiagnosticCollection,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap,
  outputChannel: OutputChannel
) => {
  runCodeAnalysisWithReanalyze(
    targetDir,
    diagnosticsCollection,
    diagnosticsResultCodeActions,
    outputChannel
  );
};
