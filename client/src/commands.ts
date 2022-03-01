import { DiagnosticCollection } from "vscode";

import {
  DiagnosticsResultCodeActionsMap,
  runDeadCodeAnalysisWithReanalyze,
} from "./commands/dead_code_analysis";

export { createInterface } from "./commands/create_interface";
export { openCompiled } from "./commands/open_compiled";

export const deadCodeAnalysisWithReanalyze = (
  targetDir: string | null,
  diagnosticsCollection: DiagnosticCollection,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap
) => {
  runDeadCodeAnalysisWithReanalyze(
    targetDir,
    diagnosticsCollection,
    diagnosticsResultCodeActions
  );
};
