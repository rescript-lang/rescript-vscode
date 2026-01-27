import * as cp from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as semver from "semver";
import {
  window,
  DiagnosticCollection,
  Diagnostic,
  Range,
  Position,
  DiagnosticSeverity,
  Uri,
  CodeAction,
  CodeActionKind,
  WorkspaceEdit,
  OutputChannel,
  StatusBarItem,
} from "vscode";
import { NormalizedPath, normalizePath } from "../utils";
import {
  findBinary,
  getMonorepoRootFromBinaryPath,
} from "../../../shared/src/findBinary";
import { findProjectRootOfFile } from "../../../shared/src/projectRoots";

// Reanalyze server constants (matches rescript monorepo)
const REANALYZE_SOCKET_FILENAME = ".rescript-reanalyze.sock";
const REANALYZE_SERVER_MIN_VERSION = "12.1.0";

// Server state per monorepo root
export interface ReanalyzeServerState {
  process: cp.ChildProcess | null;
  monorepoRoot: string;
  socketPath: string;
  startedByUs: boolean;
  outputChannel: OutputChannel | null;
}

// Map from monorepo root to server state
export const reanalyzeServers: Map<string, ReanalyzeServerState> = new Map();

// Check if ReScript version supports reanalyze-server
const supportsReanalyzeServer = async (
  monorepoRootPath: string | null,
): Promise<boolean> => {
  if (monorepoRootPath === null) return false;

  try {
    const rescriptDir = path.join(monorepoRootPath, "node_modules", "rescript");
    const packageJsonPath = path.join(rescriptDir, "package.json");
    const packageJson = JSON.parse(
      await fs.promises.readFile(packageJsonPath, "utf-8"),
    );
    const version = packageJson.version;

    return (
      semver.valid(version) != null &&
      semver.gte(version, REANALYZE_SERVER_MIN_VERSION)
    );
  } catch {
    return false;
  }
};

// Get socket path for a monorepo root
const getSocketPath = (monorepoRoot: string): string => {
  return path.join(monorepoRoot, REANALYZE_SOCKET_FILENAME);
};

// Check if server is running (socket file exists)
const isServerRunning = (monorepoRoot: string): boolean => {
  const socketPath = getSocketPath(monorepoRoot);
  return fs.existsSync(socketPath);
};

// Start reanalyze server for a monorepo.
// Note: This should only be called after supportsReanalyzeServer() returns true,
// which ensures ReScript >= 12.1.0 where the reanalyze-server subcommand exists.
export const startReanalyzeServer = async (
  monorepoRoot: string,
  binaryPath: string,
  clientOutputChannel?: OutputChannel,
): Promise<ReanalyzeServerState | null> => {
  // Check if already running (either by us or externally)
  if (isServerRunning(monorepoRoot)) {
    // Check if we have a record of starting it
    const existing = reanalyzeServers.get(monorepoRoot);
    if (existing) {
      existing.outputChannel?.appendLine(
        "[info] Server already running (started by us)",
      );
      return existing;
    }
    // Server running but not started by us - just record it
    clientOutputChannel?.appendLine(
      `[info] Found existing reanalyze-server for ${path.basename(monorepoRoot)} (not started by extension)`,
    );
    const state: ReanalyzeServerState = {
      process: null,
      monorepoRoot,
      socketPath: getSocketPath(monorepoRoot),
      startedByUs: false,
      outputChannel: null,
    };
    reanalyzeServers.set(monorepoRoot, state);
    return state;
  }

  // Create output channel for server logs
  const outputChannel = window.createOutputChannel(
    `ReScript Reanalyze Server (${path.basename(monorepoRoot)})`,
  );

  outputChannel.appendLine(
    `[info] Starting reanalyze-server in ${monorepoRoot}`,
  );

  // Start the server
  const serverProcess = cp.spawn(binaryPath, ["reanalyze-server"], {
    cwd: monorepoRoot,
    stdio: ["ignore", "pipe", "pipe"],
  });

  if (serverProcess.pid == null) {
    outputChannel.appendLine("[error] Failed to start reanalyze-server");
    return null;
  }

  const state: ReanalyzeServerState = {
    process: serverProcess,
    monorepoRoot,
    socketPath: getSocketPath(monorepoRoot),
    startedByUs: true,
    outputChannel,
  };

  // Log stdout and stderr to output channel
  serverProcess.stdout?.on("data", (data) => {
    outputChannel.appendLine(`[stdout] ${data.toString().trim()}`);
  });

  serverProcess.stderr?.on("data", (data) => {
    outputChannel.appendLine(`[stderr] ${data.toString().trim()}`);
  });

  serverProcess.on("error", (err) => {
    outputChannel.appendLine(`[error] Server error: ${err.message}`);
  });

  serverProcess.on("exit", (code, signal) => {
    outputChannel.appendLine(
      `[info] Server exited with code ${code}, signal ${signal}`,
    );
    reanalyzeServers.delete(monorepoRoot);
  });

  reanalyzeServers.set(monorepoRoot, state);

  // Wait briefly for socket file to be created (up to 3 seconds)
  for (let i = 0; i < 30; i++) {
    if (isServerRunning(monorepoRoot)) {
      outputChannel.appendLine(`[info] Server socket ready`);
      return state;
    }
    await new Promise((resolve) => setTimeout(resolve, 100));
  }

  outputChannel.appendLine(
    "[warn] Server started but socket not found after 3 seconds",
  );
  return state;
};

// Clean up socket file if it exists
const cleanupSocketFile = (socketPath: string): void => {
  try {
    if (fs.existsSync(socketPath)) {
      fs.unlinkSync(socketPath);
    }
  } catch {
    // Ignore errors during cleanup
  }
};

// Stop reanalyze server for a monorepo (only if we started it)
export const stopReanalyzeServer = (
  monorepoRoot: string | null,
  clientOutputChannel?: OutputChannel,
): void => {
  if (monorepoRoot == null) return;

  const state = reanalyzeServers.get(monorepoRoot);
  if (!state) return;

  // Only kill the process if we started it
  if (state.startedByUs && state.process != null) {
    state.process.kill();
    state.outputChannel?.appendLine("[info] Server stopped by extension");
    // Clean up socket file to prevent stale socket issues
    cleanupSocketFile(state.socketPath);
  } else if (!state.startedByUs) {
    clientOutputChannel?.appendLine(
      `[info] Leaving external reanalyze-server running for ${path.basename(monorepoRoot)}`,
    );
  }

  reanalyzeServers.delete(monorepoRoot);
};

// Stop all servers we started
export const stopAllReanalyzeServers = (): void => {
  for (const [_monorepoRoot, state] of reanalyzeServers) {
    if (state.startedByUs && state.process != null) {
      state.process.kill();
      state.outputChannel?.appendLine("[info] Server stopped by extension");
      // Clean up socket file to prevent stale socket issues
      cleanupSocketFile(state.socketPath);
    }
  }
  reanalyzeServers.clear();
};

// Show server log for a monorepo
// Returns true if the output channel was shown, false otherwise
// This is an async function because it may need to find the binary to derive monorepo root
export const showReanalyzeServerLog = async (
  monorepoRoot: string | null,
): Promise<boolean> => {
  if (monorepoRoot == null) {
    // Try to find any running server
    const firstServer = reanalyzeServers.values().next().value;
    if (firstServer?.outputChannel) {
      firstServer.outputChannel.show();
      return true;
    } else {
      window.showInformationMessage(
        "No reanalyze server is currently running.",
      );
      return false;
    }
  }

  // First try direct lookup
  let state = reanalyzeServers.get(monorepoRoot);
  if (state?.outputChannel) {
    state.outputChannel.show();
    return true;
  }

  // If not found, try to derive monorepo root from binary path
  // (the server is registered under monorepo root, not subpackage root)
  const binaryPath = await findBinary({
    projectRootPath: monorepoRoot,
    binary: "rescript-tools.exe",
  });
  if (binaryPath != null) {
    const derivedMonorepoRoot = getMonorepoRootFromBinaryPath(binaryPath);
    if (derivedMonorepoRoot != null && derivedMonorepoRoot !== monorepoRoot) {
      state = reanalyzeServers.get(derivedMonorepoRoot);
      if (state?.outputChannel) {
        state.outputChannel.show();
        return true;
      }
    }
  }

  window.showInformationMessage(
    `No reanalyze server log available for ${path.basename(monorepoRoot)}`,
  );
  return false;
};

export let statusBarItem = {
  setToStopText: (codeAnalysisRunningStatusBarItem: StatusBarItem) => {
    codeAnalysisRunningStatusBarItem.text = "$(debug-stop) Stop Code Analyzer";
    codeAnalysisRunningStatusBarItem.tooltip = null;
  },
  setToRunningText: (codeAnalysisRunningStatusBarItem: StatusBarItem) => {
    codeAnalysisRunningStatusBarItem.text =
      "$(loading~spin) Running code analysis...";
    codeAnalysisRunningStatusBarItem.tooltip = null;
  },
  setToFailed: (codeAnalysisRunningStatusBarItem: StatusBarItem) => {
    codeAnalysisRunningStatusBarItem.text = "$(alert) Failed";
    codeAnalysisRunningStatusBarItem.tooltip =
      "Something went wrong when running the code analysis.";
  },
};

export type DiagnosticsResultCodeActionsMap = Map<
  string,
  { range: Range; codeAction: CodeAction }[]
>;

export type DiagnosticsResultFormat = Array<{
  name: string;
  kind: string;
  file: string;
  range: [number, number, number, number];
  message: string;
  annotate?: {
    line: number;
    character: number;
    text: string;
    action: string;
  };
}>;

enum ClassifiedMessage {
  Removable,
  Default,
}

let classifyMessage = (msg: string) => {
  if (
    msg.endsWith(" is never used") ||
    msg.endsWith(" is never used and could have side effects") ||
    msg.endsWith(" has no side effects and can be removed")
  ) {
    return ClassifiedMessage.Removable;
  }

  return ClassifiedMessage.Default;
};

let resultsToDiagnostics = (
  results: DiagnosticsResultFormat,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap,
): {
  diagnosticsMap: Map<string, Diagnostic[]>;
} => {
  let diagnosticsMap: Map<string, Diagnostic[]> = new Map();

  results.forEach((item) => {
    {
      let startPos: Position, endPos: Position;
      let [startLine, startCharacter, endLine, endCharacter] = item.range;

      // Detect if this diagnostic is for the entire file. If so, reanalyze will
      // say that the issue is on line -1. This code below ensures
      // that the full file is highlighted, if that's the case.
      if (startLine < 0 || endLine < 0) {
        startPos = new Position(0, 0);
        endPos = new Position(99999, 0);
      } else {
        startPos = new Position(startLine, startCharacter);
        endPos = new Position(endLine, endCharacter);
      }

      let issueLocationRange = new Range(startPos, endPos);
      let diagnosticText = item.message.trim();

      let diagnostic = new Diagnostic(
        issueLocationRange,
        diagnosticText,
        DiagnosticSeverity.Warning,
      );

      // Don't show reports about optional arguments.
      if (item.name.toLowerCase().includes("unused argument")) {
        return;
      }

      if (diagnosticsMap.has(item.file)) {
        diagnosticsMap.get(item.file).push(diagnostic);
      } else {
        diagnosticsMap.set(item.file, [diagnostic]);
      }

      // If reanalyze suggests a fix, we'll set that up as a refactor code
      // action in VSCode. This way, it'll be easy to suppress the issue
      // reported if wanted. We also save the range of the issue, so we can
      // leverage that to make looking up the code actions for each cursor
      // position very cheap.
      if (item.annotate != null) {
        {
          let { line, character, text, action } = item.annotate;
          let codeAction = new CodeAction(action);
          codeAction.kind = CodeActionKind.RefactorRewrite;

          let codeActionEdit = new WorkspaceEdit();

          codeActionEdit.replace(
            Uri.parse(item.file),
            // Make sure the full line is replaced

            new Range(
              new Position(line, character),
              new Position(line, character),
            ),
            // reanalyze seems to add two extra spaces at the start of the line
            // content to replace.
            text,
          );

          codeAction.edit = codeActionEdit;

          if (diagnosticsResultCodeActions.has(item.file)) {
            diagnosticsResultCodeActions
              .get(item.file)
              .push({ range: issueLocationRange, codeAction });
          } else {
            diagnosticsResultCodeActions.set(item.file, [
              { range: issueLocationRange, codeAction },
            ]);
          }
        }
      }

      // This heuristic below helps only target dead code that can be removed
      // safely by just removing its text.
      if (classifyMessage(item.message) === ClassifiedMessage.Removable) {
        {
          let codeAction = new CodeAction("Remove unused");
          codeAction.kind = CodeActionKind.RefactorRewrite;

          let codeActionEdit = new WorkspaceEdit();

          codeActionEdit.replace(
            Uri.parse(item.file),
            new Range(
              new Position(item.range[0], item.range[1]),
              new Position(item.range[2], item.range[3]),
            ),
            "",
          );

          codeAction.command = {
            command: "rescript-vscode.clear_diagnostic",
            title: "Clear diagnostic",
            arguments: [diagnostic],
          };

          codeAction.edit = codeActionEdit;

          if (diagnosticsResultCodeActions.has(item.file)) {
            diagnosticsResultCodeActions
              .get(item.file)
              .push({ range: issueLocationRange, codeAction });
          } else {
            diagnosticsResultCodeActions.set(item.file, [
              { range: issueLocationRange, codeAction },
            ]);
          }
        }
      }
    }
  });

  return {
    diagnosticsMap,
  };
};

// Returns the monorepo root path if a reanalyze server was started, null otherwise.
// This allows the caller to track which server to stop later.
export const runCodeAnalysisWithReanalyze = async (
  diagnosticsCollection: DiagnosticCollection,
  diagnosticsResultCodeActions: DiagnosticsResultCodeActionsMap,
  outputChannel: OutputChannel,
  codeAnalysisRunningStatusBarItem: StatusBarItem,
): Promise<string | null> => {
  let currentDocument = window.activeTextEditor?.document;
  if (!currentDocument) {
    window.showErrorMessage("No active document found.");
    return null;
  }

  let projectRootPath: NormalizedPath | null = normalizePath(
    findProjectRootOfFile(currentDocument.uri.fsPath),
  );

  // findBinary walks up the directory tree to find node_modules/rescript,
  // so it works correctly for monorepos (finds the workspace root's binary)
  // Note: rescript-tools.exe (with reanalyze command) is only available in ReScript 12+
  const binaryPath: string | null = await findBinary({
    projectRootPath,
    binary: "rescript-tools.exe",
  });

  if (binaryPath === null) {
    outputChannel.appendLine(
      `[error] rescript-tools.exe not found for project root: ${projectRootPath}. Code analysis requires ReScript 12 or later.`,
    );
    window.showErrorMessage(
      "Code analysis requires ReScript 12 or later (rescript-tools.exe not found).",
    );
    return null;
  }

  // Derive monorepo root from binary path - the directory containing node_modules
  // This handles monorepos correctly since findBinary walks up to find the binary
  const monorepoRootPath: NormalizedPath | null = normalizePath(
    getMonorepoRootFromBinaryPath(binaryPath),
  );

  if (monorepoRootPath === null) {
    outputChannel.appendLine(
      `[error] Could not determine workspace root from binary path: ${binaryPath}`,
    );
    window.showErrorMessage("Could not determine workspace root.");
    return null;
  }

  // Check if we should use reanalyze-server (ReScript >= 12.1.0)
  const useServer = await supportsReanalyzeServer(monorepoRootPath);

  if (useServer && monorepoRootPath) {
    // Ensure server is running from workspace root
    const serverState = await startReanalyzeServer(
      monorepoRootPath,
      binaryPath,
      outputChannel,
    );
    if (serverState) {
      outputChannel.appendLine(
        `[info] Using reanalyze-server for ${path.basename(monorepoRootPath)}`,
      );
    }
  }

  statusBarItem.setToRunningText(codeAnalysisRunningStatusBarItem);

  let opts = ["reanalyze", "-json"];
  let p = cp.spawn(binaryPath, opts, { cwd: monorepoRootPath });

  if (p.stdout == null) {
    outputChannel.appendLine(
      `[error] Failed to spawn reanalyze process: stdout is null. Binary: ${binaryPath}, cwd: ${monorepoRootPath}`,
    );
    statusBarItem.setToFailed(codeAnalysisRunningStatusBarItem);
    window.showErrorMessage("Failed to start code analysis process.");
    return null;
  }

  let data = "";

  p.stdout.on("data", (d) => {
    data += d;
  });

  p.stderr?.on("data", (e) => {
    // Sometimes the compiler artifacts has been corrupted in some way, and
    // reanalyze will spit out a "End_of_file" exception. The solution is to
    // clean and rebuild the ReScript project, which we can tell the user about
    // here.
    if (e.includes("End_of_file")) {
      window.showErrorMessage(
        `Something went wrong trying to run reanalyze. Please try cleaning and rebuilding your ReScript project.`,
      );
    } else {
      window.showErrorMessage(
        `Something went wrong trying to run reanalyze: '${e}'`,
      );
    }
  });

  p.on("close", () => {
    diagnosticsResultCodeActions.clear();

    let json: DiagnosticsResultFormat | null = null;

    try {
      json = JSON.parse(data);
    } catch (e) {
      window
        .showErrorMessage(
          `Something went wrong when running the code analyzer.`,
          "See details in error log",
        )
        .then((_choice) => {
          outputChannel.show();
        });

      outputChannel.appendLine("\n\n>>>>");
      outputChannel.appendLine(
        "Parsing JSON from reanalyze failed. The raw, invalid JSON can be reproduced by following the instructions below. Please run that command and report the issue + failing JSON on the extension bug tracker: https://github.com/rescript-lang/rescript-vscode/issues",
      );
      outputChannel.appendLine(
        `> To reproduce, run "${binaryPath} ${opts.join(
          " ",
        )}" in directory: "${monorepoRootPath}"`,
      );
      outputChannel.appendLine("\n");
    }

    if (json == null) {
      // If reanalyze failed for some reason we'll clear the diagnostics.
      diagnosticsCollection.clear();
      statusBarItem.setToFailed(codeAnalysisRunningStatusBarItem);
      return;
    }

    let { diagnosticsMap } = resultsToDiagnostics(
      json,
      diagnosticsResultCodeActions,
    );

    // This smoothens the experience of the diagnostics updating a bit by
    // clearing only the visible diagnostics that has been fixed after the
    // updated diagnostics has been applied.
    diagnosticsCollection.forEach((uri, _) => {
      if (!diagnosticsMap.has(uri.fsPath)) {
        diagnosticsCollection.delete(uri);
      }
    });

    diagnosticsMap.forEach((diagnostics, filePath) => {
      diagnosticsCollection.set(Uri.parse(filePath), diagnostics);
    });

    statusBarItem.setToStopText(codeAnalysisRunningStatusBarItem);
  });

  // Return the monorepo root so the caller can track which server to stop
  return monorepoRootPath;
};
