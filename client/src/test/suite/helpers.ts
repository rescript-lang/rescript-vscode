/**
 * Shared test helpers for ReScript VSCode extension tests
 */
import * as assert from "assert";
import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";

/**
 * Get the workspace root folder path
 */
export function getWorkspaceRoot(): string {
  const workspaceFolders = vscode.workspace.workspaceFolders;
  if (workspaceFolders && workspaceFolders.length > 0) {
    return workspaceFolders[0].uri.fsPath;
  }
  return "";
}

/**
 * Remove ReScript 12+ lock file (lib/rescript.lock) and reanalyze socket file
 */
export function removeRescriptLockFile(workspaceRoot: string): void {
  const filesToRemove = [
    path.join(workspaceRoot, "lib", "rescript.lock"),
    // Also remove reanalyze socket file to ensure a fresh server is started
    path.join(workspaceRoot, ".rescript-reanalyze.sock"),
  ];

  for (const file of filesToRemove) {
    try {
      if (fs.existsSync(file)) {
        fs.unlinkSync(file);
        console.log(`Removed ${path.basename(file)}`);
      }
    } catch (e) {
      console.log(`Could not remove ${path.basename(file)}:`, e);
    }
  }
}

/**
 * Remove only the reanalyze socket file (not the lock file)
 * Use this when you need a fresh reanalyze server but want to keep the build watcher running
 */
export function removeReanalyzeSocketFile(workspaceRoot: string): void {
  const socketPath = path.join(workspaceRoot, ".rescript-reanalyze.sock");
  try {
    if (fs.existsSync(socketPath)) {
      fs.unlinkSync(socketPath);
      console.log("Removed .rescript-reanalyze.sock");
    }
  } catch (e) {
    console.log("Could not remove .rescript-reanalyze.sock:", e);
  }
}

/**
 * Remove ReScript 9/10/11 lock file (.bsb.lock)
 */
export function removeBsbLockFile(workspaceRoot: string): void {
  const lockPath = path.join(workspaceRoot, ".bsb.lock");
  try {
    if (fs.existsSync(lockPath)) {
      fs.unlinkSync(lockPath);
      console.log("Removed .bsb.lock");
    }
  } catch (e) {
    console.log("Could not remove .bsb.lock:", e);
  }
}

/**
 * Remove monorepo lock files (both rewatch.lock and rescript.lock)
 */
export function removeMonorepoLockFiles(monorepoRoot: string): void {
  const filesToRemove = [
    path.join(monorepoRoot, "lib", "rewatch.lock"),
    path.join(monorepoRoot, "lib", "rescript.lock"),
    // Also remove reanalyze socket file to ensure a fresh server is started
    path.join(monorepoRoot, ".rescript-reanalyze.sock"),
  ];

  for (const file of filesToRemove) {
    try {
      if (fs.existsSync(file)) {
        fs.unlinkSync(file);
        console.log(`Removed ${path.basename(file)}`);
      }
    } catch (e) {
      console.log(`Could not remove ${path.basename(file)}:`, e);
    }
  }
}

/**
 * Ensure the ReScript extension is activated
 */
export async function ensureExtensionActivated(): Promise<
  vscode.Extension<unknown> | undefined
> {
  const extension = vscode.extensions.getExtension(
    "chenglou92.rescript-vscode",
  );
  if (extension && !extension.isActive) {
    await extension.activate();
  }
  return extension;
}

/**
 * Open a file in the editor and return the document
 */
export async function openFile(filePath: string): Promise<vscode.TextDocument> {
  const document = await vscode.workspace.openTextDocument(filePath);
  await vscode.window.showTextDocument(document);
  return document;
}

/**
 * Find the LSP log file in the most recent test logs directory
 */
export function findLspLogContent(): string | null {
  // __dirname is client/out/client/src/test/suite when running tests
  const vscodeTestDir = path.resolve(__dirname, "../../../../../.vscode-test");
  const logsBaseDir = path.join(vscodeTestDir, "user-data", "logs");

  try {
    // Find the most recent log directory (format: YYYYMMDDTHHMMSS)
    const logDirs = fs
      .readdirSync(logsBaseDir)
      .filter((d) => /^\d{8}T\d{6}$/.test(d))
      .sort()
      .reverse();

    for (const logDir of logDirs) {
      const outputLoggingDir = path.join(
        logsBaseDir,
        logDir,
        "window1",
        "exthost",
      );
      if (!fs.existsSync(outputLoggingDir)) continue;

      const outputDirs = fs
        .readdirSync(outputLoggingDir)
        .filter((d) => d.startsWith("output_logging_"));

      for (const outputDir of outputDirs) {
        const lspLogPath = path.join(
          outputLoggingDir,
          outputDir,
          "1-ReScript Language Server.log",
        );
        if (fs.existsSync(lspLogPath)) {
          console.log("Checking log file:", lspLogPath);
          return fs.readFileSync(lspLogPath, "utf-8");
        }
      }
    }
  } catch (e) {
    console.log("Error reading logs:", e);
  }

  return null;
}

/**
 * Wait for a condition to become true, polling at intervals
 */
export async function waitFor(
  condition: () => boolean,
  options: { timeout?: number; interval?: number; message?: string } = {},
): Promise<boolean> {
  const { timeout = 5000, interval = 500, message = "condition" } = options;
  const maxAttempts = Math.ceil(timeout / interval);

  for (let i = 0; i < maxAttempts; i++) {
    await new Promise((resolve) => setTimeout(resolve, interval));
    const result = condition();
    console.log(`Checking ${message} (attempt ${i + 1}): ${result}`);
    if (result) return true;
  }
  return false;
}

/**
 * Sleep for a given number of milliseconds
 */
export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Get the path to the compiler.log file
 */
export function getCompilerLogPath(workspaceRoot: string): string {
  return path.join(workspaceRoot, "lib", "bs", ".compiler.log");
}

/**
 * Get the mtime of a file, or null if it doesn't exist
 */
export function getFileMtime(filePath: string): Date | null {
  try {
    return fs.statSync(filePath).mtime;
  } catch {
    return null;
  }
}

/**
 * Wait for a file's mtime to be updated (newer than the given mtime)
 */
export async function waitForFileUpdate(
  filePath: string,
  mtimeBefore: Date | null,
  options: { timeout?: number; interval?: number } = {},
): Promise<Date | null> {
  const { timeout = 5000, interval = 500 } = options;
  const maxAttempts = Math.ceil(timeout / interval);

  for (let i = 0; i < maxAttempts; i++) {
    await sleep(interval);
    const mtimeAfter = getFileMtime(filePath);
    if (mtimeAfter && (!mtimeBefore || mtimeAfter > mtimeBefore)) {
      return mtimeAfter;
    }
  }
  return getFileMtime(filePath);
}

/**
 * Insert a comment at the beginning of a document and save
 */
export async function insertCommentAndSave(
  editor: vscode.TextEditor,
  comment: string,
): Promise<void> {
  await editor.edit((editBuilder) => {
    editBuilder.insert(new vscode.Position(0, 0), comment);
  });
  await editor.document.save();
}

/**
 * Restore original content to a document and save
 */
export async function restoreContentAndSave(
  editor: vscode.TextEditor,
  originalContent: string,
): Promise<void> {
  const document = editor.document;
  await editor.edit((editBuilder) => {
    const fullRange = new vscode.Range(
      new vscode.Position(0, 0),
      document.lineAt(document.lineCount - 1).range.end,
    );
    editBuilder.replace(fullRange, originalContent);
  });
  await document.save();
}

/**
 * Start the build watcher and wait for it to initialize
 */
export async function startBuildWatcher(waitMs: number = 1000): Promise<void> {
  await vscode.commands.executeCommand("rescript-vscode.start_build");
  await sleep(waitMs);
}

/**
 * Start code analysis and wait for it to initialize
 */
export async function startCodeAnalysis(waitMs: number = 1000): Promise<void> {
  await vscode.commands.executeCommand("rescript-vscode.start_code_analysis");
  await sleep(waitMs);
}

/**
 * Stop code analysis
 */
export async function stopCodeAnalysis(): Promise<void> {
  await vscode.commands.executeCommand("rescript-vscode.stop_code_analysis");
}

/**
 * Show the reanalyze server log and assert it returns true
 */
export async function showReanalyzeServerLog(): Promise<void> {
  const result = await vscode.commands.executeCommand<boolean>(
    "rescript-vscode.show_reanalyze_server_log",
  );
  console.log(`Show reanalyze server log result: ${result}`);
  assert.strictEqual(
    result,
    true,
    "Show reanalyze server log should return true when output channel is shown",
  );
}

/**
 * Result of searching for build prompt in logs
 */
export interface BuildPromptResult {
  found: boolean;
  path: string;
}

/**
 * Find "Prompting to start build" message in LSP logs
 */
export function findBuildPromptInLogs(): BuildPromptResult {
  const logContent = findLspLogContent();
  if (logContent) {
    const promptMatch = logContent.match(
      /\[Info.*\] Prompting to start build for (.+)/,
    );
    if (promptMatch) {
      return { found: true, path: promptMatch[1] };
    }
  }
  return { found: false, path: "" };
}
