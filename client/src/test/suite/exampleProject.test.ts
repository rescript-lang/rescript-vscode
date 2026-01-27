/**
 * Example Project Tests (Code Analysis Server Test Suite)
 *
 * Run these tests:
 *   cd client && npm run test -- --label example-project
 */
import * as assert from "assert";
import * as path from "path";
import * as vscode from "vscode";
import {
  getWorkspaceRoot,
  removeRescriptLockFile,
  removeReanalyzeSocketFile,
  ensureExtensionActivated,
  sleep,
  getCompilerLogPath,
  getFileMtime,
  waitForFileUpdate,
  insertCommentAndSave,
  restoreContentAndSave,
  startBuildWatcher,
  startCodeAnalysis,
  stopCodeAnalysis,
  showReanalyzeServerLog,
  findBuildPromptInLogs,
} from "./helpers";

suite("Code Analysis Server Test Suite", () => {
  test("Extension should be present", async () => {
    const extension = vscode.extensions.getExtension(
      "chenglou92.rescript-vscode",
    );
    assert.ok(extension, "ReScript extension should be present");
    console.log("Extension found:", extension.id);
  });

  test("Commands should be registered after activation", async () => {
    const extension = vscode.extensions.getExtension(
      "chenglou92.rescript-vscode",
    );
    if (!extension) {
      console.log("Extension not found, skipping command test");
      return;
    }

    if (!extension.isActive) {
      console.log("Activating extension...");
      await extension.activate();
    }

    const commands = await vscode.commands.getCommands(true);

    const expectedCommands = [
      "rescript-vscode.start_code_analysis",
      "rescript-vscode.stop_code_analysis",
      "rescript-vscode.show_reanalyze_server_log",
      "rescript-vscode.start_build",
    ];

    for (const cmd of expectedCommands) {
      assert.ok(commands.includes(cmd), `Command ${cmd} should be registered`);
    }
    console.log("All commands registered successfully!");
  });

  test("Start Code Analysis should run on a ReScript file", async () => {
    const workspaceRoot = getWorkspaceRoot();
    if (!workspaceRoot) {
      console.log("No workspace folder found, skipping test");
      return;
    }
    console.log("Workspace root:", workspaceRoot);

    const resFilePath = path.join(workspaceRoot, "src", "Hello.res");
    console.log("Opening file:", resFilePath);

    const document = await vscode.workspace.openTextDocument(resFilePath);
    await vscode.window.showTextDocument(document);
    console.log("File opened successfully");

    await ensureExtensionActivated();

    console.log("Running start_code_analysis command...");
    await startCodeAnalysis();
    console.log("Code analysis command completed");

    console.log("Running stop_code_analysis command...");
    await stopCodeAnalysis();
    console.log("Test completed successfully");
  });

  test("Start Build command should start build watcher", async () => {
    const workspaceRoot = getWorkspaceRoot();
    if (!workspaceRoot) {
      console.log("No workspace folder found, skipping test");
      return;
    }
    console.log("Workspace root:", workspaceRoot);

    const resFilePath = path.join(workspaceRoot, "src", "Hello.res");
    console.log("Opening file:", resFilePath);

    const document = await vscode.workspace.openTextDocument(resFilePath);
    await vscode.window.showTextDocument(document);
    console.log("File opened successfully");

    await ensureExtensionActivated();

    console.log("Running start_build command...");
    await startBuildWatcher();

    console.log(
      "Test completed - check Language Server log for 'Starting build watcher' or 'Build watcher already running' message",
    );
  });

  test("Build watcher recompiles on file save", async () => {
    const workspaceRoot = getWorkspaceRoot();
    if (!workspaceRoot) {
      console.log("No workspace folder found, skipping test");
      return;
    }
    console.log("Workspace root:", workspaceRoot);

    const resFilePath = path.join(workspaceRoot, "src", "Hello.res");
    console.log("Opening file:", resFilePath);

    const document = await vscode.workspace.openTextDocument(resFilePath);
    const editor = await vscode.window.showTextDocument(document);
    console.log("File opened successfully");

    await ensureExtensionActivated();

    console.log("Starting build watcher...");
    await startBuildWatcher();
    console.log("Build watcher started");

    const compilerLogPath = getCompilerLogPath(workspaceRoot);
    const mtimeBefore = getFileMtime(compilerLogPath);
    if (mtimeBefore) {
      console.log(`compiler.log mtime before: ${mtimeBefore.toISOString()}`);
    } else {
      console.log("compiler.log does not exist yet");
    }

    console.log("Editing file...");
    const originalContent = document.getText();
    await insertCommentAndSave(editor, "/* test comment */\n");
    console.log("File saved with edit");

    console.log("Waiting for compilation...");
    const mtimeAfter = await waitForFileUpdate(compilerLogPath, mtimeBefore);
    if (mtimeAfter) {
      console.log(`compiler.log mtime after: ${mtimeAfter.toISOString()}`);
    } else {
      console.log("compiler.log still does not exist");
    }

    assert.ok(mtimeAfter, "compiler.log should exist after file save");
    if (mtimeBefore && mtimeAfter) {
      assert.ok(
        mtimeAfter > mtimeBefore,
        "compiler.log should be updated after file save",
      );
      console.log("SUCCESS: compiler.log was updated after file save");
    } else if (!mtimeBefore && mtimeAfter) {
      console.log("SUCCESS: compiler.log was created after file save");
    }

    console.log("Restoring original content...");
    await restoreContentAndSave(editor, originalContent);
    console.log("Original content restored");

    await sleep(1000);
    console.log("Test completed");
  });

  test("Code analysis with incremental updates", async () => {
    const workspaceRoot = getWorkspaceRoot();
    if (!workspaceRoot) {
      console.log("No workspace folder found, skipping test");
      return;
    }
    console.log("Workspace root:", workspaceRoot);

    // Remove stale socket file to ensure a fresh server is started
    // Note: Only remove the socket file, not the lock file, to keep the build watcher running
    removeReanalyzeSocketFile(workspaceRoot);

    const resFilePath = path.join(workspaceRoot, "src", "Hello.res");
    console.log("Step 1: Opening file:", resFilePath);

    const document = await vscode.workspace.openTextDocument(resFilePath);
    const editor = await vscode.window.showTextDocument(document);
    console.log("File opened successfully");

    await ensureExtensionActivated();

    console.log("Step 2: Starting build...");
    await startBuildWatcher();
    console.log("Build started");

    console.log("Step 3: Starting code analysis...");
    await startCodeAnalysis();
    console.log("Code analysis started");

    console.log("Step 3b: Opening reanalyze server log...");
    await showReanalyzeServerLog();

    console.log("Step 4: Checking for diagnostics...");
    const diagnostics = vscode.languages.getDiagnostics(document.uri);
    console.log(`Found ${diagnostics.length} diagnostics in Hello.res`);
    assert.ok(diagnostics.length > 0, "Should have diagnostics for dead code");
    for (const diag of diagnostics.slice(0, 5)) {
      console.log(
        `  - Line ${diag.range.start.line + 1}: ${diag.message.substring(0, 80)}...`,
      );
    }
    if (diagnostics.length > 5) {
      console.log(`  ... and ${diagnostics.length - 5} more`);
    }
    const initialDiagnosticsCount = diagnostics.length;

    console.log("Step 5: Adding dead code...");
    const originalContent = document.getText();
    const deadCode = "let testDeadVariable12345 = 999\n";

    const compilerLogPath = getCompilerLogPath(workspaceRoot);
    const compilerLogMtimeBefore = getFileMtime(compilerLogPath);
    if (compilerLogMtimeBefore) {
      console.log(
        `compiler.log mtime before: ${compilerLogMtimeBefore.toISOString()}`,
      );
    } else {
      console.log("compiler.log does not exist before edit");
    }

    await insertCommentAndSave(editor, deadCode);
    console.log("Dead code added and saved");

    console.log("Step 5a: Waiting for compilation...");
    const mtimeAfter = await waitForFileUpdate(
      compilerLogPath,
      compilerLogMtimeBefore,
    );
    if (mtimeAfter) {
      console.log(`compiler.log updated: ${mtimeAfter.toISOString()}`);
    } else {
      console.log("Warning: compilation may not have completed");
    }

    console.log("Step 5b: Re-running code analysis...");
    await vscode.window.showTextDocument(document);
    await startCodeAnalysis();
    console.log("Code analysis re-run complete");

    console.log("Step 6: Checking for updated diagnostics...");
    const updatedDiagnostics = vscode.languages.getDiagnostics(document.uri);
    console.log(
      `Found ${updatedDiagnostics.length} diagnostics after edit (was ${initialDiagnosticsCount})`,
    );

    assert.ok(
      updatedDiagnostics.length > initialDiagnosticsCount,
      `Diagnostics count should increase after adding dead code (was ${initialDiagnosticsCount}, now ${updatedDiagnostics.length})`,
    );

    const deadVarDiagnostic = updatedDiagnostics.find((d) =>
      d.message.includes("testDeadVariable12345"),
    );
    assert.ok(
      deadVarDiagnostic,
      "Should find diagnostic for testDeadVariable12345",
    );
    console.log(
      `Found diagnostic for testDeadVariable12345: ${deadVarDiagnostic.message}`,
    );

    console.log("Step 7: Undoing change...");
    await restoreContentAndSave(editor, originalContent);
    console.log("Change undone and saved");

    await sleep(1000);

    console.log("Step 8: Stopping code analysis...");
    await stopCodeAnalysis();
    console.log("Code analysis stopped");

    console.log("Step 9: Test completed - check Reanalyze Server log for:");
    console.log("  - [request #1] with 'files: X processed, 0 cached'");
    console.log(
      "  - [request #2] with 'files: X processed, Y cached' where Y > 0 (incremental)",
    );
  });

  test("Should prompt to start build when no lock file exists", async () => {
    const workspaceRoot = getWorkspaceRoot();
    if (!workspaceRoot) {
      console.log("No workspace folder found, skipping test");
      return;
    }
    console.log("Workspace root:", workspaceRoot);

    removeRescriptLockFile(workspaceRoot);

    const resFilePath = path.join(workspaceRoot, "src", "More.res");
    console.log("Opening file:", resFilePath);

    const document = await vscode.workspace.openTextDocument(resFilePath);
    await vscode.window.showTextDocument(document);
    console.log("File opened successfully");

    await sleep(1000);

    const promptResult = findBuildPromptInLogs();
    if (promptResult.found) {
      console.log(
        `Found prompt message: "Prompting to start build for ${promptResult.path}"`,
      );
    }

    assert.ok(
      promptResult.found,
      "Should find 'Prompting to start build' message in Language Server log",
    );
    console.log("SUCCESS: Build prompt was shown");
  });
});
