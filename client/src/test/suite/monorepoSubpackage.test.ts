/**
 * Monorepo Subpackage Tests (Monorepo Subpackage Test Suite)
 *
 * This test suite runs with VSCode opened on a subpackage (packages/app),
 * not the monorepo root. It tests that the extension correctly detects
 * monorepo structure even when opened from a subpackage.
 *
 * Run these tests:
 *   cd client && npm run test -- --label monorepo-subpackage
 */
import * as assert from "assert";
import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import {
  getWorkspaceRoot,
  removeMonorepoLockFiles,
  sleep,
  startBuildWatcher,
  startCodeAnalysis,
  stopCodeAnalysis,
  showReanalyzeServerLog,
  findBuildPromptInLogs,
} from "./helpers";

suite("Monorepo Subpackage Test Suite", () => {
  test("Subpackage workspace: Should prompt to start build with monorepo root path", async () => {
    // In this test, workspaceRoot is packages/app (the subpackage)
    const workspaceRoot = getWorkspaceRoot();
    console.log("Workspace root (subpackage):", workspaceRoot);

    // The monorepo root is 2 levels up from packages/app
    const monorepoRoot = path.resolve(workspaceRoot, "../..");
    console.log("Monorepo root:", monorepoRoot);

    // Verify we're in the right setup - workspace should be a subpackage
    assert.ok(
      workspaceRoot.includes("packages"),
      `Workspace should be in packages folder, got: ${workspaceRoot}`,
    );

    // Check if the subpackage has a rescript.json
    const rescriptJsonPath = path.join(workspaceRoot, "rescript.json");
    if (!fs.existsSync(rescriptJsonPath)) {
      console.log("Subpackage rescript.json not found, skipping test");
      return;
    }

    // Remove lock file from MONOREPO ROOT to trigger the "Start Build" prompt
    console.log("Removing lock file from monorepo root...");
    removeMonorepoLockFiles(monorepoRoot);

    // Open a .res file from the subpackage workspace
    const resFilePath = path.join(workspaceRoot, "src", "App.res");
    console.log("Opening file:", resFilePath);

    const document = await vscode.workspace.openTextDocument(resFilePath);
    await vscode.window.showTextDocument(document);
    console.log("File opened successfully");

    // Wait for LSP to process - since no lock file exists, it should prompt for build
    await sleep(1000);

    // Read the Language Server log to verify the prompt was shown
    const promptResult = findBuildPromptInLogs();
    if (promptResult.found) {
      console.log(
        `Found prompt message: "Prompting to start build for ${promptResult.path}"`,
      );
    }

    // Assert that the prompt was shown
    assert.ok(
      promptResult.found,
      "Should find 'Prompting to start build' message in Language Server log",
    );

    // Assert that the prompt path is the monorepo root, not the subpackage
    // Even though we opened from packages/app, the build prompt should use monorepo root
    assert.ok(
      promptResult.path.includes("monorepo-project") &&
        !promptResult.path.includes("packages"),
      `Prompt path should be monorepo root, not subpackage. Got: ${promptResult.path}`,
    );
    console.log(
      "SUCCESS: Build prompt correctly uses monorepo root path when opened from subpackage",
    );

    // Now start the build and verify the lock file is created at the monorepo root
    console.log("Starting build from subpackage workspace...");
    await startBuildWatcher(1500);
    console.log("Build started");

    // Check that the lock file exists at the MONOREPO ROOT, not the subpackage
    const monorepoLockPath = path.join(monorepoRoot, "lib", "rescript.lock");
    const subpackageLockPath = path.join(workspaceRoot, "lib", "rescript.lock");

    const monorepoLockExists = fs.existsSync(monorepoLockPath);
    const subpackageLockExists = fs.existsSync(subpackageLockPath);

    console.log(
      `Monorepo lock file (${monorepoLockPath}): ${monorepoLockExists ? "EXISTS" : "NOT FOUND"}`,
    );
    console.log(
      `Subpackage lock file (${subpackageLockPath}): ${subpackageLockExists ? "EXISTS" : "NOT FOUND"}`,
    );

    // The lock file should exist at the monorepo root
    assert.ok(
      monorepoLockExists,
      `Lock file should exist at monorepo root: ${monorepoLockPath}`,
    );

    // The lock file should NOT exist at the subpackage level
    assert.ok(
      !subpackageLockExists,
      `Lock file should NOT exist at subpackage: ${subpackageLockPath}`,
    );

    console.log("SUCCESS: Lock file created at monorepo root, not subpackage");

    // Remove any stale reanalyze socket file from a previous test run
    const socketPath = path.join(monorepoRoot, ".rescript-reanalyze.sock");
    if (fs.existsSync(socketPath)) {
      fs.unlinkSync(socketPath);
      console.log("Removed stale socket file");
    }

    // Start code analysis
    console.log("Starting code analysis...");
    await startCodeAnalysis();
    console.log("Code analysis started");

    // Open the reanalyze server log - verify it returns true (output channel shown)
    console.log("Opening reanalyze server log...");
    await showReanalyzeServerLog();

    // Verify diagnostics are shown (code analysis is working from subpackage)
    const diagnostics = vscode.languages.getDiagnostics(document.uri);
    console.log(`Found ${diagnostics.length} diagnostics in App.res`);
    assert.ok(
      diagnostics.length > 0,
      "Code analysis should find diagnostics in App.res when run from subpackage",
    );

    // Stop code analysis
    console.log("Stopping code analysis...");
    await stopCodeAnalysis();
    console.log("Code analysis stopped");

    console.log("Test complete - lock file will be cleaned up on LSP shutdown");
  });
});
