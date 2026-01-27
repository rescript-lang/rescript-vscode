/**
 * Monorepo Root Tests (Monorepo Code Analysis Test Suite)
 *
 * Run these tests:
 *   cd client && npm run test -- --label monorepo-root
 */
import * as assert from "assert";
import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import {
  getWorkspaceRoot,
  removeMonorepoLockFiles,
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

suite("Monorepo Code Analysis Test Suite", () => {
  test("Monorepo: Build watcher works when opening root package", async () => {
    const monorepoRoot = getWorkspaceRoot();
    console.log("Monorepo root:", monorepoRoot);

    const rootResPath = path.join(monorepoRoot, "src", "Root.res");
    if (!fs.existsSync(rootResPath)) {
      console.log("Monorepo project not found, skipping test");
      return;
    }

    removeMonorepoLockFiles(monorepoRoot);

    console.log("Opening root file:", rootResPath);
    const document = await vscode.workspace.openTextDocument(rootResPath);
    const editor = await vscode.window.showTextDocument(document);
    console.log("Root file opened successfully");

    await ensureExtensionActivated();

    console.log("Starting build watcher from root...");
    await startBuildWatcher();
    console.log("Build watcher started");

    const compilerLogPath = getCompilerLogPath(monorepoRoot);
    const mtimeBefore = getFileMtime(compilerLogPath);
    if (mtimeBefore) {
      console.log(
        `Root compiler.log mtime before: ${mtimeBefore.toISOString()}`,
      );
    } else {
      console.log("Root compiler.log does not exist yet");
    }

    console.log("Editing root file...");
    const originalContent = document.getText();
    await insertCommentAndSave(editor, "/* monorepo root test */\n");
    console.log("Root file saved with edit");

    const mtimeAfter = await waitForFileUpdate(compilerLogPath, mtimeBefore);
    if (mtimeAfter) {
      console.log(`Root compiler.log mtime after: ${mtimeAfter.toISOString()}`);
    } else {
      console.log("Root compiler.log still does not exist");
    }

    assert.ok(mtimeAfter, "Root compiler.log should exist after file save");
    if (mtimeBefore && mtimeAfter) {
      assert.ok(
        mtimeAfter > mtimeBefore,
        "Root compiler.log should be updated after file save",
      );
      console.log("SUCCESS: Root compiler.log was updated");
    }

    await restoreContentAndSave(editor, originalContent);
    console.log("Original content restored");

    await sleep(1000);
    console.log("Monorepo root test completed");
  });

  test("Monorepo: Build watcher works when opening subpackage file", async () => {
    const monorepoRoot = getWorkspaceRoot();
    console.log("Monorepo root:", monorepoRoot);

    const appResPath = path.join(
      monorepoRoot,
      "packages",
      "app",
      "src",
      "App.res",
    );
    if (!fs.existsSync(appResPath)) {
      console.log("Monorepo app package not found, skipping test");
      return;
    }

    console.log("Opening subpackage file:", appResPath);
    const document = await vscode.workspace.openTextDocument(appResPath);
    const editor = await vscode.window.showTextDocument(document);
    console.log("Subpackage file opened successfully");

    await ensureExtensionActivated();

    console.log("Starting build watcher from subpackage...");
    await startBuildWatcher();
    console.log("Build watcher started");

    const rootCompilerLogPath = getCompilerLogPath(monorepoRoot);
    const mtimeBefore = getFileMtime(rootCompilerLogPath);
    if (mtimeBefore) {
      console.log(
        `Root compiler.log mtime before: ${mtimeBefore.toISOString()}`,
      );
    } else {
      console.log(
        "Root compiler.log does not exist yet (expected for monorepo subpackage)",
      );
    }

    console.log("Editing subpackage file...");
    const originalContent = document.getText();
    await insertCommentAndSave(editor, "/* monorepo subpackage test */\n");
    console.log("Subpackage file saved with edit");

    const mtimeAfter = await waitForFileUpdate(
      rootCompilerLogPath,
      mtimeBefore,
    );
    if (mtimeAfter) {
      console.log(`Root compiler.log mtime after: ${mtimeAfter.toISOString()}`);
    } else {
      console.log("Root compiler.log still does not exist");
    }

    assert.ok(
      mtimeAfter,
      "Root compiler.log should exist after subpackage file save",
    );
    if (mtimeBefore && mtimeAfter) {
      assert.ok(
        mtimeAfter > mtimeBefore,
        "Root compiler.log should be updated after subpackage file save",
      );
      console.log(
        "SUCCESS: Root compiler.log was updated from subpackage edit",
      );
    }

    await restoreContentAndSave(editor, originalContent);
    console.log("Original content restored");

    await sleep(1000);
    console.log("Monorepo subpackage test completed");
  });

  test("Monorepo: Code analysis works from subpackage", async () => {
    const monorepoRoot = getWorkspaceRoot();
    console.log("Monorepo root:", monorepoRoot);

    const libResPath = path.join(
      monorepoRoot,
      "packages",
      "lib",
      "src",
      "Lib.res",
    );
    if (!fs.existsSync(libResPath)) {
      console.log("Monorepo lib package not found, skipping test");
      return;
    }

    console.log("Opening lib file:", libResPath);
    const document = await vscode.workspace.openTextDocument(libResPath);
    await vscode.window.showTextDocument(document);
    console.log("Lib file opened successfully");

    await ensureExtensionActivated();

    console.log("Starting build...");
    await startBuildWatcher();

    console.log("Starting code analysis...");
    await startCodeAnalysis();
    console.log("Code analysis started");

    console.log("Opening reanalyze server log...");
    await showReanalyzeServerLog();

    const diagnostics = vscode.languages.getDiagnostics(document.uri);
    console.log(`Found ${diagnostics.length} diagnostics in Lib.res`);
    for (const diag of diagnostics.slice(0, 5)) {
      console.log(
        `  - Line ${diag.range.start.line + 1}: ${diag.message.substring(0, 80)}...`,
      );
    }

    assert.ok(
      diagnostics.length > 0,
      "Should have diagnostics for dead code in Lib.res",
    );

    const deadFuncDiagnostic = diagnostics.find((d) =>
      d.message.includes("unusedLibFunction"),
    );
    assert.ok(
      deadFuncDiagnostic,
      "Should find diagnostic for unusedLibFunction in monorepo lib",
    );
    console.log(
      `Found diagnostic for unusedLibFunction: ${deadFuncDiagnostic?.message}`,
    );

    console.log("Stopping code analysis...");
    await stopCodeAnalysis();
    console.log("Code analysis stopped");

    console.log("Monorepo code analysis test completed");
  });

  test("Monorepo: Should prompt to start build when opening subpackage without lock file", async () => {
    const monorepoRoot = getWorkspaceRoot();
    console.log("Monorepo root:", monorepoRoot);

    const appResPath = path.join(
      monorepoRoot,
      "packages",
      "app",
      "src",
      "App.res",
    );
    if (!fs.existsSync(appResPath)) {
      console.log("Monorepo app package not found, skipping test");
      return;
    }

    console.log("Removing lock file from monorepo root...");
    removeMonorepoLockFiles(monorepoRoot);

    console.log("Opening subpackage file:", appResPath);
    const document = await vscode.workspace.openTextDocument(appResPath);
    await vscode.window.showTextDocument(document);
    console.log("Subpackage file opened successfully");

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

    assert.ok(
      promptResult.path.includes("monorepo-project") &&
        !promptResult.path.includes("packages"),
      `Prompt path should be monorepo root, not subpackage. Got: ${promptResult.path}`,
    );
    console.log("SUCCESS: Build prompt was shown for monorepo root path");
  });
});
