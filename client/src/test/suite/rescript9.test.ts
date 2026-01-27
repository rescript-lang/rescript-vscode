/**
 * ReScript 9 Tests (ReScript 9 Build Test Suite)
 *
 * This tests that the build watcher works with older ReScript versions
 * that use "rescript build -w" instead of "rescript watch".
 *
 * Run these tests:
 *   cd client && npm run test -- --label rescript9-project
 */
import * as assert from "assert";
import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import {
  getWorkspaceRoot,
  removeBsbLockFile,
  ensureExtensionActivated,
  waitFor,
  sleep,
  getCompilerLogPath,
  getFileMtime,
  waitForFileUpdate,
  insertCommentAndSave,
  restoreContentAndSave,
  startBuildWatcher,
  findBuildPromptInLogs,
} from "./helpers";

suite("ReScript 9 Build Test Suite", () => {
  test("ReScript 9: Extension should be present", async () => {
    const extension = vscode.extensions.getExtension(
      "chenglou92.rescript-vscode",
    );
    assert.ok(extension, "ReScript extension should be present");
    console.log("Extension found:", extension.id);
  });

  test("ReScript 9: Build watcher should start with 'rescript build -w'", async () => {
    const workspaceRoot = getWorkspaceRoot();
    console.log("Workspace root:", workspaceRoot);

    // Verify we're in the ReScript 9 project
    const bsconfigPath = path.join(workspaceRoot, "bsconfig.json");
    if (!fs.existsSync(bsconfigPath)) {
      console.log("bsconfig.json not found, skipping test");
      return;
    }

    // Check ReScript version
    const packageJsonPath = path.join(
      workspaceRoot,
      "node_modules",
      "rescript",
      "package.json",
    );
    if (fs.existsSync(packageJsonPath)) {
      const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, "utf-8"));
      console.log("ReScript version:", packageJson.version);
      assert.ok(
        packageJson.version.startsWith("9."),
        `Expected ReScript 9.x, got ${packageJson.version}`,
      );
    }

    // Open a ReScript file
    const resFilePath = path.join(workspaceRoot, "src", "Hello.res");
    console.log("Opening file:", resFilePath);

    const document = await vscode.workspace.openTextDocument(resFilePath);
    await vscode.window.showTextDocument(document);
    console.log("File opened successfully");

    await ensureExtensionActivated();

    // Start the build watcher
    console.log("Starting build watcher (should use 'rescript build -w')...");
    await startBuildWatcher(1500);
    console.log("Build watcher started");

    // Check if the lock file was created (.bsb.lock for ReScript 9)
    const lockPath = path.join(workspaceRoot, ".bsb.lock");
    const lockExists = fs.existsSync(lockPath);
    console.log(`.bsb.lock exists: ${lockExists}`);
    assert.ok(
      lockExists,
      ".bsb.lock should exist after starting build watcher",
    );

    console.log("ReScript 9 build watcher test completed");
  });

  test("ReScript 9: Build watcher recompiles on file save", async () => {
    const workspaceRoot = getWorkspaceRoot();
    console.log("Workspace root:", workspaceRoot);

    // Open a ReScript file
    const resFilePath = path.join(workspaceRoot, "src", "Hello.res");
    console.log("Opening file:", resFilePath);

    const document = await vscode.workspace.openTextDocument(resFilePath);
    const editor = await vscode.window.showTextDocument(document);
    console.log("File opened successfully");

    await ensureExtensionActivated();

    // Start the build watcher
    console.log("Starting build watcher...");
    await startBuildWatcher();
    console.log("Build watcher started");

    // Check compiler.log modification time before edit
    const compilerLogPath = getCompilerLogPath(workspaceRoot);
    const mtimeBefore = getFileMtime(compilerLogPath);
    if (mtimeBefore) {
      console.log(`compiler.log mtime before: ${mtimeBefore.toISOString()}`);
    } else {
      console.log("compiler.log does not exist yet");
    }

    // Edit the file and save
    console.log("Editing file...");
    const originalContent = document.getText();
    await insertCommentAndSave(editor, "/* rescript 9 test */\n");
    console.log("File saved with edit");

    // Wait for compilation
    console.log("Waiting for compilation...");
    const mtimeAfter = await waitForFileUpdate(compilerLogPath, mtimeBefore, {
      timeout: 3000,
    });
    if (mtimeAfter) {
      console.log(`compiler.log mtime after: ${mtimeAfter.toISOString()}`);
    } else {
      console.log("compiler.log still does not exist");
    }

    // Assert that compiler.log was updated
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

    // Restore original content
    console.log("Restoring original content...");
    await restoreContentAndSave(editor, originalContent);
    console.log("Original content restored");

    await sleep(1000);
    console.log("ReScript 9 recompilation test completed");
  });

  test("ReScript 9: Should prompt to start build when no lock file exists", async () => {
    const workspaceRoot = getWorkspaceRoot();
    console.log("Workspace root:", workspaceRoot);

    // Remove lock file to trigger the "Start Build" prompt
    removeBsbLockFile(workspaceRoot);

    // Open a .res file to trigger the LSP
    const resFilePath = path.join(workspaceRoot, "src", "Hello.res");
    console.log("Opening file:", resFilePath);

    const document = await vscode.workspace.openTextDocument(resFilePath);
    await vscode.window.showTextDocument(document);
    console.log("File opened successfully");

    // Wait for LSP to process and potentially show the prompt
    await sleep(1000);

    // Read the Language Server log to verify the prompt was shown
    const promptResult = findBuildPromptInLogs();
    if (promptResult.found) {
      console.log("Found prompt message in logs");
    }

    assert.ok(
      promptResult.found,
      "Should find 'Prompting to start build' message in Language Server log",
    );
    console.log("SUCCESS: Build prompt was shown for ReScript 9 project");
  });

  test("ReScript 9: Lock file should be cleaned up on language server restart", async () => {
    const workspaceRoot = getWorkspaceRoot();
    console.log("Workspace root:", workspaceRoot);

    // First restart language server to ensure clean state
    console.log("Restarting language server to ensure clean state...");
    await vscode.commands.executeCommand(
      "rescript-vscode.restart_language_server",
    );
    await sleep(2000);

    // Open a ReScript file
    const resFilePath = path.join(workspaceRoot, "src", "Hello.res");
    console.log("Opening file:", resFilePath);

    const document = await vscode.workspace.openTextDocument(resFilePath);
    await vscode.window.showTextDocument(document);
    console.log("File opened successfully");

    // Start the build watcher
    console.log("Starting build watcher...");
    await vscode.commands.executeCommand("rescript-vscode.start_build");

    // Wait for lock file to appear (poll up to 5 seconds)
    await sleep(500);
    const lockPath = path.join(workspaceRoot, ".bsb.lock");
    const lockExistsBefore = await waitFor(() => fs.existsSync(lockPath), {
      timeout: 5000,
      interval: 500,
      message: ".bsb.lock",
    });
    assert.ok(lockExistsBefore, ".bsb.lock should exist before restart");

    // Restart language server (this should kill the build watcher and clean up lock file)
    console.log("Restarting language server...");
    await vscode.commands.executeCommand(
      "rescript-vscode.restart_language_server",
    );

    // Wait for restart to complete
    await sleep(2000);

    // Verify lock file is cleaned up
    const lockExistsAfter = fs.existsSync(lockPath);
    console.log(`.bsb.lock exists after restart: ${lockExistsAfter}`);
    assert.ok(
      !lockExistsAfter,
      ".bsb.lock should be cleaned up after language server restart",
    );

    console.log("SUCCESS: Lock file was cleaned up on language server restart");
  });
});
