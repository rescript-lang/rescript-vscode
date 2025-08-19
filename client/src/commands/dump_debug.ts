import * as cp from "child_process";
import * as fs from "fs";
import {
  ExtensionContext,
  StatusBarItem,
  Uri,
  ViewColumn,
  window,
} from "vscode";
import {
  createFileInTempDir,
  findProjectRootOfFileInDir,
  getBinaryPath,
} from "../utils";
import * as path from "path";

// Maps to Cli.ml
const debugCommands = [
  { command: "dumpAst" as const, title: "Dump the AST" },
  { command: "completion" as const, title: "Completion" },
  { command: "definition" as const, title: "Definition" },
  { command: "typeDefinition" as const, title: "Type Definition" },
  { command: "documentSymbol" as const, title: "Document Symbol" },
  { command: "hover" as const, title: "Hover" },
  { command: "signatureHelp" as const, title: "Signature Help" },
  { command: "inlayHint" as const, title: "Inlay Hint" },
  { command: "codeLens" as const, title: "Code Lens" },
  { command: "extractDocs" as const, title: "Extract Docs" },
  { command: "codeAction" as const, title: "Code Action" },
  { command: "codemod" as const, title: "Code Mod" },
  { command: "diagnosticSyntax" as const, title: "Diagnostic Syntax" },
  { command: "references" as const, title: "References" },
  { command: "rename" as const, title: "Rename" },
  { command: "semanticTokens" as const, title: "Semantic Tokens" },
  { command: "createInterface" as const, title: "Create Interface" },
  { command: "format" as const, title: "Format" },
];

const logLevels = ["verbose" as const, "regular" as const, "off" as const];

function runDebugDump({
  binaryPath,
  cwd,
  cliOptions,
}: {
  binaryPath: string;
  cwd: string;
  cliOptions: string[];
}): Promise<string | null> {
  return new Promise((resolve) => {
    let opts = [...cliOptions];
    window.showInformationMessage(JSON.stringify(opts));
    let p = cp.spawn(binaryPath, opts, {
      cwd,
    });

    if (p.stdout == null) {
      window.showErrorMessage("Something went wrong.");
      resolve(null);
      return;
    }

    let data = "";

    p.stdout.on("data", (d) => {
      data += d;
    });

    p.stderr?.on("data", (e) => {
      window.showErrorMessage(
        `Something went wrong trying to run debug dump: '${e}'`,
      );
      resolve(e.toString());
    });

    p.on("close", () => {
      resolve(data);
    });
  });
}

function runBsc({
  cwd,
  cliOptions,
}: {
  cwd: string;
  cliOptions: string[];
}): Promise<string | null> {
  return new Promise((resolve) => {
    let opts = ["bsc", ...cliOptions];
    let p = cp.spawn("npx", opts, {
      cwd,
    });

    if (p.stdout == null) {
      window.showErrorMessage("Something went wrong.");
      resolve(null);
      return;
    }

    let data = "";

    p.stdout.on("data", (d) => {
      data += d;
    });

    p.stderr?.on("data", (e) => {
      data += e.toString();
    });

    p.on("close", () => {
      resolve(data);
    });
  });
}

let rerunCommand: null | (() => Promise<void>) = null;

export const dumpDebugRetrigger = () => {
  if (rerunCommand != null) {
    rerunCommand();
  }
};

export const dumpDebug = async (
  context: ExtensionContext,
  statusBarItem: StatusBarItem,
) => {
  const editor = window.activeTextEditor;

  if (!editor) {
    return window.showInformationMessage("No active editor");
  }

  const { line, character } = editor.selection.active;
  const { line: endLine, character: endChar } = editor.selection.end;
  const filePath = editor.document.uri.fsPath;

  let projectRootPath: string | null = findProjectRootOfFileInDir(filePath);
  const binaryPath = getBinaryPath(
    "rescript-editor-analysis.exe",
    projectRootPath,
  );
  if (binaryPath === null) {
    window.showErrorMessage("Binary executable not found.");
    return;
  }

  const callTypeTitle = await window.showQuickPick(
    debugCommands.map((d) => d.title),
    {
      title: "Select call type",
    },
  );
  const callType = debugCommands.find((d) => d.title === callTypeTitle);

  if (callType == null) {
    window.showErrorMessage(`Debug call type not found: "${callTypeTitle}"`);
    return null;
  }

  let logLevel = "verbose";

  if (!["dumpAst"].includes(callType.command)) {
    logLevel = await window.showQuickPick(logLevels, {
      title: "Select log level",
    });
  }

  const outputFile = createFileInTempDir(callType.title, ".txt");
  const document = window.activeTextEditor.document;

  const runCommand = async () => {
    const extension = path.extname(filePath);
    const currentFile = createFileInTempDir("current_file", extension);

    fs.writeFileSync(currentFile, document.getText());

    if (["dumpAst"].includes(callType.command)) {
      switch (callType.command) {
        case "dumpAst":
          const res = await runBsc({
            cwd: path.dirname(filePath),
            cliOptions: [
              "-dparsetree",
              "-only-parse",
              "-ignore-parse-errors",
              "-bs-loc",
              currentFile,
            ],
          });
          fs.writeFileSync(outputFile, `Pos: ${line}:${character}\n\n${res}`);
          return;
      }

      fs.rmSync(currentFile);
    }

    const opts: string[] = ["debug-dump", logLevel, callType.command];

    switch (callType.command) {
      case "completion": {
        opts.push(filePath, line.toString(), character.toString(), currentFile);
        break;
      }
      case "definition": {
        opts.push(filePath, line.toString(), character.toString());
        break;
      }
      case "typeDefinition": {
        opts.push(filePath, line.toString(), character.toString());
        break;
      }
      case "documentSymbol": {
        opts.push(filePath);
        break;
      }
      case "hover": {
        opts.push(filePath, line.toString(), character.toString(), currentFile);
        break;
      }
      case "signatureHelp": {
        opts.push(filePath, line.toString(), character.toString(), currentFile);
        break;
      }
      case "inlayHint": {
        window.showErrorMessage("Not implemented yet.");
        return null;
      }
      case "codeLens": {
        opts.push(filePath);
        break;
      }
      case "extractDocs": {
        opts.push(filePath);
        break;
      }
      case "codeAction": {
        opts.push(
          filePath,
          line.toString(),
          character.toString(),
          endLine.toString(),
          endChar.toString(),
          currentFile,
        );
        break;
      }
      case "codemod": {
        opts.push(
          currentFile,
          line.toString(),
          character.toString(),
          "add-missing-cases", // TODO: Make selectable
        );
        break;
      }
      case "diagnosticSyntax": {
        opts.push(currentFile);
        break;
      }
      case "references": {
        opts.push(filePath, line.toString(), character.toString());
        break;
      }
      case "semanticTokens": {
        opts.push(currentFile);
        break;
      }
      case "createInterface": {
        window.showErrorMessage("Not implemented yet.");
        return null;
      }
      case "format": {
        opts.push(currentFile);
        break;
      }
      default:
        window.showErrorMessage(`"${callType.title}" is not implemented yet.`);
        return null;
    }

    const res = await runDebugDump({
      binaryPath,
      cwd: path.dirname(filePath),
      cliOptions: opts,
    });

    fs.writeFileSync(outputFile, res);
    fs.rmSync(currentFile);
  };

  rerunCommand = runCommand;

  await runCommand();

  await window.showTextDocument(Uri.parse(outputFile), {
    viewColumn: ViewColumn.Beside,
  });

  statusBarItem.show();
  statusBarItem.text = "$(debug-restart) Rerun command";
  statusBarItem.command = "rescript-vscode.debug-dump-retrigger";

  const unwatch = fs.watch(outputFile, (event, _) => {
    if (event === "rename") {
      fs.rmSync(outputFile);
      statusBarItem.hide();
      rerunCommand = null;
    }
  });

  context.subscriptions.push({ dispose: () => unwatch });
};
