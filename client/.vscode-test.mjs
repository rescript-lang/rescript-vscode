import { defineConfig } from "@vscode/test-cli";
import * as path from "path";

export default defineConfig([
  {
    label: "example-project",
    files: "out/client/src/test/suite/exampleProject.test.js",
    version: "stable",
    extensionDevelopmentPath: path.resolve(import.meta.dirname, ".."),
    workspaceFolder: path.resolve(
      import.meta.dirname,
      "../analysis/examples/example-project",
    ),
    mocha: {
      ui: "tdd",
      timeout: 60000,
    },
    launchArgs: ["--disable-extensions"],
  },
  {
    label: "monorepo-root",
    files: "out/client/src/test/suite/monorepoRoot.test.js",
    version: "stable",
    extensionDevelopmentPath: path.resolve(import.meta.dirname, ".."),
    workspaceFolder: path.resolve(
      import.meta.dirname,
      "../analysis/examples/monorepo-project",
    ),
    mocha: {
      ui: "tdd",
      timeout: 60000,
    },
    launchArgs: ["--disable-extensions"],
  },
  {
    label: "monorepo-subpackage",
    files: "out/client/src/test/suite/monorepoSubpackage.test.js",
    version: "stable",
    extensionDevelopmentPath: path.resolve(import.meta.dirname, ".."),
    workspaceFolder: path.resolve(
      import.meta.dirname,
      "../analysis/examples/monorepo-project/packages/app",
    ),
    mocha: {
      ui: "tdd",
      timeout: 60000,
    },
    launchArgs: ["--disable-extensions"],
  },
  {
    label: "rescript9-project",
    files: "out/client/src/test/suite/rescript9.test.js",
    version: "stable",
    extensionDevelopmentPath: path.resolve(import.meta.dirname, ".."),
    workspaceFolder: path.resolve(
      import.meta.dirname,
      "../analysis/examples/rescript9-project",
    ),
    mocha: {
      ui: "tdd",
      timeout: 60000,
    },
    launchArgs: ["--disable-extensions"],
  },
]);
