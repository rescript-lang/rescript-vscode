import * as path from "path";

// See https://microsoft.github.io/language-server-protocol/specification Abstract Message
// version is fixed to 2.0
export let jsonrpcVersion = "2.0";
export let bscNativeReScriptPartialPath = path.join(
  "node_modules",
  "rescript",
  process.platform,
  "bsc.exe"
);
export let bscNativePartialPath = path.join(
  "node_modules",
  "bs-platform",
  process.platform,
  "bsc.exe"
);

export let analysisDevPath = path.join(
  path.dirname(__dirname),
  "..",
  "analysis",
  "rescript-editor-analysis.exe"
);
export let analysisProdPath = path.join(
  path.dirname(__dirname),
  "analysis_binaries",
  process.platform,
  "rescript-editor-analysis.exe"
);

// can't use the native bsb/rescript since we might need the watcher -w flag, which is only in the JS wrapper
export let rescriptNodePartialPath = path.join(
  "node_modules",
  ".bin",
  "rescript"
);
export let bsbNodePartialPath = path.join("node_modules", ".bin", "bsb");

export let bsbLock = ".bsb.lock";
export let bsconfigPartialPath = "bsconfig.json";
export let compilerDirPartialPath = path.join("lib", "bs");
export let compilerLogPartialPath = path.join("lib", "bs", ".compiler.log");
export let resExt = ".res";
export let resiExt = ".resi";
export let cmiExt = ".cmi";
export let startBuildAction = "Start Build";
