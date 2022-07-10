import * as path from "path";

let platformDir =
  process.arch == "arm64" ? process.platform + process.arch : process.platform;

// See https://microsoft.github.io/language-server-protocol/specification Abstract Message
// version is fixed to 2.0
export let jsonrpcVersion = "2.0";
export let bscNativeReScriptPartialPath = path.join(
  "node_modules",
  "rescript",
  platformDir,
  "bsc.exe"
);
export let bscNativePartialPath = path.join(
  "node_modules",
  "bs-platform",
  platformDir,
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

export let rescriptBinName = "rescript";

export let bsbBinName = "bsb";

export let bscBinName = "bsc";

// can't use the native bsb/rescript since we might need the watcher -w flag, which is only in the JS wrapper
export let rescriptNodePartialPath = path.join(
  "node_modules",
  ".bin",
  rescriptBinName,
);
export let bsbNodePartialPath = path.join("node_modules", ".bin", bsbBinName);

export let bsbLock = ".bsb.lock";
export let bsconfigPartialPath = "bsconfig.json";
export let compilerDirPartialPath = path.join("lib", "bs");
export let compilerLogPartialPath = path.join("lib", "bs", ".compiler.log");
export let resExt = ".res";
export let resiExt = ".resi";
export let cmiExt = ".cmi";
export let startBuildAction = "Start Build";

// bsconfig defaults according configuration schema (https://rescript-lang.org/docs/manual/latest/build-configuration-schema)
export let bsconfigModuleDefault = "commonjs";
export let bsconfigSuffixDefault = ".js";

export let configurationRequestId = "rescript_configuration_request";
export let pullConfigurationInterval = 10_000;
