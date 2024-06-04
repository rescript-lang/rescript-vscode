import * as path from "path";
import { ModuleFormat } from "./buildSchema";

export let platformDir =
  process.arch == "arm64" ? process.platform + process.arch : process.platform;

// See https://microsoft.github.io/language-server-protocol/specification Abstract Message
// version is fixed to 2.0
export let jsonrpcVersion = "2.0";
export let platformPath = path.join("rescript", platformDir);
export let nodeModulesPlatformPath = path.join("node_modules", platformPath);
export let bscExeName = "bsc.exe";
export let bscNativeReScriptPartialPath = path.join(
  nodeModulesPlatformPath,
  bscExeName
);

export let analysisDevPath = path.join(
  path.dirname(__dirname),
  "..",
  "rescript-editor-analysis.exe"
);
export let analysisProdPath = path.join(
  path.dirname(__dirname),
  "analysis_binaries",
  platformDir,
  "rescript-editor-analysis.exe"
);

export let rescriptBinName = "rescript";

export let bscBinName = "bsc";

export let nodeModulesBinDir = path.join("node_modules", ".bin");

// can't use the native bsb/rescript since we might need the watcher -w flag, which is only in the JS wrapper
export let rescriptNodePartialPath = path.join(
  nodeModulesBinDir,
  rescriptBinName
);

export let bsbLock = ".bsb.lock";
export let bsconfigPartialPath = "bsconfig.json";
export let rescriptJsonPartialPath = "rescript.json";
export let compilerDirPartialPath = path.join("lib", "bs");
export let compilerLogPartialPath = path.join("lib", "bs", ".compiler.log");
export let buildNinjaPartialPath = path.join("lib", "bs", "build.ninja");
export let resExt = ".res";
export let resiExt = ".resi";
export let cmiExt = ".cmi";
export let startBuildAction = "Start Build";

// bsconfig defaults according configuration schema (https://rescript-lang.org/docs/manual/latest/build-configuration-schema)
export let bsconfigModuleDefault = ModuleFormat.Commonjs;
export let bsconfigSuffixDefault = ".js";

export let configurationRequestId = "rescript_configuration_request";
export let pullConfigurationInterval = 10_000;
