import * as path from "path";
import { ModuleFormat } from "./buildSchema";

export let platformDir =
  process.arch == "arm64" ? process.platform + process.arch : process.platform;

// See https://microsoft.github.io/language-server-protocol/specification Abstract Message
// version is fixed to 2.0
export let jsonrpcVersion = "2.0";

export let editorAnalysisName = "rescript-editor-analysis.exe";
export let builtinAnalysisDevPath = path.join(
  path.dirname(__dirname),
  "..",
  editorAnalysisName,
);
export let builtinAnalysisProdPath = path.join(
  path.dirname(__dirname),
  "analysis_binaries",
  platformDir,
  editorAnalysisName,
);

export let rescriptBinName = "rescript";

export let bscBinName = "bsc";

export let nodeModulesBinDir = path.join("node_modules", ".bin");

export let bsbLock = ".bsb.lock";
export let bsconfigPartialPath = "bsconfig.json";
export let rescriptJsonPartialPath = "rescript.json";
export let compilerDirPartialPath = path.join("lib", "bs");
export let compilerOcamlDirPartialPath = path.join("lib", "ocaml");
export let compilerLogPartialPath = path.join("lib", "bs", ".compiler.log");
export let buildNinjaPartialPath = path.join("lib", "bs", "build.ninja");
export let rewatchLockPartialPath = path.join("lib", "rewatch.lock");
export let rescriptLockPartialPath = path.join("lib", "rescript.lock");
export let resExt = ".res";
export let resiExt = ".resi";
export let cmiExt = ".cmi";
export let startBuildAction = "Start Build";

// bsconfig defaults according configuration schema (https://rescript-lang.org/docs/manual/latest/build-configuration-schema)
export let bsconfigModuleDefault = ModuleFormat.Commonjs;
export let bsconfigSuffixDefault = ".js";

export let configurationRequestId = "rescript_configuration_request";
export let pullConfigurationInterval = 10_000;
