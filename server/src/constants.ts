import * as path from "path";

// See https://microsoft.github.io/language-server-protocol/specification Abstract Message
// version is fixed to 2.0
export let jsonrpcVersion = "2.0";
// can't use the native bsc, different package managers and project layouts (e.g. yarn workspace)
// might put the platform specific binaries in an unknown location
export let bscPartialPath = path.join("node_modules", ".bin", "bsc");
// can't use the native bsb since we might need the watcher -w flag, which is only in the js wrapper
// export let bsbPartialPath = path.join('node_modules', 'bs-platform', process.platform, 'bsb.exe');
export let bsbPartialPath = path.join("node_modules", ".bin", "bsb");
export let bsbLock = ".bsb.lock";
export let bsconfigPartialPath = "bsconfig.json";
export let compilerLogPartialPath = path.join("lib", "bs", ".compiler.log");
export let resExt = ".res";
export let resiExt = ".resi";
export let startBuildAction = "Start Build";
