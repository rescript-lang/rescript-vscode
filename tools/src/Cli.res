@@directive("#!/usr/bin/env node")

@module("fs") external readFileSync: string => string = "readFileSync"
@variadic @module("path") external join: array<string> => string = "join"
@module("path") external dirname: string => string = "dirname"
@val external __dirname: string = "__dirname"

type processEnvOptions = {stdio?: string}
type spawnSyncResult = {status: Js.Null.t<int>}

@module("child_process")
external spawnSync: (string, array<string>, option<processEnvOptions>) => spawnSyncResult =
  "spawnSync"

@val @scope("process")
external exit: int => unit = "exit"

@val
external process: {"arch": string, "platform": string, "argv": array<string>} = "process"

let argv = process["argv"]

let args = argv->Js.Array2.slice(~start=2, ~end_=Js.Array2.length(argv))

let platformDir =
  process["arch"] === "arm64" ? process["platform"] ++ process["arch"] : process["platform"]

let analysisProdPath = join([
  dirname(__dirname),
  "analysis_binaries",
  platformDir,
  "rescript-editor-analysis.exe",
])

let docHelp = `ReScript Tools

Output documentation to standard output

Usage: rescript-tools doc <FILE>

Example: rescript-tools doc ./path/to/EntryPointLib.res`

let help = `ReScript Tools

Usage: rescript-tools [command]

Commands:

doc                   Generate documentation
reanalyze             Reanalyze
-v, --version         Print version
-h, --help            Print help`

let logAndExit = (~log, ~code) => {
  Js.log(log)
  exit(code)
}

switch args->Belt.List.fromArray {
| list{"doc", ...rest} =>
  switch rest {
  | list{"-h" | "--help"} => logAndExit(~log=docHelp, ~code=0)
  | list{filePath} =>
    let spawn = spawnSync(analysisProdPath, ["extractDocs", filePath], Some({stdio: "inherit"}))

    switch spawn.status->Js.Null.toOption {
    | Some(code) => exit(code)
    | None => ()
    }
  | _ => logAndExit(~log=docHelp, ~code=1)
  }
| list{"reanalyze", ...rest} =>
  let args = ["reanalyze"]->Js.Array2.concat(Belt.List.toArray(rest))
  let spawn = spawnSync(analysisProdPath, args, Some({stdio: "inherit"}))

  switch spawn.status->Js.Null.toOption {
  | Some(code) => exit(code)
  | None => ()
  }
| list{"-h" | "--help"} => logAndExit(~log=help, ~code=0)
| list{"-v" | "--version"} =>
  let packageJson = join([__dirname, "..", "package.json"])
  switch readFileSync(packageJson)->Js.Json.parseExn->Js.Json.decodeObject {
  | None => logAndExit(~log="error: failed to find version in package.json", ~code=1)
  | Some(dict) =>
    switch dict->Js.Dict.get("version") {
    | Some(version) => logAndExit(~log=version, ~code=0)
    | None => logAndExit(~log="error: failed to find version in package.json", ~code=1)
    }
  }
| _ => logAndExit(~log=help, ~code=1)
}
