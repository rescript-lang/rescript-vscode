@@directive("#!/usr/bin/env node")

@module("fs") external readFileSync: string => string = "readFileSync"
@variadic @module("path") external join: array<string> => string = "join"
@module("path") external dirname: string => string = "dirname"
@val external __dirname: string = "__dirname"

module Buffer = {
  type t

  @send external toString: t => string = "toString"
}

type spawnSyncResult = {
  stdout: Buffer.t,
  stderr: Buffer.t,
  status: Js.Null.t<int>,
}
@module("child_process")
external spawnSync: (string, array<string>) => spawnSyncResult = "spawnSync"

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
    let spawn = spawnSync(analysisProdPath, ["extractDocs", filePath])

    switch spawn.status->Js.Null.toOption {
    | Some(code) if code !== 0 => logAndExit(~log=spawn.stderr->Buffer.toString, ~code)
    | Some(code) => logAndExit(~log=spawn.stdout->Buffer.toString, ~code)
    | None => logAndExit(~log=`error: unexpected error to extract docs for ${filePath}`, ~code=1)
    }
  | _ => logAndExit(~log=docHelp, ~code=1)
  }
| list{"reanalyze", ...rest} =>
  let args = ["reanalyze"]->Js.Array2.concat(Belt.List.toArray(rest))
  let spawn = spawnSync(analysisProdPath, args)

  switch spawn.status->Js.Null.toOption {
  | Some(code) if code !== 0 => logAndExit(~log=spawn.stderr->Buffer.toString, ~code)
  | Some(code) => logAndExit(~log=spawn.stdout->Buffer.toString, ~code)
  | None =>
    logAndExit(
      ~log=`error: unexpected error to run reanalyze with arguments: ${args->Js.Array2.joinWith(
          " ",
        )}`,
      ~code=1,
    )
  }
| list{"-h" | "--help"} => logAndExit(~log=help, ~code=0)
| list{"-v" | "--version"} =>
  switch readFileSync("./package.json")->Js.Json.parseExn->Js.Json.decodeObject {
  | None => logAndExit(~log="error: failed to find version in package.json", ~code=1)
  | Some(dict) => logAndExit(~log=dict->Js.Dict.unsafeGet("version"), ~code=0)
  }
| _ => logAndExit(~log=help, ~code=1)
}
