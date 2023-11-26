(* Update here and package.json *)
let version = "0.1.1"

let docHelp =
  {|ReScript Tools

Output documentation to standard output

Usage: rescript-tools doc <FILE>

Example: rescript-tools doc ./path/to/EntryPointLib.res|}

let help =
  {|ReScript Tools

Usage: rescript-tools [command]

Commands:

doc                   Generate documentation
reanalyze             Reanalyze
-v, --version         Print version
-h, --help            Print help|}

type exitCode = [`Ok | `Error]

let logAndExit ~log ~code =
  print_endline log |> ignore;
  let code =
    match code with
    | `Ok -> 0
    | `Error -> 1
  in
  exit code

let main () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "doc" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> logAndExit ~log:docHelp ~code:`Ok
    | [path] -> Analysis.DocExtraction.extractDocs ~path ~debug:false
    | _ -> logAndExit ~log:docHelp ~code:`Error)
  | "reanalyze" :: _ ->
    let len = Array.length Sys.argv in
    for i = 1 to len - 2 do
      Sys.argv.(i) <- Sys.argv.(i + 1)
    done;
    Sys.argv.(len - 1) <- "";
    Reanalyze.cli ()
  | ["-h"] | ["--help"] -> logAndExit ~log:help ~code:`Ok
  | ["-v"] | ["--version"] -> logAndExit ~log:version ~code:`Ok
  | _ -> logAndExit ~log:help ~code:`Error

let () = main ()
