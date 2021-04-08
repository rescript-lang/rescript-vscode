module J = JsonShort
module StringSet = Set.Make (String)

let parseArgs args =
  match args with
  | [] -> assert false
  | _ :: args ->
    let opts, pos =
      args |> List.rev
      |> List.fold_left
          (fun (set, pos) arg ->
            if arg <> "" && arg.[0] = '-' then (set |> StringSet.add arg, pos)
            else (set, arg :: pos))
          (StringSet.empty, [])
    in
    (opts, pos)

let hasOpt opts name = opts |> StringSet.mem name

let hasOpts opts names = names |> List.exists (opts |> hasOpt)

let help =
  {|
Commands for Rescript Language Server

-dump: compute definition and hover for Foo.res at line 0 and column 4:

rescript-editor-support.exe dump src/Foo.res:0:4

-complete: compute autocomplete for Foo.res at line 0 and column 4,
 where Foo.res is being edited and the editor content is in file current.res.

rescript-editor-support.exe complete src/Foo.res:0:4 current.res

The dump command can also omit `:line:column`, to show results for every position in the file. Several files can be specified on the command line.
|}

let showHelp () = prerr_endline help

let main () =
  match parseArgs (Sys.argv |> Array.to_list) with
  | opts, _ when hasOpts opts ["-h"; "--help"] -> showHelp ()
  | _opts, "dump" :: files -> EditorSupportCommands.dump files
  | _opts, ["complete"; pathWithPos; currentFile] ->
    EditorSupportCommands.complete ~pathWithPos ~currentFile
  | _ ->
    showHelp ();
    exit 1
;;

main ()
