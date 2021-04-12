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
**Private CLI For rescript-vscode usage only**

Examples:
  rescript-editor-support.exe dump src/MyFile.res src/MyFile2.res
  rescript-editor-support.exe complete src/MyFile.res 0 4 currentContent.res
  rescript-editor-support.exe hover src/MyFile.res 10 2

Options:
  dump: debugging. definition and hover for Foo.res and Foo2.res:

    rescript-editor-support.exe dump src/Foo.res src/Foo2.res

  complete: compute autocomplete for Foo.res at line 0 and column 4,
    where Foo.res is being edited and the editor content is in file current.res.

    rescript-editor-support.exe complete src/Foo.res 0 4 current.res

  hover: get inferred type for Foo.res at line 10 column 2:

    rescript-editor-support.exe hover src/Foo.res 10 2

  definition: get inferred type for Foo.res at line 10 column 2:

    rescript-editor-support.exe definition src/Foo.res 10 2
|}

let showHelp () = prerr_endline help

let main () =
  match parseArgs (Sys.argv |> Array.to_list) with
  | opts, _ when hasOpts opts ["-h"; "--help"] -> showHelp ()
  | _opts, "dump" :: files -> EditorSupportCommands.dump files
  | _opts, ["complete"; path; line; char; currentFile] ->
    EditorSupportCommands.complete ~path ~line:(int_of_string line)
      ~char:(int_of_string char) ~currentFile
  | _opts, ["hover"; path; line; char] ->
    EditorSupportCommands.hover ~path ~line:(int_of_string line) ~char:(int_of_string char)
  | _opts, ["definition"; path; line; char] ->
    EditorSupportCommands.definition ~path ~line:(int_of_string line) ~char:(int_of_string char)
  | _ ->
    showHelp ();
    exit 1
;;

main ()
