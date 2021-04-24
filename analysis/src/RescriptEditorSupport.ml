let help =
  {|
**Private CLI For rescript-vscode usage only**

Examples:
  rescript-editor-support.exe dump src/MyFile.res src/MyFile2.res
  rescript-editor-support.exe complete src/MyFile.res 0 4 currentContent.res
  rescript-editor-support.exe hover src/MyFile.res 10 2
  rescript-editor-support.exe definition src/MyFile.res 9 3

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
  match Array.to_list Sys.argv with
  | [_; "complete"; path; line; col; currentFile] ->
    EditorSupportCommands.complete ~path ~line:(int_of_string line)
      ~col:(int_of_string col) ~currentFile
  | [_; "hover"; path; line; col] ->
    EditorSupportCommands.hover ~path ~line:(int_of_string line)
      ~col:(int_of_string col)
  | [_; "definition"; path; line; col] ->
    EditorSupportCommands.definition ~path ~line:(int_of_string line)
      ~col:(int_of_string col)
  | _ :: "dump" :: files -> EditorSupportCommands.dump files
  | [_; "test"; path] -> EditorSupportCommands.test ~path
  | args when List.mem "-h" args || List.mem "--help" args -> showHelp ()
  | _ ->
    showHelp ();
    exit 1

;;
main ()
