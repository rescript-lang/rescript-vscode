let help =
  {|
**Private CLI For rescript-vscode usage only**

Examples:
  ./rescript-editor-analysis.exe complete src/MyFile.res 0 4 currentContent.res
  ./rescript-editor-analysis.exe definition src/MyFile.res 9 3
  ./rescript-editor-analysis.exe dump src/MyFile.res src/MyFile2.res
  ./rescript-editor-analysis.exe documentSymbol src/Foo.res
  ./rescript-editor-analysis.exe hover src/MyFile.res 10 2
  ./rescript-editor-analysis.exe references src/MyFile.res 10 2
  ./rescript-editor-analysis.exe test src/MyFile.res

Note: coordinates are zero-based, so the first position is 0 0.

Options:
  complete: compute autocomplete for MyFile.res at line 0 and column 4,
    where MyFile.res is being edited and the editor content is in file current.res.

    ./rescript-editor-analysis.exe complete src/MyFile.res 0 4 current.res

  definition: get definition for item in MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe definition src/MyFile.res 10 2

  dump: for debugging, show all definitions and hovers for MyFile.res and MyFile.res:

    ./rescript-editor-analysis.exe dump src/Foo.res src/MyFile.res

  documentSymbol: get all symbols declared in MyFile.res

    ./rescript-editor-analysis.exe documentSymbol src/MyFile.res

  hover: get inferred type for MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe hover src/MyFile.res 10 2

  references: get all references to item in MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe references src/MyFile.res 10 2

  test: run tests specified by special comments in file src/MyFile.res

  ./rescript-editor-analysis.exe test src/src/MyFile.res
|}

let main () =
  match Array.to_list Sys.argv with
  | [_; "complete"; path; line; col; currentFile] ->
    Commands.complete ~path ~line:(int_of_string line) ~col:(int_of_string col)
      ~currentFile
  | [_; "definition"; path; line; col] ->
    Commands.definition ~path ~line:(int_of_string line)
      ~col:(int_of_string col)
  | _ :: "dump" :: files -> Commands.dump files
  | [_; "documentSymbol"; path] -> Commands.documentSymbol ~path
  | [_; "hover"; path; line; col] ->
    Commands.hover ~path ~line:(int_of_string line) ~col:(int_of_string col)
  | [_; "references"; path; line; col] ->
    Commands.references ~path ~line:(int_of_string line)
      ~col:(int_of_string col)
  | [_; "test"; path] -> Commands.test ~path
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1

;;
main ()
