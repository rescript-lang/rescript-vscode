let help =
  {|
**Private CLI For rescript-vscode usage only**

Examples:
  ./run.exe dump src/MyFile.res src/MyFile2.res
  ./run.exe complete src/MyFile.res 0 4 currentContent.res
  ./run.exe hover src/MyFile.res 10 2
  ./run.exe definition src/MyFile.res 9 3

Note: coordinates are zero-based, so the first position is 0 0.

Options:
  dump: debugging. definition and hover for Foo.res and Foo2.res:

    ./run.exe dump src/Foo.res src/Foo2.res

  complete: compute autocomplete for Foo.res at line 0 and column 4,
    where Foo.res is being edited and the editor content is in file current.res.

    ./run.exe complete src/Foo.res 0 4 current.res

  hover: get inferred type for Foo.res at line 10 column 2:

    ./run.exe hover src/Foo.res 10 2

  definition: get definition for item in Foo.res at line 10 column 2:

    ./run.exe definition src/Foo.res 10 2

  references: get references to item in Foo.res at line 10 column 2:

  ./run.exe references src/Foo.res 10 2

  documentSymbol: get all symbols in Foo.res:

  ./run.exe documentSymbol src/Foo.res 
  |}

let main () =
  match Array.to_list Sys.argv with
  | [_; "complete"; path; line; col; currentFile] ->
    Commands.complete ~path ~line:(int_of_string line) ~col:(int_of_string col)
      ~currentFile
  | [_; "hover"; path; line; col] ->
    Commands.hover ~path ~line:(int_of_string line) ~col:(int_of_string col)
  | [_; "definition"; path; line; col] ->
    Commands.definition ~path ~line:(int_of_string line)
      ~col:(int_of_string col)
  | [_; "references"; path; line; col] ->
    Commands.references ~path ~line:(int_of_string line)
      ~col:(int_of_string col)
  | [_; "documentSymbol"; path] -> Commands.documentSymbol ~path
  | _ :: "dump" :: files -> Commands.dump files
  | [_; "test"; path] -> Commands.test ~path
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1

;;
main ()
