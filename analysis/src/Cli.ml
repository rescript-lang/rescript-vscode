let help =
  {|
**Private CLI For rescript-vscode usage only**

API examples:
  ./rescript-editor-analysis.exe completion src/MyFile.res 0 4 currentContent.res
  ./rescript-editor-analysis.exe definition src/MyFile.res 9 3
  ./rescript-editor-analysis.exe typeDefinition src/MyFile.res 9 3
  ./rescript-editor-analysis.exe documentSymbol src/Foo.res
  ./rescript-editor-analysis.exe hover src/MyFile.res 10 2
  ./rescript-editor-analysis.exe references src/MyFile.res 10 2
  ./rescript-editor-analysis.exe rename src/MyFile.res 10 2 foo

Dev-time examples:
  ./rescript-editor-analysis.exe dump src/MyFile.res src/MyFile2.res
  ./rescript-editor-analysis.exe test src/MyFile.res

Note: positions are zero-indexed (start at 0 0), following LSP.
https://microsoft.github.io/language-server-protocol/specification#position

Options:
  completion: compute autocomplete for MyFile.res at line 0 and column 4,
    where MyFile.res is being edited and the editor content is in file current.res.

    ./rescript-editor-analysis.exe completion src/MyFile.res 0 4 current.res

  definition: get definition for item in MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe definition src/MyFile.res 10 2

  typeDefinition: get type definition for item in MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe typeDefinition src/MyFile.res 10 2

  documentSymbol: get all symbols declared in MyFile.res

    ./rescript-editor-analysis.exe documentSymbol src/MyFile.res

  hover: get inferred type for MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe hover src/MyFile.res 10 2

  references: get all references to item in MyFile.res at line 10 column 2:

    ./rescript-editor-analysis.exe references src/MyFile.res 10 2

  rename: rename all appearances of item in MyFile.res at line 10 column 2 with foo:

    ./rescript-editor-analysis.exe rename src/MyFile.res 10 2 foo

  semanticTokens: return token semantic highlighting info for MyFile.res

    ./rescript-editor-analysis.exe semanticTokens src/MyFile.res

  createInterface: print to stdout the interface file for src/MyFile.res

    ./rescript-editor-analysis.exe createInterface src/MyFile.res lib/bs/src/MyFile.cmi

  format: print to stdout the formatted version of the provided file

    ./rescript-editor-analysis.exe format src/MyFile.res

  test: run tests specified by special comments in file src/MyFile.res

    ./rescript-editor-analysis.exe test src/src/MyFile.res
|}

let main () =
  match Array.to_list Sys.argv with
  | [_; "completion"; path; line; col; currentFile] ->
    Commands.completion ~debug:false ~path
      ~pos:(int_of_string line, int_of_string col)
      ~currentFile
  | [_; "definition"; path; line; col] ->
    Commands.definition ~path
      ~pos:(int_of_string line, int_of_string col)
      ~debug:false
  | [_; "typeDefinition"; path; line; col] ->
    Commands.typeDefinition ~path
      ~pos:(int_of_string line, int_of_string col)
      ~debug:false
  | [_; "documentSymbol"; path] -> DocumentSymbol.command ~path
  | [_; "hover"; path; line; col; currentFile] ->
    Commands.hover ~path
      ~pos:(int_of_string line, int_of_string col)
      ~currentFile ~debug:false
  | [_; "inlayHint"; path; line_start; line_end] ->
    Commands.inlayhint ~path
      ~pos:(int_of_string line_start, int_of_string line_end)
      ~debug:false
  | [_; "codeAction"; path; line; col; currentFile] ->
    Commands.codeAction ~path
      ~pos:(int_of_string line, int_of_string col)
      ~currentFile ~debug:false
  | _ :: "reanalyze" :: _ ->
    let len = Array.length Sys.argv in
    for i = 1 to len - 2 do
      Sys.argv.(i) <- Sys.argv.(i + 1)
    done;
    Sys.argv.(len - 1) <- "";
    Reanalyze.cli ()
  | [_; "references"; path; line; col] ->
    Commands.references ~path
      ~pos:(int_of_string line, int_of_string col)
      ~debug:false
  | [_; "rename"; path; line; col; newName] ->
    Commands.rename ~path
      ~pos:(int_of_string line, int_of_string col)
      ~newName ~debug:false
  | [_; "semanticTokens"; currentFile] ->
    SemanticTokens.semanticTokens ~currentFile
  | [_; "createInterface"; path; cmiFile] ->
    Printf.printf "\"%s\""
      (Json.escape (CreateInterface.command ~path ~cmiFile))
  | [_; "format"; path] ->
    Printf.printf "\"%s\"" (Json.escape (Commands.format ~path))
  | [_; "test"; path] -> Commands.test ~path
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1
;;

main ()
