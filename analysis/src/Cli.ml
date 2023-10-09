let help =
  {|
**Private CLI For rescript-vscode usage only**

API examples:
  ./rescript-editor-analysis.exe completion src/MyFile.res 0 4 currentContent.res true
  ./rescript-editor-analysis.exe definition src/MyFile.res 9 3
  ./rescript-editor-analysis.exe typeDefinition src/MyFile.res 9 3
  ./rescript-editor-analysis.exe documentSymbol src/Foo.res
  ./rescript-editor-analysis.exe hover src/MyFile.res 10 2 true
  ./rescript-editor-analysis.exe references src/MyFile.res 10 2
  ./rescript-editor-analysis.exe rename src/MyFile.res 10 2 foo
  ./rescript-editor-analysis.exe diagnosticSyntax src/MyFile.res
  ./rescript-editor-analysis.exe inlayHint src/MyFile.res 0 3 25
  ./rescript-editor-analysis.exe codeLens src/MyFile.res

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

  hover: get inferred type for MyFile.res at line 10 column 2 (supporting markdown links):

    ./rescript-editor-analysis.exe hover src/MyFile.res 10 2 true

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

  diagnosticSyntax: print to stdout diagnostic for syntax

    ./rescript-editor-analysis.exe diagnosticSyntax src/MyFile.res

  inlayHint: get all inlay Hint between line 0 and 3 declared in MyFile.res. Last argument is maximum of character length for inlay hints

    ./rescript-editor-analysis.exe inlayHint src/MyFile.res 0 3 25

  codeLens: get all code lens entries for file src/MyFile.res

    ./rescript-editor-analysis.exe codeLens src/MyFile.res

  signatureHelp: get signature help if available for position at line 10 column 2 in src/MyFile.res

    ./rescript-editor-analysis.exe signatureHelp src/MyFile.res 10 2

  test: run tests specified by special comments in file src/MyFile.res

    ./rescript-editor-analysis.exe test src/src/MyFile.res
|}

let main () =
  match Array.to_list Sys.argv with
  | [_; "completion"; path; line; col; currentFile; supportsSnippets] ->
    (Cfg.supportsSnippets :=
       match supportsSnippets with
       | "true" -> true
       | _ -> false);
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
  | [_; "hover"; path; line; col; currentFile; supportsMarkdownLinks] ->
    Commands.hover ~path
      ~pos:(int_of_string line, int_of_string col)
      ~currentFile ~debug:false
      ~supportsMarkdownLinks:
        (match supportsMarkdownLinks with
        | "true" -> true
        | _ -> false)
  | [_; "signatureHelp"; path; line; col; currentFile] ->
    Commands.signatureHelp ~path
      ~pos:(int_of_string line, int_of_string col)
      ~currentFile ~debug:false
  | [_; "inlayHint"; path; line_start; line_end; maxLength] ->
    Commands.inlayhint ~path
      ~pos:(int_of_string line_start, int_of_string line_end)
      ~maxLength ~debug:false
  | [_; "codeLens"; path] -> Commands.codeLens ~path ~debug:false
  | [_; "extractDocs"; path] -> DocExtraction.extractDocs ~path ~debug:false
  | [_; "codeAction"; path; startLine; startCol; endLine; endCol; currentFile] ->
    Commands.codeAction ~path
      ~startPos:(int_of_string startLine, int_of_string startCol)
      ~endPos:(int_of_string endLine, int_of_string endCol)
      ~currentFile ~debug:false
  | [_; "codemod"; path; line; col; typ; hint] ->
    let typ =
      match typ with
      | "add-missing-cases" -> Codemod.AddMissingCases
      | _ -> raise (Failure "unsupported type")
    in
    let res =
      Codemod.transform ~path
        ~pos:(int_of_string line, int_of_string col)
        ~debug:false ~typ ~hint
      |> Json.escape
    in
    Printf.printf "\"%s\"" res
  | [_; "diagnosticSyntax"; path] -> Commands.diagnosticSyntax ~path
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
  | [_; "test"; path] ->
    Cfg.supportsSnippets := true;
    Commands.test ~path
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1
;;

main ()
