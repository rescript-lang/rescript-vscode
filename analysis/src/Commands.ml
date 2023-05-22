let completion ~debug ~path ~pos ~currentFile =
  Completions.complete ~debug ~path ~pos ~currentFile
  |> List.map Protocol.stringifyCompletionItem
  |> Protocol.array |> print_endline

let inlayhint ~path ~pos ~maxLength ~debug =
  let result =
    match Hint.inlay ~path ~pos ~maxLength ~debug with
    | Some hints -> hints |> List.map Protocol.stringifyHint |> Protocol.array
    | None -> Protocol.null
  in
  print_endline result

let codeLens ~path ~debug =
  let result =
    match Hint.codeLens ~path ~debug with
    | Some lens -> lens |> List.map Protocol.stringifyCodeLens |> Protocol.array
    | None -> Protocol.null
  in
  print_endline result

let hover ~path ~pos ~currentFile ~debug ~supportsMarkdownLinks =
  (match Hover.hover ~path ~pos ~currentFile ~debug ~supportsMarkdownLinks with
  | None -> Protocol.null
  | Some content -> content |> Protocol.stringifyHover)
  |> print_endline

let signatureHelp ~path ~pos ~currentFile ~debug =
  let result =
    match SignatureHelp.signatureHelp ~path ~pos ~currentFile ~debug with
    | None ->
      {Protocol.signatures = []; activeSignature = None; activeParameter = None}
    | Some res -> res
  in
  print_endline (Protocol.stringifySignatureHelp result)

let codeAction ~path ~pos ~currentFile ~debug =
  Xform.extractCodeActions ~path ~pos ~currentFile ~debug
  |> CodeActions.stringifyCodeActions |> print_endline

let definition ~path ~pos ~debug =
  (match Definition.definition ~path ~pos ~debug with
  | Some loc -> loc |> Protocol.stringifyLocation
  | None -> Protocol.null)
  |> print_endline

let typeDefinition ~path ~pos ~debug =
  (match TypeDefinition.typeDefinition ~path ~pos ~debug with
  | None -> Protocol.null
  | Some location -> location |> Protocol.stringifyLocation)
  |> print_endline

let references ~path ~pos ~debug =
  (match References.references ~path ~pos ~debug with
  | [] -> Protocol.null
  | locs -> locs |> List.map Protocol.stringifyLocation |> Protocol.array)
  |> print_endline

let rename ~path ~pos ~newName ~debug =
  let result =
    match Rename.rename ~path ~pos ~newName ~debug with
    | None -> Protocol.null
    | Some documentChanges ->
      documentChanges
      |> List.map (fun (changes : Protocol.documentChanges) ->
             match changes with
             | RenameFile renames ->
               renames |> List.map Protocol.stringifyRenameFile
             | TextDocumentEdit textDocumentEdits ->
               textDocumentEdits |> List.map Protocol.stringifyTextDocumentEdit)
      |> List.flatten |> Protocol.array
  in
  print_endline result

let format ~path =
  let result =
    match Formatter.format ~path with
    | Some text -> text
    | None -> ""
  in
  Printf.printf "\"%s\"" (Json.escape result)

let diagnosticSyntax ~path =
  Diagnostics.document_syntax ~path
  |> List.map Protocol.stringifyDiagnostic
  |> Protocol.array |> print_endline

let semanticTokens ~currentFile =
  Printf.printf "{\"data\":[%s]}" (SemanticTokens.semanticTokens ~currentFile)

let createInterface ~path ~cmiFile =
  (match CreateInterface.command ~path ~cmiFile with
  | Some text -> text
  | None -> "")
  |> Json.escape |> Printf.printf "\"%s\""

let documentSymbol ~path =
  DocumentSymbol.command ~path
  |> Protocol.stringifyDocumentSymbolItems |> print_endline

let test ~path =
  Uri.stripPath := true;
  match Files.readFile path with
  | None -> assert false
  | Some text ->
    let lines = text |> String.split_on_char '\n' in
    let processLine i line =
      let createCurrentFile () =
        let currentFile, cout = Filename.open_temp_file "def" "txt" in
        let removeLineComment l =
          let len = String.length l in
          let rec loop i =
            if i + 2 <= len && l.[i] = '/' && l.[i + 1] = '/' then Some (i + 2)
            else if i + 2 < len && l.[i] = ' ' then loop (i + 1)
            else None
          in
          match loop 0 with
          | None -> l
          | Some indexAfterComment ->
            String.make indexAfterComment ' '
            ^ String.sub l indexAfterComment (len - indexAfterComment)
        in
        lines
        |> List.iteri (fun j l ->
               let lineToOutput =
                 if j == i - 1 then removeLineComment l else l
               in
               Printf.fprintf cout "%s\n" lineToOutput);
        close_out cout;
        currentFile
      in
      if Str.string_match (Str.regexp "^ *//[ ]*\\^") line 0 then
        let matched = Str.matched_string line in
        let len = line |> String.length in
        let mlen = String.length matched in
        let rest = String.sub line mlen (len - mlen) in
        let line = i - 1 in
        let col = mlen - 1 in
        if mlen >= 3 then (
          (match String.sub rest 0 3 with
          | "db+" -> Log.verbose := true
          | "db-" -> Log.verbose := false
          | "def" ->
            print_endline
              ("Definition " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            definition ~path ~pos:(line, col) ~debug:true
          | "com" ->
            print_endline
              ("Complete " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let currentFile = createCurrentFile () in
            completion ~debug:true ~path ~pos:(line, col) ~currentFile;
            Sys.remove currentFile
          | "dce" ->
            print_endline ("DCE " ^ path);
            Reanalyze.RunConfig.runConfig.suppress <- ["src"];
            Reanalyze.RunConfig.runConfig.unsuppress <-
              [Filename.concat "src" "dce"];
            DceCommand.command ()
          | "doc" ->
            print_endline ("DocumentSymbol " ^ path);
            documentSymbol ~path
          | "hig" ->
            print_endline ("Highlight " ^ path);
            SemanticTokens.command ~debug:true
              ~emitter:(SemanticTokens.Token.createEmitter ())
              ~path
          | "hov" ->
            print_endline
              ("Hover " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let currentFile = createCurrentFile () in
            hover ~supportsMarkdownLinks:true ~path ~pos:(line, col)
              ~currentFile ~debug:true;
            Sys.remove currentFile
          | "she" ->
            print_endline
              ("Signature help " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let currentFile = createCurrentFile () in
            signatureHelp ~path ~pos:(line, col) ~currentFile ~debug:true;
            Sys.remove currentFile
          | "int" ->
            print_endline ("Create Interface " ^ path);
            let cmiFile =
              let open Filename in
              let ( ++ ) = concat in
              let name = chop_extension (basename path) ^ ".cmi" in
              let dir = dirname path in
              dir ++ parent_dir_name ++ "lib" ++ "bs" ++ "src" ++ name
            in
            Printf.printf "%s"
              (match CreateInterface.command ~path ~cmiFile with
              | Some text -> text
              | None -> "")
          | "ref" ->
            print_endline
              ("References " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            references ~path ~pos:(line, col) ~debug:true
          | "ren" ->
            let newName = String.sub rest 4 (len - mlen - 4) in
            let () =
              print_endline
                ("Rename " ^ path ^ " " ^ string_of_int line ^ ":"
               ^ string_of_int col ^ " " ^ newName)
            in
            rename ~path ~pos:(line, col) ~newName ~debug:true
          | "typ" ->
            print_endline
              ("TypeDefinition " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            typeDefinition ~path ~pos:(line, col) ~debug:true
          | "xfm" ->
            print_endline
              ("Xform " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let codeActions =
              Xform.extractCodeActions ~path ~pos:(line, col) ~currentFile:path
                ~debug:true
            in
            codeActions
            |> List.iter (fun {Protocol.title; edit = {documentChanges}} ->
                   Printf.printf "Hit: %s\n" title;
                   documentChanges
                   |> List.iter (fun {Protocol.edits} ->
                          edits
                          |> List.iter (fun {Protocol.range; newText} ->
                                 let indent =
                                   String.make range.start.character ' '
                                 in
                                 Printf.printf "%s\nnewText:\n%s<--here\n%s%s\n"
                                   (Protocol.stringifyRange range)
                                   indent indent newText)))
          | "dia" -> diagnosticSyntax ~path
          | "hin" ->
            (* Get all inlay Hint between line 1 and n.
               Don't get the first line = 0.
            *)
            let line_start = 1 in
            let line_end = 34 in
            print_endline
              ("Inlay Hint " ^ path ^ " " ^ string_of_int line_start ^ ":"
             ^ string_of_int line_end);
            inlayhint ~path ~pos:(line_start, line_end) ~maxLength:"25"
              ~debug:false
          | "cle" ->
            print_endline ("Code Lens " ^ path);
            codeLens ~path ~debug:false
          | "ast" ->
            print_endline
              ("Dump AST " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let currentFile = createCurrentFile () in
            DumpAst.dump ~pos:(line, col) ~currentFile;
            Sys.remove currentFile
          | _ -> ());
          print_newline ())
    in
    lines |> List.iteri processLine
