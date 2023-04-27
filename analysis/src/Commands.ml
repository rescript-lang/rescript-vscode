let completion ~debug ~path ~pos ~currentFile =
  let completions =
    match
      Completions.getCompletions ~debug ~path ~pos ~currentFile ~forHover:false
    with
    | None -> []
    | Some (completions, _, _) -> completions
  in
  print_endline
    (completions
    |> List.map CompletionBackEnd.completionToItem
    |> List.map Protocol.stringifyCompletionItem
    |> Protocol.array)

let inlayhint ~path ~pos ~maxLength ~debug =
  let result =
    match Hint.inlay ~path ~pos ~maxLength ~debug with
    | Some hints -> hints |> Protocol.array
    | None -> Protocol.null
  in
  print_endline result

let codeLens ~path ~debug =
  let result =
    match Hint.codeLens ~path ~debug with
    | Some lens -> lens |> Protocol.array
    | None -> Protocol.null
  in
  print_endline result

let hover ~path ~pos ~currentFile ~debug ~supportsMarkdownLinks =
  let result =
    match Cmt.loadFullCmtFromPath ~path with
    | None -> Protocol.null
    | Some full -> (
      match References.getLocItem ~full ~pos ~debug with
      | None -> (
        if debug then
          Printf.printf
            "Nothing at that position. Now trying to use completion.\n";
        match
          Hover.getHoverViaCompletions ~debug ~path ~pos ~currentFile
            ~forHover:true ~supportsMarkdownLinks
        with
        | None -> Protocol.null
        | Some hover -> hover)
      | Some locItem -> (
        let isModule =
          match locItem.locType with
          | LModule _ | TopLevelModule _ -> true
          | TypeDefinition _ | Typed _ | Constant _ -> false
        in
        let uriLocOpt = References.definitionForLocItem ~full locItem in
        let skipZero =
          match uriLocOpt with
          | None -> false
          | Some (_, loc) ->
            let isInterface = full.file.uri |> Uri.isInterface in
            let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
              (not isInterface) && pos_lnum = 1 && pos_cnum - pos_bol = 0
            in
            (* Skip if range is all zero, unless it's a module *)
            (not isModule) && posIsZero loc.loc_start && posIsZero loc.loc_end
        in
        if skipZero then Protocol.null
        else
          let hoverText = Hover.newHover ~supportsMarkdownLinks ~full locItem in
          match hoverText with
          | None -> Protocol.null
          | Some s -> Protocol.stringifyHover s))
  in
  print_endline result

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
  let locationOpt =
    match Cmt.loadFullCmtFromPath ~path with
    | None -> None
    | Some full -> (
      match References.getLocItem ~full ~pos ~debug with
      | None -> None
      | Some locItem -> (
        match References.definitionForLocItem ~full locItem with
        | None -> None
        | Some (uri, loc) ->
          let isInterface = full.file.uri |> Uri.isInterface in
          let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
            (* range is zero *)
            pos_lnum = 1 && pos_cnum - pos_bol = 0
          in
          let isModule =
            match locItem.locType with
            | LModule _ | TopLevelModule _ -> true
            | TypeDefinition _ | Typed _ | Constant _ -> false
          in
          let skipLoc =
            (not isModule) && (not isInterface) && posIsZero loc.loc_start
            && posIsZero loc.loc_end
          in
          if skipLoc then None
          else
            Some
              {Protocol.uri = Uri.toString uri; range = Utils.cmtLocToRange loc}
        ))
  in
  print_endline
    (match locationOpt with
    | None -> Protocol.null
    | Some location -> location |> Protocol.stringifyLocation)

let typeDefinition ~path ~pos ~debug =
  let maybeLocation =
    match Cmt.loadFullCmtFromPath ~path with
    | None -> None
    | Some full -> (
      match References.getLocItem ~full ~pos ~debug with
      | None -> None
      | Some locItem -> (
        match References.typeDefinitionForLocItem ~full locItem with
        | None -> None
        | Some (uri, loc) ->
          Some
            {Protocol.uri = Uri.toString uri; range = Utils.cmtLocToRange loc}))
  in
  print_endline
    (match maybeLocation with
    | None -> Protocol.null
    | Some location -> location |> Protocol.stringifyLocation)

let references ~path ~pos ~debug =
  let allLocs =
    match Cmt.loadFullCmtFromPath ~path with
    | None -> []
    | Some full -> (
      match References.getLocItem ~full ~pos ~debug with
      | None -> []
      | Some locItem ->
        let allReferences = References.allReferencesForLocItem ~full locItem in
        allReferences
        |> List.fold_left
             (fun acc {References.uri = uri2; locOpt} ->
               let loc =
                 match locOpt with
                 | Some loc -> loc
                 | None -> Uri.toTopLevelLoc uri2
               in
               Protocol.stringifyLocation
                 {uri = Uri.toString uri2; range = Utils.cmtLocToRange loc}
               :: acc)
             [])
  in
  print_endline
    (if allLocs = [] then Protocol.null
    else "[\n" ^ (allLocs |> String.concat ",\n") ^ "\n]")

let rename ~path ~pos ~newName ~debug =
  let result =
    match Cmt.loadFullCmtFromPath ~path with
    | None -> Protocol.null
    | Some full -> (
      match References.getLocItem ~full ~pos ~debug with
      | None -> Protocol.null
      | Some locItem ->
        let allReferences = References.allReferencesForLocItem ~full locItem in
        let referencesToToplevelModules =
          allReferences
          |> Utils.filterMap (fun {References.uri = uri2; locOpt} ->
                 if locOpt = None then Some uri2 else None)
        in
        let referencesToItems =
          allReferences
          |> Utils.filterMap (function
               | {References.uri = uri2; locOpt = Some loc} -> Some (uri2, loc)
               | {locOpt = None} -> None)
        in
        let fileRenames =
          referencesToToplevelModules
          |> List.map (fun uri ->
                 let path = Uri.toPath uri in
                 let dir = Filename.dirname path in
                 let newPath =
                   Filename.concat dir (newName ^ Filename.extension path)
                 in
                 let newUri = Uri.fromPath newPath in
                 Protocol.
                   {
                     oldUri = uri |> Uri.toString;
                     newUri = newUri |> Uri.toString;
                   })
        in
        let textDocumentEdits =
          let module StringMap = Misc.StringMap in
          let textEditsByUri =
            referencesToItems
            |> List.map (fun (uri, loc) -> (Uri.toString uri, loc))
            |> List.fold_left
                 (fun acc (uri, loc) ->
                   let textEdit =
                     Protocol.
                       {range = Utils.cmtLocToRange loc; newText = newName}
                   in
                   match StringMap.find_opt uri acc with
                   | None -> StringMap.add uri [textEdit] acc
                   | Some prevEdits ->
                     StringMap.add uri (textEdit :: prevEdits) acc)
                 StringMap.empty
          in
          StringMap.fold
            (fun uri edits acc ->
              let textDocumentEdit =
                Protocol.{textDocument = {uri; version = None}; edits}
              in
              textDocumentEdit :: acc)
            textEditsByUri []
        in
        let fileRenamesString =
          fileRenames |> List.map Protocol.stringifyRenameFile
        in
        let textDocumentEditsString =
          textDocumentEdits |> List.map Protocol.stringifyTextDocumentEdit
        in
        "[\n"
        ^ (fileRenamesString @ textDocumentEditsString |> String.concat ",\n")
        ^ "\n]")
  in
  print_endline result

let format ~path =
  if Filename.check_suffix path ".res" then
    let {Res_driver.parsetree = structure; comments; diagnostics} =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:true
        ~filename:path
    in
    if List.length diagnostics > 0 then ""
    else
      Res_printer.printImplementation ~width:!Res_cli.ResClflags.width ~comments
        structure
  else if Filename.check_suffix path ".resi" then
    let {Res_driver.parsetree = signature; comments; diagnostics} =
      Res_driver.parsingEngine.parseInterface ~forPrinter:true ~filename:path
    in
    if List.length diagnostics > 0 then ""
    else
      Res_printer.printInterface ~width:!Res_cli.ResClflags.width ~comments
        signature
  else ""

let diagnosticSyntax ~path =
  print_endline (Diagnostics.document_syntax ~path |> Protocol.array)

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
            DocumentSymbol.command ~path
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
            Printf.printf "%s" (CreateInterface.command ~path ~cmiFile)
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
