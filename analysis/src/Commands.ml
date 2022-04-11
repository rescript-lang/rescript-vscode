open SharedTypes

let posInLoc ~pos ~loc =
  Utils.tupleOfLexing loc.Location.loc_start <= pos
  && pos < Utils.tupleOfLexing loc.loc_end

let extractJsxProps ~text (expr : Parsetree.expression) =
  let rec extractLabelPos ~pos ~i str =
    if i < String.length str then
      match str.[i] with
      | ' ' | '\r' | '\t' ->
        extractLabelPos ~pos:(fst pos, snd pos + 1) ~i:(i + 1) str
      | '\n' -> extractLabelPos ~pos:(fst pos + 1, 0) ~i:(i + 1) str
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> Some pos
      | _ -> None
    else None
  in
  match expr.pexp_desc with
  | Pexp_apply (eComp, args) ->
    (* To check if tha prop is punned, take the string between the last prop
         (or the end of e) and the expr in the arg.
       If it's whitespace only, then it's a punned <C prop />.
       If it's "?" then it's a punned optional <C ?prop />.
       Otherwise it should be "id =" perhaps followed by "?".
    *)
    let rec processProps ~lastOffset ~lastPos args =
      match args with
      | (Asttypes.Labelled "children", _) :: _ -> []
      | ((Labelled s | Optional s), (eProp : Parsetree.expression)) :: rest -> (
        let ePosStart = Utils.tupleOfLexing eProp.pexp_loc.loc_start in
        let ePosEnd = Utils.tupleOfLexing eProp.pexp_loc.loc_end in
        match
          ( PartialParser.positionToOffset text ePosStart,
            PartialParser.positionToOffset text ePosEnd )
        with
        | Some offsetStart, Some offsetEnd ->
          let label = String.sub text lastOffset (offsetStart - lastOffset) in
          let labelPos =
            match extractLabelPos ~pos:lastPos ~i:0 label with
            | Some pos -> pos
            | None -> (* Must be punned *) ePosStart
          in
          (s, labelPos, eProp)
          :: processProps ~lastOffset:offsetEnd ~lastPos:ePosEnd rest
        | _ -> assert false)
      | _ -> []
    in
    let posAfterCompName = Utils.tupleOfLexing eComp.pexp_loc.loc_end in
    let offsetAfterCompName =
      match PartialParser.positionToOffset text posAfterCompName with
      | None -> assert false
      | Some offset -> offset
    in
    args
    |> processProps ~lastOffset:offsetAfterCompName ~lastPos:posAfterCompName
  | _ -> []

let completionWithParser ~debug ~path ~pos ~currentFile ~textOpt =
  let text = match textOpt with Some text -> text | None -> assert false in
  let offset =
    match PartialParser.positionToOffset text pos with
    | Some offset -> offset
    | None -> assert false
  in
  let line, col = pos in
  let offsetNoWhite = PartialParser.skipWhite text offset in
  let posNoWhite = (line, max 0 col - offset + offsetNoWhite) in

  if Filename.check_suffix path ".res" then (
    let parser =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:false
    in
    let found = ref false in
    let expr (iterator : Ast_iterator.iterator) (expr : Parsetree.expression) =
      if posInLoc ~pos:posNoWhite ~loc:expr.pexp_loc then (
        found := true;
        let exprKind =
          match expr.pexp_desc with
          | Pexp_apply _ when Res_parsetree_viewer.isJsxExpression expr ->
            let props = extractJsxProps ~text expr in
            " JSX "
            ^ (props
              |> List.map (fun (lbl, lblPos, (eProp : Parsetree.expression)) ->
                     Printf.sprintf "(%s:%s e:%s)" lbl
                       (SemanticTokens.posToString lblPos)
                       (SemanticTokens.locToString eProp.pexp_loc))
              |> String.concat ", ")
          | _ -> ""
        in
        if debug then
          Printf.printf "posNoWhite:%s Found expr:%s%s\n"
            (SemanticTokens.posToString posNoWhite ^ " ")
            (SemanticTokens.locToString expr.pexp_loc)
            exprKind);
      Ast_iterator.default_iterator.expr iterator expr
    in
    let {Res_driver.parsetree = structure} = parser ~filename:currentFile in
    let iterator = {Ast_iterator.default_iterator with expr} in
    iterator.structure iterator structure |> ignore;
    if not !found then if debug then Printf.printf "XXX Not found!\n")

let completion ~debug ~path ~line ~col ~currentFile =
  let pos = (line, col) in

  let result =
    let textOpt = Files.readFile currentFile in
    completionWithParser ~debug ~path ~pos ~currentFile ~textOpt;
    let completionItems =
      match NewCompletions.getCompletable ~textOpt ~pos with
      | None -> []
      | Some (completable, rawOpens) -> (
        (* Only perform expensive ast operations if there are completables *)
        match Cmt.fromPath ~path with
        | None -> []
        | Some full ->
          NewCompletions.computeCompletions ~completable ~full ~pos ~rawOpens)
    in
    completionItems
    |> List.map Protocol.stringifyCompletionItem
    |> Protocol.array
  in
  print_endline result

let hover ~path ~line ~col =
  let result =
    match Cmt.fromPath ~path with
    | None -> Protocol.null
    | Some ({file} as full) -> (
      match References.getLocItem ~full ~line ~col with
      | None -> Protocol.null
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
            let isInterface = file.uri |> Uri2.isInterface in
            let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
              (not isInterface) && pos_lnum = 1 && pos_cnum - pos_bol = 0
            in
            (* Skip if range is all zero, unless it's a module *)
            (not isModule) && posIsZero loc.loc_start && posIsZero loc.loc_end
        in
        if skipZero then Protocol.null
        else
          let hoverText = Hover.newHover ~full locItem in
          match hoverText with
          | None -> Protocol.null
          | Some s -> Protocol.stringifyHover {contents = s}))
  in
  print_endline result

let definition ~path ~line ~col =
  let locationOpt =
    match Cmt.fromPath ~path with
    | None -> None
    | Some ({file} as full) -> (
      match References.getLocItem ~full ~line ~col with
      | None -> None
      | Some locItem -> (
        match References.definitionForLocItem ~full locItem with
        | None -> None
        | Some (uri, loc) ->
          let isInterface = file.uri |> Uri2.isInterface in
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
              {
                Protocol.uri = Uri2.toString uri;
                range = Utils.cmtLocToRange loc;
              }))
  in
  print_endline
    (match locationOpt with
    | None -> Protocol.null
    | Some location -> location |> Protocol.stringifyLocation)

let typeDefinition ~path ~line ~col =
  let maybeLocation =
    match Cmt.fromPath ~path with
    | None -> None
    | Some full -> (
      match References.getLocItem ~full ~line ~col with
      | None -> None
      | Some locItem -> (
        match References.typeDefinitionForLocItem ~full locItem with
        | None -> None
        | Some (uri, loc) ->
          Some
            {Protocol.uri = Uri2.toString uri; range = Utils.cmtLocToRange loc})
      )
  in
  print_endline
    (match maybeLocation with
    | None -> Protocol.null
    | Some location -> location |> Protocol.stringifyLocation)

let references ~path ~line ~col =
  let allLocs =
    match Cmt.fromPath ~path with
    | None -> []
    | Some full -> (
      match References.getLocItem ~full ~line ~col with
      | None -> []
      | Some locItem ->
        let allReferences = References.allReferencesForLocItem ~full locItem in
        allReferences
        |> List.fold_left
             (fun acc {References.uri = uri2; locOpt} ->
               let loc =
                 match locOpt with
                 | Some loc -> loc
                 | None -> Uri2.toTopLevelLoc uri2
               in
               Protocol.stringifyLocation
                 {uri = Uri2.toString uri2; range = Utils.cmtLocToRange loc}
               :: acc)
             [])
  in
  print_endline
    (if allLocs = [] then Protocol.null
    else "[\n" ^ (allLocs |> String.concat ",\n") ^ "\n]")

let rename ~path ~line ~col ~newName =
  let result =
    match Cmt.fromPath ~path with
    | None -> Protocol.null
    | Some full -> (
      match References.getLocItem ~full ~line ~col with
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
                 let path = Uri2.toPath uri in
                 let dir = Filename.dirname path in
                 let newPath =
                   Filename.concat dir (newName ^ Filename.extension path)
                 in
                 let newUri = Uri2.fromPath newPath in
                 Protocol.
                   {
                     oldUri = uri |> Uri2.toString;
                     newUri = newUri |> Uri2.toString;
                   })
        in
        let textDocumentEdits =
          let module StringMap = Misc.StringMap in
          let textEditsByUri =
            referencesToItems
            |> List.map (fun (uri, loc) -> (Uri2.toString uri, loc))
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
    (* if List.length diagnostics > 0 then ""
       else *)
    Res_printer.printImplementation !Res_cli.ResClflags.width structure comments
  else if Filename.check_suffix path ".resi" then
    let {Res_driver.parsetree = signature; comments; diagnostics} =
      Res_driver.parsingEngine.parseInterface ~forPrinter:true ~filename:path
    in
    if List.length diagnostics > 0 then ""
    else Res_printer.printInterface !Res_cli.ResClflags.width signature comments
  else ""

let test ~path =
  Uri2.stripPath := true;
  match Files.readFile path with
  | None -> assert false
  | Some text ->
    let lines = text |> String.split_on_char '\n' in
    let processLine i line =
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
            definition ~path ~line ~col
          | "typ" ->
            print_endline
              ("TypeDefinition " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            typeDefinition ~path ~line ~col
          | "hov" ->
            print_endline
              ("Hover " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            hover ~path ~line ~col
          | "ref" ->
            print_endline
              ("References " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            references ~path ~line ~col
          | "doc" ->
            print_endline ("DocumentSymbol " ^ path);
            DocumentSymbol.command ~path
          | "ren" ->
            let newName = String.sub rest 4 (len - mlen - 4) in
            let () =
              print_endline
                ("Rename " ^ path ^ " " ^ string_of_int line ^ ":"
               ^ string_of_int col ^ " " ^ newName)
            in
            rename ~path ~line ~col ~newName
          | "com" ->
            print_endline
              ("Complete " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let currentFile, cout = Filename.open_temp_file "def" "txt" in
            lines
            |> List.iteri (fun j l ->
                   let lineToOutput =
                     if j == i then String.sub rest 3 (len - mlen - 3) else l
                   in
                   Printf.fprintf cout "%s\n" lineToOutput);
            let line = line + 1 in
            let col = len - mlen - 3 in
            close_out cout;
            completion ~debug:true ~path ~line ~col ~currentFile;
            Sys.remove currentFile
          | "hig" ->
            print_endline ("Highlight " ^ path);
            SemanticTokens.command ~debug:true
              ~emitter:(SemanticTokens.Token.createEmitter ())
              ~path
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
          | _ -> ());
          print_newline ())
    in
    lines |> List.iteri processLine
