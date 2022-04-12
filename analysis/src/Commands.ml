open SharedTypes

let posInLoc ~pos ~loc =
  Utils.tupleOfLexing loc.Location.loc_start <= pos
  && pos < Utils.tupleOfLexing loc.loc_end

type prop = {
  name : string;
  posStart : int * int;
  posEnd : int * int;
  exp : Parsetree.expression;
}

type jsxProps = {
  componentPath : string list;
  props : prop list;
  childrenStart : (int * int) option;
}

let findJsxPropCompletable ~jsxProps ~endPos ~pos =
  let rec loop ~seen props =
    match props with
    | prop :: rest ->
      if prop.posEnd = pos then
        Some (PartialParser.Cjsx (jsxProps.componentPath, prop.name, seen))
      else if posInLoc ~pos ~loc:prop.exp.pexp_loc then None
      else loop ~seen:(seen @ [prop.name]) rest
    | [] ->
      let posAfterProps =
        match jsxProps.childrenStart with
        | Some childrenPos -> childrenPos
        | None -> endPos
      in
      if pos <= posAfterProps then
        Some (PartialParser.Cjsx (jsxProps.componentPath, "", seen))
      else None
  in
  loop ~seen:[] jsxProps.props

let extractJsxProps ~text ~(compName : Longident.t Location.loc) ~args =
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
  let flattenComponentName lid =
    let rec loop acc lid =
      match lid with
      | Longident.Lident txt -> txt :: acc
      | Ldot (lid, txt) ->
        let acc = if txt = "createElement" then acc else txt :: acc in
        loop acc lid
      | _ -> acc
    in
    loop [] lid
  in
  let rec processProps ~lastOffset ~lastPos ~acc args =
    match args with
    | (Asttypes.Labelled "children", {Parsetree.pexp_loc}) :: _ ->
      {
        componentPath = flattenComponentName compName.txt;
        props = List.rev acc;
        childrenStart =
          (if pexp_loc.loc_ghost then None
          else Some (Utils.tupleOfLexing pexp_loc.loc_start));
      }
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
        processProps
          ~acc:
            ({
               name = s;
               posStart = labelPos;
               posEnd = (fst labelPos, snd labelPos + String.length s);
               exp = eProp;
             }
            :: acc)
          ~lastOffset:offsetEnd ~lastPos:ePosEnd rest
      | _ -> assert false)
    | _ ->
      (* should not happen *)
      {componentPath = []; props = []; childrenStart = None}
  in
  let posAfterCompName = Utils.tupleOfLexing compName.loc.loc_end in
  let offsetAfterCompName =
    match PartialParser.positionToOffset text posAfterCompName with
    | None -> assert false
    | Some offset -> offset
  in
  args
  |> processProps ~lastOffset:offsetAfterCompName ~lastPos:posAfterCompName
       ~acc:[]

let completionWithParser ~debug ~path ~posCursor ~currentFile ~text =
  let offset =
    match PartialParser.positionToOffset text posCursor with
    | Some offset -> offset
    | None -> assert false
  in
  let offsetNoWhite = PartialParser.skipWhite text (offset - 1) in
  let posNoWhite =
    let line, col = posCursor in
    (line, max 0 col - offset + offsetNoWhite)
  in

  if Filename.check_suffix path ".res" then (
    let parser =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:false
    in
    let found = ref false in
    let expr (iterator : Ast_iterator.iterator) (expr : Parsetree.expression) =
      if posInLoc ~pos:posNoWhite ~loc:expr.pexp_loc then (
        found := true;
        if debug then
          Printf.printf "posCursor:%s posNoWhite:%s Found expr:%s\n"
            (SemanticTokens.posToString posCursor)
            (SemanticTokens.posToString posNoWhite)
            (SemanticTokens.locToString expr.pexp_loc);

        match expr.pexp_desc with
        | Pexp_apply ({pexp_desc = Pexp_ident compName}, args)
          when Res_parsetree_viewer.isJsxExpression expr ->
          let jsxProps = extractJsxProps ~text ~compName ~args in
          if debug then
            Printf.printf "JSX %s:%s childrenStart:%s %s\n"
              (jsxProps.componentPath |> String.concat ",")
              (SemanticTokens.locToString compName.loc)
              (match jsxProps.childrenStart with
              | None -> "None"
              | Some childrenPosStart ->
                SemanticTokens.posToString childrenPosStart)
              (jsxProps.props
              |> List.map (fun {name; posStart; posEnd; exp} ->
                     Printf.sprintf "(%s:%s->%s e:%s)" name
                       (SemanticTokens.posToString posStart)
                       (SemanticTokens.posToString posEnd)
                       (SemanticTokens.locToString exp.pexp_loc))
              |> String.concat ", ");
          let () =
            match
              findJsxPropCompletable ~jsxProps
                ~endPos:(Utils.tupleOfLexing expr.pexp_loc.loc_end)
                ~pos:posCursor
            with
            | Some completable ->
              ();
              (* if debug then
                Printf.printf "Found JSX completable %s\n"
                  (PartialParser.completableToString completable) *)
            | None -> ()
          in
          ()
        | _ -> ());
      Ast_iterator.default_iterator.expr iterator expr
    in
    let {Res_driver.parsetree = structure} = parser ~filename:currentFile in
    let iterator = {Ast_iterator.default_iterator with expr} in
    iterator.structure iterator structure |> ignore;
    if not !found then if debug then Printf.printf "XXX Not found!\n")

let completion ~debug ~path ~pos ~currentFile =
  let result =
    let textOpt = Files.readFile currentFile in
    match textOpt with
    | None | Some "" -> []
    | Some text ->
      completionWithParser ~debug ~path ~posCursor:pos ~currentFile ~text;
      let completionItems =
        match NewCompletions.getCompletable ~text ~pos with
        | None -> []
        | Some (completable, rawOpens) -> (
          if debug then
            Printf.printf "Completable: %s\n"
              (PartialParser.completableToString completable);
          (* Only perform expensive ast operations if there are completables *)
          match Cmt.fromPath ~path with
          | None -> []
          | Some full ->
            NewCompletions.computeCompletions ~completable ~full ~pos ~rawOpens)
      in
      completionItems
  in
  print_endline
    (result |> List.map Protocol.stringifyCompletionItem |> Protocol.array)

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
    let {Res_driver.parsetree = structure; comments} =
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
            completion ~debug:true ~path ~pos:(line, col) ~currentFile;
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
