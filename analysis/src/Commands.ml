open SharedTypes

let dumpLocations ~full =
  full.extra.locItems
  |> List.map (fun locItem ->
         let locItemTxt = locItemToString locItem in
         let hoverText = Hover.newHover ~full locItem in
         let hover =
           match hoverText with None -> "" | Some s -> String.escaped s
         in
         let uriLocOpt = References.definitionForLocItem ~full locItem in
         let def =
           match uriLocOpt with
           | None -> Protocol.null
           | Some (uri2, loc) ->
             Protocol.stringifyLocation
               {uri = Uri2.toString uri2; range = Utils.cmtLocToRange loc}
         in
         Protocol.stringifyRange (Utils.cmtLocToRange locItem.loc)
         ^ "\n  Hover: " ^ hover ^ "\n  Definition: " ^ def ^ "\n locItem: "
         ^ locItemTxt)
  |> String.concat "\n\n"

let dump files =
  Shared.cacheTypeToString := true;
  files
  |> List.iter (fun path ->
         let result =
           match Cmt.fromPath ~path with
           | None -> "[]"
           | Some full -> dumpLocations ~full
         in
         print_endline result)

let completion ~path ~line ~col ~currentFile =
  let pos = (line, col) in
  let result =
    let textOpt = Files.readFile currentFile in
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

let documentSymbol ~path =
  let result =
    match Cmt.fromPath ~path with
    | None -> Protocol.null
    | Some {file} ->
      let open SharedTypes in
      let rec getItems topLevel =
        let rec getItem = function
          | Module.Value v -> (v |> variableKind, [])
          | Type (t, _) -> (t.decl |> declarationKind, [])
          | Module (Structure contents) -> (Module, getItems contents.items)
          | Module (Constraint (_, modTypeItem)) -> getItem (Module modTypeItem)
          | Module (Ident _) -> (Module, [])
        in
        let fn {Module.name; extentLoc; kind} =
          let item, siblings = getItem kind in
          if extentLoc.loc_ghost then siblings
          else (name, extentLoc, item) :: siblings
        in
        let x = topLevel |> List.map fn |> List.concat in
        x
      in
      let allSymbols =
        getItems file.structure.items
        |> List.map (fun (name, loc, kind) ->
               Protocol.stringifyDocumentSymbolItem
                 {
                   name;
                   location =
                     {
                       uri = Uri2.toString (Uri2.fromPath path);
                       range = Utils.cmtLocToRange loc;
                     };
                   kind = symbolKind kind;
                 })
      in
      "[\n" ^ (allSymbols |> String.concat ",\n") ^ "\n]"
  in
  print_endline result

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

module Token = struct
  type legend = {tokenTypes : string array; tokenModifiers : string array}

  (* This needs to stay synced with the same legend in `server.ts` *)
  (* See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens *)
  type tokenType = Keyword
  type tokenModifiers = NoModifier

  let tokenTypeToString = function Keyword -> "0"
  let tokenModifiersToString = function NoModifier -> "0"

  type emitter = {
    buf : Buffer.t;
    mutable lastLine : int;
    mutable lastChar : int;
  }

  let createEmitter () = {buf = Buffer.create 0; lastLine = 0; lastChar = 0}

  let emit ~line ~char ~length ~type_ ?(modifiers = NoModifier) e =
    let deltaLine = line - e.lastLine in
    let deltaChar = char - e.lastChar in
    e.lastLine <- line;
    e.lastChar <- char;
    if Buffer.length e.buf > 0 then Buffer.add_char e.buf ',';
    Buffer.add_string e.buf
      (string_of_int deltaLine ^ "," ^ string_of_int deltaChar ^ ","
     ^ string_of_int length ^ "," ^ tokenTypeToString type_ ^ ","
      ^ tokenModifiersToString modifiers);
    ()
end

let semanticTokensTest () =
  let emitter = Token.createEmitter () in
  emitter |> Token.emit ~line:0 ~char:0 ~length:3 ~type_:Token.Keyword;
  emitter |> Token.emit ~line:1 ~char:2 ~length:3 ~type_:Token.Keyword;
  Printf.printf "{\"data\":[%s]}" (Buffer.contents emitter.buf)

let parser ~path =
  if Filename.check_suffix path ".res" then (
    let parser =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:false
    in
    let {Res_driver.parsetree = structure; diagnostics} =
      parser ~filename:path
    in
    Printf.printf "structure items:%d diagnostics:%d \n" (List.length structure)
      (List.length diagnostics);

    let jsxName lident =
      let rec flatten acc lident =
        match lident with
        | Longident.Lident txt -> txt :: acc
        | Ldot (lident, txt) ->
          let acc = if txt = "createElement" then acc else txt :: acc in
          flatten acc lident
        | _ -> acc
      in
      match lident with
      | Longident.Lident txt -> txt
      | _ as lident ->
        let segments = flatten [] lident in
        segments |> String.concat "."
    in
    let locToString (loc : Location.t) =
      let lineStart, colStart = Utils.tupleOfLexing loc.loc_start in
      let lineEnd, colEnd = Utils.tupleOfLexing loc.loc_end in
      Printf.sprintf "(%d,%d)->(%d,%d)" lineStart colStart lineEnd colEnd
    in
    let rec processExpression (expr : Parsetree.expression) =
      match expr.pexp_desc with
      | Pexp_apply ({pexp_desc = Pexp_ident lident; pexp_loc}, args)
        when Res_parsetree_viewer.isJsxExpression expr ->
        let rec isSelfClosing args =
          match args with
          | [] -> false
          | [
           ( Asttypes.Labelled "children",
             {
               Parsetree.pexp_desc =
                 Pexp_construct ({txt = Longident.Lident "[]"}, None);
             } );
           _;
          ] ->
            true
          | _ :: rest -> isSelfClosing rest
        in
        Printf.printf "JsxOpen: %s %s\n" (jsxName lident.txt)
          (locToString pexp_loc);
        (if not (isSelfClosing args) then
         let lineStart, colStart = Utils.tupleOfLexing pexp_loc.loc_start in
         let lineEnd, colEnd = Utils.tupleOfLexing pexp_loc.loc_end in
         let size = if lineStart = lineEnd then colEnd - colStart else 0 in
         let lineEndWhole, colEndWhole =
           Utils.tupleOfLexing expr.pexp_loc.loc_end
         in
         if size > 0 && colEndWhole > size then
           Printf.printf "JsxClose: (%d,%d)->(%d,%d)\n" lineEndWhole
             (colEndWhole - size - 1)
             lineEndWhole (colEndWhole - 1));
        args |> List.iter (fun (_lbl, e) -> processExpression e)
      | Pexp_apply ({pexp_loc}, args)
        when Res_parsetree_viewer.isBinaryExpression expr ->
        Printf.printf "BinaryExp: %s\n" (locToString pexp_loc);
        args |> List.iter (fun (_lbl, e) -> processExpression e)
      | Pexp_apply (f, args) ->
        processExpression f;
        args |> List.iter (fun (_lbl, e) -> processExpression e)
      | Pexp_construct (_lidend, expOpt) -> processExpressionOption expOpt
      | Pexp_tuple exprs -> exprs |> List.iter processExpression
      | Pexp_ident _ -> ()
      | Pexp_constant _ -> ()
      | Pexp_unreachable -> assert false
      | Pexp_let (_, _, _) -> assert false
      | Pexp_function _ -> assert false
      | Pexp_fun (_, _, _, _) -> assert false
      | Pexp_match (_, _) -> assert false
      | Pexp_try (_, _) -> assert false
      | Pexp_variant (_, _) -> assert false
      | Pexp_record (_, _) -> assert false
      | Pexp_field (_, _) -> assert false
      | Pexp_setfield (_, _, _) -> assert false
      | Pexp_array _ -> assert false
      | Pexp_ifthenelse (_, _, _) -> assert false
      | Pexp_sequence (_, _) -> assert false
      | Pexp_while (_, _) -> assert false
      | Pexp_for (_, _, _, _, _) -> assert false
      | Pexp_constraint (_, _) -> assert false
      | Pexp_coerce (_, _, _) -> assert false
      | Pexp_send (_, _) -> assert false
      | Pexp_new _ -> assert false
      | Pexp_setinstvar (_, _) -> assert false
      | Pexp_override _ -> assert false
      | Pexp_letmodule (_, _, _) -> assert false
      | Pexp_letexception (_, _) -> assert false
      | Pexp_assert _ -> assert false
      | Pexp_lazy _ -> assert false
      | Pexp_poly (_, _) -> assert false
      | Pexp_object _ -> assert false
      | Pexp_newtype (_, _) -> assert false
      | Pexp_pack _ -> assert false
      | Pexp_open (_, _, _) -> assert false
      | Pexp_extension _ -> assert false
    and processExpressionOption = function
      | None -> ()
      | Some e -> processExpression e
    in

    let processValueBinding (binding : Parsetree.value_binding) =
      processExpression binding.pvb_expr
    in
    let rec processTypeArg (coreType : Parsetree.core_type) =
      Printf.printf "TypeArg: %s\n" (locToString coreType.ptyp_loc);
      processCoreType coreType
    and processCoreType (coreType : Parsetree.core_type) =
      match coreType.ptyp_desc with
      | Ptyp_constr (_lident, args) -> args |> List.iter processTypeArg
      | _ -> ()
    in
    let processTypeDeclaration (typeDecl : Parsetree.type_declaration) =
      match typeDecl.ptype_manifest with
      | Some t -> processCoreType t
      | None -> ()
    in
    let processStructureItem (item : Parsetree.structure_item) =
      match item.pstr_desc with
      | Pstr_value (_recFlag, bindings) ->
        bindings |> List.iter processValueBinding
      | Pstr_type (_recFlat, typeDecls) ->
        typeDecls |> List.iter processTypeDeclaration
      | _ -> ()
    in
    structure |> List.iter processStructureItem)
  else
    let parser = Res_driver.parsingEngine.parseInterface ~forPrinter:false in
    let {Res_driver.parsetree = signature; diagnostics} =
      parser ~filename:path
    in
    Printf.printf "signature items:%d diagnostics:%d \n" (List.length signature)
      (List.length diagnostics)

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
            documentSymbol ~path
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
            completion ~path ~line ~col ~currentFile;
            Sys.remove currentFile
          | "par" ->
            print_endline ("Parse " ^ path);
            parser ~path
          | _ -> ());
          print_newline ())
    in
    lines |> List.iteri processLine
