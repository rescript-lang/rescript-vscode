module Token = struct
  type legend = {tokenTypes : string array; tokenModifiers : string array}

  (* This needs to stay synced with the same legend in `server.ts` *)
  (* See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens *)
  type tokenType = Keyword | Variable | Type | Module
  type tokenModifiers = NoModifier

  let tokenTypeToString = function
    | Keyword -> "0"
    | Variable -> "1"
    | Type -> "2"
    | Module -> "3"

  let tokenModifiersToString = function NoModifier -> "0"

  type token = int * int * int * tokenType * tokenModifiers

  type emitter = {
    mutable tokens : token list;
    mutable lastLine : int;
    mutable lastChar : int;
  }

  let createEmitter () = {tokens = []; lastLine = 0; lastChar = 0}

  let add ~line ~char ~length ~type_ ?(modifiers = NoModifier) e =
    e.tokens <- (line, char, length, type_, modifiers) :: e.tokens

  let emitToken buf (line, char, length, type_, modifiers) e =
    let deltaLine = line - e.lastLine in
    let deltaChar = if deltaLine = 0 then char - e.lastChar else char in
    e.lastLine <- line;
    e.lastChar <- char;
    if Buffer.length buf > 0 then Buffer.add_char buf ',';
    Buffer.add_string buf
      (string_of_int deltaLine ^ "," ^ string_of_int deltaChar ^ ","
     ^ string_of_int length ^ "," ^ tokenTypeToString type_ ^ ","
      ^ tokenModifiersToString modifiers)

  let emit e =
    let sortedTokens =
      e.tokens
      |> List.sort (fun (l1, c1, _, _, _) (l2, c2, _, _, _) ->
             if l1 = l2 then compare c1 c2 else compare l1 l2)
    in
    let buf = Buffer.create 1 in
    sortedTokens |> List.iter (fun t -> e |> emitToken buf t);
    Buffer.contents buf
end

let locToPositions (loc : Location.t) =
  (Utils.tupleOfLexing loc.loc_start, Utils.tupleOfLexing loc.loc_end)

let posToString (loc, col) = Printf.sprintf "(%d,%d)" loc col

let locToString (loc : Location.t) =
  let posStart, posEnd = locToPositions loc in
  Printf.sprintf "%s->%s" (posToString posStart) (posToString posEnd)

let isLowercaseId id =
  let c = id.[0] in
  c == '_' || (c >= 'a' && c <= 'z')

let isUppercaseId id =
  let c = id.[0] in
  c >= 'A' && c <= 'Z'

let emitFromPos posStart posEnd ~type_ emitter =
  let length =
    if fst posStart = fst posEnd then snd posEnd - snd posStart else 0
  in
  if length > 0 then
    emitter
    |> Token.add ~line:(fst posStart) ~char:(snd posStart) ~length ~type_

let emitFromLoc ~loc ~type_ emitter =
  let posStart, posEnd = locToPositions loc in
  emitter |> emitFromPos posStart posEnd ~type_

let emitLongident ~backwards ~pos ~jsx ~lid ~debug emitter =
  let rec flatten acc lid =
    match lid with
    | Longident.Lident txt -> txt :: acc
    | Ldot (lid, txt) ->
      let acc = if jsx && txt = "createElement" then acc else txt :: acc in
      flatten acc lid
    | _ -> acc
  in
  let rec loop pos segments =
    match segments with
    | [id] when isUppercaseId id || isLowercaseId id ->
      if debug then Printf.printf "Lident: %s %s\n" id (posToString pos);
      emitter
      |> emitFromPos pos
           (fst pos, snd pos + String.length id)
           ~type_:(if isUppercaseId id then Module else Token.Variable)
    | id :: segments when isUppercaseId id || isLowercaseId id ->
      if debug then Printf.printf "Ldot: %s %s\n" id (posToString pos);
      let length = String.length id in
      emitter
      |> emitFromPos pos
           (fst pos, snd pos + length)
           ~type_:(if isUppercaseId id then Module else Token.Variable);
      loop (fst pos, snd pos + length + 1) segments
    | _ -> ()
  in
  let segments = flatten [] lid in
  let segments = if backwards then List.rev segments else segments in
  if backwards then (
    let totalLength = segments |> String.concat "." |> String.length in
    if snd pos >= totalLength then
      loop (fst pos, snd pos - totalLength) segments)
  else loop pos segments

let emitVariable ~id ~debug ~loc emitter =
  emitter
  |> emitLongident ~backwards:false
       ~pos:(Utils.tupleOfLexing loc.Location.loc_start)
       ~jsx:false ~lid:(Longident.Lident id) ~debug

let emitJsxOpen ~lid ~debug ~loc emitter =
  emitter
  |> emitLongident ~backwards:false
       ~pos:(Utils.tupleOfLexing loc.Location.loc_start)
       ~lid ~jsx:true ~debug

let emitJsxClose ~lid ~debug ~pos emitter =
  emitter |> emitLongident ~backwards:true ~pos ~lid ~jsx:true ~debug

let emitType ~id ~debug ~loc emitter =
  if debug then Printf.printf "Type: %s %s\n" id (locToString loc);
  emitter |> emitFromLoc ~loc ~type_:Token.Type

let parser ~debug ~emitter ~path =
  let processTypeArg (coreType : Parsetree.core_type) =
    if debug then Printf.printf "TypeArg: %s\n" (locToString coreType.ptyp_loc)
  in
  let typ (mapper : Ast_mapper.mapper) (coreType : Parsetree.core_type) =
    match coreType.ptyp_desc with
    | Ptyp_constr ({txt; loc}, args) ->
      (match txt with
      | Lident id -> emitter |> emitType ~id ~debug ~loc
      | _ -> ());
      args |> List.iter processTypeArg;
      Ast_mapper.default_mapper.typ mapper coreType
    | _ -> Ast_mapper.default_mapper.typ mapper coreType
  in
  let type_declaration (mapper : Ast_mapper.mapper)
      (tydecl : Parsetree.type_declaration) =
    emitter
    |> emitType ~id:tydecl.ptype_name.txt ~debug ~loc:tydecl.ptype_name.loc;
    Ast_mapper.default_mapper.type_declaration mapper tydecl
  in
  let pat (mapper : Ast_mapper.mapper) (p : Parsetree.pattern) =
    match p.ppat_desc with
    | Ppat_var {loc; txt = id} ->
      if isLowercaseId id then emitter |> emitVariable ~id ~debug ~loc;
      Ast_mapper.default_mapper.pat mapper p
    | _ -> Ast_mapper.default_mapper.pat mapper p
  in
  let expr (mapper : Ast_mapper.mapper) (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_ident {txt = lid; loc} ->
      emitter
      |> emitLongident ~backwards:false
           ~pos:(Utils.tupleOfLexing loc.loc_start)
           ~lid ~jsx:false ~debug;
      Ast_mapper.default_mapper.expr mapper e
    | Pexp_apply ({pexp_desc = Pexp_ident lident; pexp_loc}, args)
      when Res_parsetree_viewer.isJsxExpression e ->
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
      emitter |> emitJsxOpen ~lid:lident.txt ~debug ~loc:pexp_loc;
      (if not (isSelfClosing args) then
       let lineStart, colStart = Utils.tupleOfLexing pexp_loc.loc_start in
       let lineEnd, colEnd = Utils.tupleOfLexing pexp_loc.loc_end in
       let length = if lineStart = lineEnd then colEnd - colStart else 0 in
       let lineEndWhole, colEndWhole = Utils.tupleOfLexing e.pexp_loc.loc_end in
       if length > 0 && colEndWhole > length then
         emitter
         |> emitJsxClose ~debug ~lid:lident.txt
              ~pos:(lineEndWhole, colEndWhole - 1));
      (* only process again arguments, not the jsx label *)
      let _ =
        args
        |> List.map (fun (_lbl, arg) ->
               Ast_mapper.default_mapper.expr mapper arg)
      in
      e
    | Pexp_apply ({pexp_loc}, _) when Res_parsetree_viewer.isBinaryExpression e
      ->
      if debug then Printf.printf "BinaryExp: %s\n" (locToString pexp_loc);
      Ast_mapper.default_mapper.expr mapper e
    | _ -> Ast_mapper.default_mapper.expr mapper e
  in

  let mapper =
    {Ast_mapper.default_mapper with expr; pat; typ; type_declaration}
  in

  if Filename.check_suffix path ".res" then (
    let parser =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:false
    in
    let {Res_driver.parsetree = structure; diagnostics} =
      parser ~filename:path
    in
    if debug then
      Printf.printf "structure items:%d diagnostics:%d \n"
        (List.length structure) (List.length diagnostics);
    mapper.structure mapper structure |> ignore)
  else
    let parser = Res_driver.parsingEngine.parseInterface ~forPrinter:false in
    let {Res_driver.parsetree = signature; diagnostics} =
      parser ~filename:path
    in
    if debug then
      Printf.printf "signature items:%d diagnostics:%d \n"
        (List.length signature) (List.length diagnostics);
    mapper.signature mapper signature |> ignore

let testCommand ~currentFile =
  let emitter = Token.createEmitter () in
  parser ~emitter ~debug:false ~path:currentFile;
  (* emitter |> Token.add ~line:0 ~char:0 ~length:3 ~type_:Token.Keyword; *)
  Printf.printf "{\"data\":[%s]}" (Token.emit emitter)
