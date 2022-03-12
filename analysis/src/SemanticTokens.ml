module Token = struct
  type legend = {tokenTypes : string array; tokenModifiers : string array}

  (* This needs to stay synced with the same legend in `server.ts` *)
  (* See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens *)
  type tokenType = Keyword
  type tokenModifiers = NoModifier

  let tokenTypeToString = function Keyword -> "0"
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

let emitJsxOpen ~id ~debug ~loc emitter =
  if debug then Printf.printf "JsxOpen: %s %s\n" id (locToString loc);
  emitter |> emitFromLoc ~loc ~type_:Token.Keyword

let emitVariable ~id ~debug ~loc emitter =
  if debug then Printf.printf "Variable: %s %s\n" id (locToString loc);
  emitter |> emitFromLoc ~loc ~type_:Token.Keyword

let emitJsxClose ~debug ~posStart ~posEnd emitter =
  let l1, c1 = posStart and l2, c2 = posEnd in
  if debug then Printf.printf "JsxClose: (%d,%d)->(%d,%d)\n" l1 c1 l2 c2;
  emitter |> emitFromPos posStart posEnd ~type_:Token.Keyword

let parser ~debug ~emitter ~path =
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
  let processTypeArg (coreType : Parsetree.core_type) =
    if debug then Printf.printf "TypeArg: %s\n" (locToString coreType.ptyp_loc)
  in
  let isLowercaseId id =
    let c = id.[0] in
    c == '_' || (c >= 'a' && c <= 'z')
  in
  let typ (mapper : Ast_mapper.mapper) (coreType : Parsetree.core_type) =
    match coreType.ptyp_desc with
    | Ptyp_constr (_lident, args) ->
      args |> List.iter processTypeArg;
      Ast_mapper.default_mapper.typ mapper coreType
    | _ -> Ast_mapper.default_mapper.typ mapper coreType
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
    | Pexp_ident {txt = Lident id; loc} ->
      if isLowercaseId id then emitter |> emitVariable ~id ~debug ~loc;
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
      emitter |> emitJsxOpen ~id:(jsxName lident.txt) ~debug ~loc:pexp_loc;
      (if not (isSelfClosing args) then
       let lineStart, colStart = Utils.tupleOfLexing pexp_loc.loc_start in
       let lineEnd, colEnd = Utils.tupleOfLexing pexp_loc.loc_end in
       let length = if lineStart = lineEnd then colEnd - colStart else 0 in
       let lineEndWhole, colEndWhole = Utils.tupleOfLexing e.pexp_loc.loc_end in
       if length > 0 && colEndWhole > length then
         emitter
         |> emitJsxClose ~debug
              ~posStart:(lineEndWhole, colEndWhole - length - 1)
              ~posEnd:(lineEndWhole, colEndWhole - 1));
      Ast_mapper.default_mapper.expr mapper e
    | Pexp_apply ({pexp_loc}, _) when Res_parsetree_viewer.isBinaryExpression e
      ->
      if debug then Printf.printf "BinaryExp: %s\n" (locToString pexp_loc);
      Ast_mapper.default_mapper.expr mapper e
    | _ -> Ast_mapper.default_mapper.expr mapper e
  in

  let mapper = {Ast_mapper.default_mapper with expr; pat; typ} in

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
