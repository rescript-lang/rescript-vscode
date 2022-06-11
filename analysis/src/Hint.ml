open SharedTypes

type kind =
  | Module
  | Property
  | Constructor
  | Function
  | Variable
  | Constant
  | String
  | Number
  | EnumMember
  | TypeParameter

let typeHintKindToNumber = function Variable -> 1 | _ -> 2
let getPaddingFromKind = function Variable -> 4 | _ -> 0

let parseTypeReturn (t : Types.type_expr) =
  let typeString = Shared.typeToString t in
  match Str.split (Str.regexp "=>") typeString with
  | x :: xs -> List.fold_left (fun _ y -> y) x xs |> String.trim
  | _ -> typeString

let locItemToTypeHint ~full:{file; package} locItem =
  match locItem.locType with
  | Constant t ->
    Some
      (match t with
      | Const_int _ -> "int"
      | Const_char _ -> "char"
      | Const_string _ -> "string"
      | Const_float _ -> "float"
      | Const_int32 _ -> "int32"
      | Const_int64 _ -> "int64"
      | Const_nativeint _ -> "int")
  | Typed (_, t, locKind) ->
    Some
      (match References.definedForLoc ~file ~package locKind with
      | None -> parseTypeReturn t
      | Some (_, res) -> (
        match res with
        | `Declared -> parseTypeReturn t
        | `Constructor _ -> parseTypeReturn t
        | `Field -> parseTypeReturn t))
  | _ -> None

(* TODO: filter for range of lines*)
let inlay ~path ~pos ~debug =
  let symbols = ref [] in
  let rec exprKind (exp : Parsetree.expression) =
    match exp.pexp_desc with
    | Pexp_fun _ -> Function
    | Pexp_function _ -> Function
    | Pexp_constraint (e, _) -> exprKind e
    | Pexp_constant (Pconst_string _) -> String
    | Pexp_constant (Pconst_float _ | Pconst_integer _) -> Number
    | Pexp_constant _ -> Constant
    | _ -> Variable
  in
  (* TODO: Handle with tuples let-bindings *)
  let value_binding (iterator : Ast_iterator.iterator)
      (vb : Parsetree.value_binding) =
    (match vb.pvb_pat.ppat_desc with
    | Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _) ->
      symbols := (txt, vb.pvb_loc, exprKind vb.pvb_expr) :: !symbols
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator vb
  in
  let iterator = {Ast_iterator.default_iterator with value_binding} in
  (if Filename.check_suffix path ".res" then
   let parser =
     Res_driver.parsingEngine.parseImplementation ~forPrinter:false
   in
   let {Res_driver.parsetree = structure} = parser ~filename:path in
   iterator.structure iterator structure |> ignore
  else
    let parser = Res_driver.parsingEngine.parseInterface ~forPrinter:false in
    let {Res_driver.parsetree = signature} = parser ~filename:path in
    iterator.signature iterator signature |> ignore);
  !symbols
  |> List.rev_map (fun (name, loc, kind) ->
         let range = Utils.cmtLocToRange loc in
         (* TODO: find the ending or starting position of a let bindings *)
         let rangeEndCharacter =
          getPaddingFromKind kind + range.start.character + String.length name
         in
         let hintKind = typeHintKindToNumber kind in
         let position : Protocol.position =
           {line = range.start.line; character = rangeEndCharacter}
         in
         match Cmt.fullFromPath ~path with
         | None -> None
         | Some full -> (
           match
             References.getLocItem ~full
               ~pos:(position.line, position.character)
               ~debug
           with
           | None -> None
           | Some s -> (
             match locItemToTypeHint ~full s with
             | Some typeHint -> Some (typeHint, hintKind, position)
             | None -> None)))
  |> List.filter_map (fun x -> x)
  |> List.map (fun (typeHint, kind, position) ->
         Protocol.stringifyHint
           {
             kind;
             position;
             tooltip = {kind = "markdown"; value = Hover.codeBlock typeHint};
             paddingLeft = false;
             paddingRight = false;
             label = ": " ^ typeHint;
           })
