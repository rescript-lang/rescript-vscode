open SharedTypes

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
  (* TODO: Handle with tuples let-bindings *)
  let value_binding (iterator : Ast_iterator.iterator)
      (vb : Parsetree.value_binding) =
    (match vb.pvb_pat.ppat_desc with
    | Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _) ->
      symbols := vb.pvb_pat.ppat_loc :: !symbols
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
  |> List.rev_map (fun locOfName ->
         let range = Utils.cmtLocToRange locOfName in
         match Cmt.fullFromPath ~path with
         | Some full -> (
           match
             References.getLocItem ~full
               ~pos:(range.start.line, range.start.character + 1)
               ~debug
           with
           | Some locItem ->
             let position : Protocol.position =
               {line = range.start.line; character = range.end_.character}
             in
             let typeHint =
               match locItemToTypeHint locItem ~full with
               | Some hint -> hint
               | None -> "Unknown type"
             in
             let result =
               Protocol.stringifyHint
                 {
                   kind = 1;
                   position;
                   tooltip =
                     {kind = "markdown"; value = Hover.codeBlock typeHint};
                   paddingLeft = false;
                   paddingRight = false;
                   label = ": " ^ typeHint;
                 }
             in
             Some result
           | None -> None)
         | None -> None)
  |> List.filter_map (fun x -> x)
