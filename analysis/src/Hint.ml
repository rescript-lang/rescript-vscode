open SharedTypes

type inlayHintKind = Type | Parameter
let inlayKindToNumber = function
  | Type -> 1
  | Parameter -> 2

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
    let fromType typ =
      typ |> Shared.typeToString
      |> Str.global_replace (Str.regexp "[\r\n\t]") ""
    in
    Some
      (match References.definedForLoc ~file ~package locKind with
      | None -> fromType t
      | Some (_, res) -> (
        match res with
        | `Declared -> fromType t
        | `Constructor _ -> fromType t
        | `Field -> fromType t))
  | _ -> None

let inlay ~path ~debug =
  let hints = ref [] in
  let rec processFunction (exp : Parsetree.expression) =
    match exp.pexp_desc with
    | Pexp_fun (_, _, pat_exp, e) -> (
      match pat_exp with
      | {ppat_desc = Ppat_var _} ->
        hints := (pat_exp.ppat_loc, Type) :: !hints;
        processFunction e
      | _ -> processFunction e)
    | _ -> ()
  in
  let value_binding (iterator : Ast_iterator.iterator)
      (vb : Parsetree.value_binding) =
    (match vb with
    | {
     pvb_pat = {ppat_desc = Ppat_var _};
     pvb_expr =
       {
         pexp_desc =
           ( Pexp_constant _ | Pexp_tuple _ | Pexp_record _ | Pexp_variant _
           | Pexp_apply _ | Pexp_match _ | Pexp_construct _ | Pexp_ifthenelse _
           | Pexp_array _ | Pexp_ident _ | Pexp_try _ | Pexp_lazy _
           | Pexp_send _ | Pexp_field _ | Pexp_open _ );
       };
    } ->
      hints := (vb.pvb_pat.ppat_loc, Type) :: !hints
    | {pvb_pat = {ppat_desc = Ppat_tuple tuples}} ->
      List.iter
        (fun (tuple : Parsetree.pattern) ->
          hints := (tuple.ppat_loc, Type) :: !hints)
        tuples
    | {
     pvb_pat = {ppat_desc = Ppat_var _};
     pvb_expr = {pexp_desc = Pexp_fun (_, _, pat, e)};
    } ->
      (match pat with
      | {ppat_desc = Ppat_var _} -> hints := (pat.ppat_loc, Type) :: !hints
      | _ -> ());
      processFunction e
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
  !hints
  |> List.filter_map (fun (locOfName, hintKind) ->
         let range = Utils.cmtLocToRange locOfName in
         match Cmt.fullFromPath ~path with
         | None -> None
         | Some full -> (
           match
             References.getLocItem ~full
               ~pos:(range.start.line, range.start.character + 1)
               ~debug
           with
           | None -> None
           | Some locItem -> (
             let position : Protocol.position =
               {line = range.start.line; character = range.end_.character}
             in
             match locItemToTypeHint locItem ~full with
             | Some label ->
               let result =
                 Protocol.stringifyHint
                   {
                     kind = inlayKindToNumber hintKind;
                     position;
                     paddingLeft = true;
                     paddingRight = false;
                     label = ": " ^ label;
                   }
               in
               Some result
             | None -> None)))