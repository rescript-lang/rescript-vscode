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

let inlay ~path ~pos ~maxLength ~debug =
  let maxlen = try Some (int_of_string maxLength) with Failure _ -> None in
  let hints = ref [] in
  let start_line, end_line = pos in
  let push loc kind =
    let range = Utils.cmtLocToRange loc in
    if start_line <= range.end_.line && end_line >= range.start.line then
      hints := (range, kind) :: !hints
  in
  let rec processFunction (exp : Parsetree.expression) =
    match exp.pexp_desc with
    | Pexp_fun (_, _, pat_exp, e) -> (
      match pat_exp with
      | {ppat_desc = Ppat_var _} ->
        push pat_exp.ppat_loc Type;
        processFunction e
      | _ -> processFunction e)
    | _ -> ()
  in
  let rec processPattern (pat : Parsetree.pattern) =
    match pat.ppat_desc with
    | Ppat_tuple pl -> pl |> List.iter processPattern
    | Ppat_record (fields, _) ->
      fields |> List.iter (fun (_, p) -> processPattern p)
    | Ppat_array fields -> fields |> List.iter processPattern
    | Ppat_var {loc} -> push loc Type
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
      push vb.pvb_pat.ppat_loc Type
    | {pvb_pat = {ppat_desc = Ppat_tuple _}} -> processPattern vb.pvb_pat
    | {pvb_pat = {ppat_desc = Ppat_record _}} -> processPattern vb.pvb_pat
    | {
     pvb_pat = {ppat_desc = Ppat_var _};
     pvb_expr = {pexp_desc = Pexp_fun (_, _, pat, e)};
    } ->
      (match pat with
      | {ppat_desc = Ppat_var _} -> push pat.ppat_loc Type
      | _ -> ());
      processFunction e
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator vb
  in
  let iterator = {Ast_iterator.default_iterator with value_binding} in
  (if Files.classifyFile path = Res then
   let parser =
     Res_driver.parsingEngine.parseImplementation ~forPrinter:false
   in
   let {Res_driver.parsetree = structure} = parser ~filename:path in
   iterator.structure iterator structure |> ignore);
  !hints
  |> List.filter_map (fun ((range : Protocol.range), hintKind) ->
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
             | Some label -> (
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
               match maxlen with
               | Some value ->
                 if String.length label > value then None else Some result
               | None -> Some result)
             | None -> None)))

let codeLens ~path ~debug =
  let lenses = ref [] in
  let push loc =
    let range = Utils.cmtLocToRange loc in
    lenses := range :: !lenses
  in
  (* Code lenses are only emitted for functions right now. So look for value bindings that are functions,
     and use the loc of the value binding itself so we can look up the full function type for our code lens. *)
  let value_binding (iterator : Ast_iterator.iterator)
      (vb : Parsetree.value_binding) =
    (match vb with
    | {
     pvb_pat = {ppat_desc = Ppat_var _; ppat_loc};
     pvb_expr = {pexp_desc = Pexp_fun _};
    } ->
      push ppat_loc
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator vb
  in
  let iterator = {Ast_iterator.default_iterator with value_binding} in
  (* We only print code lenses in implementation files. This is because they'd be redundant in interface files,
     where the definition itself will be the same thing as what would've been printed in the code lens. *)
  (if Files.classifyFile path = Res then
   let parser =
     Res_driver.parsingEngine.parseImplementation ~forPrinter:false
   in
   let {Res_driver.parsetree = structure} = parser ~filename:path in
   iterator.structure iterator structure |> ignore);
  !lenses
  |> List.filter_map (fun (range : Protocol.range) ->
         match Cmt.fullFromPath ~path with
         | None -> None
         | Some full -> (
           match
             References.getLocItem ~full
               ~pos:(range.start.line, range.start.character + 1)
               ~debug
           with
           | Some {locType = Typed (_, typeExpr, _)} ->
             Some
               (Protocol.stringifyCodeLens
                  {
                    range;
                    command =
                      Some
                        {
                          (* Code lenses can run commands. An empty command string means we just want the editor
                             to print the text, not link to running a command. *)
                          command = "";
                          (* Print the type with a huge line width, because the code lens always prints on a
                             single line in the editor. *)
                          title = typeExpr |> Shared.typeToString ~lineWidth:400;
                        };
                  })
           | _ -> None))
