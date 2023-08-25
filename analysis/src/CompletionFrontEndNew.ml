open SharedTypes

module PositionContext = struct
  type t = {
    offset: int;  (** The offset *)
    cursor: Pos.t;  (** The actual position of the cursor *)
    beforeCursor: Pos.t;  (** The position just before the cursor *)
    noWhitespace: Pos.t;
        (** The position of the cursor, removing any whitespace _before_ it *)
    charBeforeNoWhitespace: char option;
        (** The first character before the cursor, excluding any whitespace *)
    charBeforeCursor: char option;
        (** The char before the cursor, not excluding whitespace *)
    whitespaceAfterCursor: char option;
        (** The type of whitespace after the cursor, if any *)
  }

  let make ~offset ~posCursor text =
    let offsetNoWhite = Utils.skipWhite text (offset - 1) in
    let posNoWhite =
      let line, col = posCursor in
      (line, max 0 col - offset + offsetNoWhite)
    in
    let firstCharBeforeCursorNoWhite =
      if offsetNoWhite < String.length text && offsetNoWhite >= 0 then
        Some text.[offsetNoWhite]
      else None
    in
    let posBeforeCursor = Pos.posBeforeCursor posCursor in
    let charBeforeCursor, whitespaceAfterCursor =
      match Pos.positionToOffset text posCursor with
      | Some offset when offset > 0 -> (
        let charBeforeCursor = text.[offset - 1] in
        let charAtCursor =
          if offset < String.length text then text.[offset] else '\n'
        in
        match charAtCursor with
        | ' ' | '\t' | '\r' | '\n' ->
          (Some charBeforeCursor, Some charBeforeCursor)
        | _ -> (Some charBeforeCursor, None))
      | _ -> (None, None)
    in
    {
      offset;
      beforeCursor = posBeforeCursor;
      noWhitespace = posNoWhite;
      charBeforeNoWhitespace = firstCharBeforeCursorNoWhite;
      cursor = posCursor;
      charBeforeCursor;
      whitespaceAfterCursor;
    }
end

type completionCategory = Type | Value | Module | Field

type ctxPath =
  | CId of string list * completionCategory
      (** A regular id of an expected category. `let fff = thisIsAnId<com>` and `let fff = SomePath.alsoAnId<com>` *)
  | CVariantPayload of {itemNum: int}
      (** A variant payload. `Some(<com>)` = itemNum 0, `Whatever(true, f<com>)` = itemNum 1*)
  | CRecordField of {seenFields: string list; prefix: string}
      (** A record field. `let f = {on: true, s<com>}` seenFields = [on], prefix = "s",*)
  | COption of ctxPath  (** An option with an inner type. *)
  | CArray of ctxPath option  (** An array with an inner type. *)

let rec ctxPathToString (ctxPath : ctxPath) =
  match ctxPath with
  | CId (prefix, typ) ->
    Printf.sprintf "CId(%s)=%s"
      (match typ with
      | Value -> "Value"
      | Type -> "Type"
      | Module -> "Module"
      | Field -> "Field")
      (ident prefix)
  | CVariantPayload {itemNum} -> Printf.sprintf "CVariantPayload($%i)" itemNum
  | CRecordField {prefix} -> Printf.sprintf "CRecordField=%s" prefix
  | COption ctxPath -> Printf.sprintf "COption<%s>" (ctxPathToString ctxPath)
  | CArray ctxPath ->
    Printf.sprintf "CArray%s"
      (match ctxPath with
      | None -> ""
      | Some ctxPath -> "[" ^ ctxPathToString ctxPath ^ "]")

type currentlyExpecting = Type of ctxPath

let currentlyExpectingToString (c : currentlyExpecting) =
  match c with
  | Type ctxPath -> Printf.sprintf "Type<%s>" (ctxPathToString ctxPath)

type completionContext = {
  positionContext: PositionContext.t;
  scope: Scope.t;
  currentlyExpecting: currentlyExpecting list;
  ctxPath: ctxPath list;
}

type completionResult = (ctxPath list * completionContext) option

let flattenLidCheckDot ?(jsx = true) ~(completionContext : completionContext)
    (lid : Longident.t Location.loc) =
  (* Flatten an identifier keeping track of whether the current cursor
     is after a "." in the id followed by a blank character.
     In that case, cut the path after ".". *)
  let cutAtOffset =
    let idStart = Loc.start lid.loc in
    match completionContext.positionContext.whitespaceAfterCursor with
    | Some '.' ->
      if fst completionContext.positionContext.beforeCursor = fst idStart then
        Some (snd completionContext.positionContext.beforeCursor - snd idStart)
      else None
    | _ -> None
  in
  Utils.flattenLongIdent ~cutAtOffset ~jsx lid.txt

let rec ctxPathFromCoreType ~completionContext (coreType : Parsetree.core_type)
    =
  match coreType.ptyp_desc with
  | Ptyp_constr ({txt = Lident "option"}, [innerTyp]) ->
    innerTyp
    |> ctxPathFromCoreType ~completionContext
    |> Option.map (fun innerTyp -> COption innerTyp)
  | Ptyp_constr ({txt = Lident "array"}, [innerTyp]) ->
    Some (CArray (innerTyp |> ctxPathFromCoreType ~completionContext))
  | Ptyp_constr (lid, _) ->
    Some (CId (lid |> flattenLidCheckDot ~completionContext, Type))
  | _ -> None

let findCurrentlyLookingForInPattern ~completionContext
    (pat : Parsetree.pattern) =
  match pat.ppat_desc with
  | Ppat_constraint (_pat, typ) -> (
    match ctxPathFromCoreType ~completionContext typ with
    | None -> None
    | Some ctxPath -> Some (Type ctxPath))
  | _ -> None

let mergeCurrentlyLookingFor (currentlyExpecting : currentlyExpecting option)
    list =
  match currentlyExpecting with
  | None -> list
  | Some currentlyExpecting -> currentlyExpecting :: list

let contextWithNewScope scope context = {context with scope}

(** Scopes *)
let rec scopePattern ~scope (pat : Parsetree.pattern) =
  match pat.ppat_desc with
  | Ppat_any -> scope
  | Ppat_var {txt; loc} -> scope |> Scope.addValue ~name:txt ~loc
  | Ppat_alias (p, asA) ->
    let scope = scopePattern p ~scope in
    scope |> Scope.addValue ~name:asA.txt ~loc:asA.loc
  | Ppat_constant _ | Ppat_interval _ -> scope
  | Ppat_tuple pl ->
    pl |> List.map (fun p -> scopePattern p ~scope) |> List.concat
  | Ppat_construct (_, None) -> scope
  | Ppat_construct (_, Some {ppat_desc = Ppat_tuple pl}) ->
    pl |> List.map (fun p -> scopePattern p ~scope) |> List.concat
  | Ppat_construct (_, Some p) -> scopePattern ~scope p
  | Ppat_variant (_, None) -> scope
  | Ppat_variant (_, Some {ppat_desc = Ppat_tuple pl}) ->
    pl |> List.map (fun p -> scopePattern p ~scope) |> List.concat
  | Ppat_variant (_, Some p) -> scopePattern ~scope p
  | Ppat_record (fields, _) ->
    fields
    |> List.map (fun (fname, p) ->
           match fname with
           | {Location.txt = Longident.Lident _fname} -> scopePattern ~scope p
           | _ -> [])
    |> List.concat
  | Ppat_array pl ->
    pl
    |> List.map (fun (p : Parsetree.pattern) -> scopePattern ~scope p)
    |> List.concat
  | Ppat_or (p1, _) -> scopePattern ~scope p1
  | Ppat_constraint (p, _coreType) -> scopePattern ~scope p
  | Ppat_type _ -> scope
  | Ppat_lazy p -> scopePattern ~scope p
  | Ppat_unpack {txt; loc} -> scope |> Scope.addValue ~name:txt ~loc
  | Ppat_exception p -> scopePattern ~scope p
  | Ppat_extension _ -> scope
  | Ppat_open (_, p) -> scopePattern ~scope p

let scopeValueBinding ~scope (vb : Parsetree.value_binding) =
  scopePattern ~scope vb.pvb_pat

let scopeTypeKind ~scope (tk : Parsetree.type_kind) =
  match tk with
  | Ptype_variant constrDecls ->
    constrDecls
    |> List.map (fun (cd : Parsetree.constructor_declaration) ->
           scope |> Scope.addConstructor ~name:cd.pcd_name.txt ~loc:cd.pcd_loc)
    |> List.concat
  | Ptype_record labelDecls ->
    labelDecls
    |> List.map (fun (ld : Parsetree.label_declaration) ->
           scope |> Scope.addField ~name:ld.pld_name.txt ~loc:ld.pld_loc)
    |> List.concat
  | _ -> scope

let scopeTypeDeclaration ~scope (td : Parsetree.type_declaration) =
  let scope =
    scope |> Scope.addType ~name:td.ptype_name.txt ~loc:td.ptype_name.loc
  in
  scopeTypeKind ~scope td.ptype_kind

let scopeModuleBinding ~scope (mb : Parsetree.module_binding) =
  scope |> Scope.addModule ~name:mb.pmb_name.txt ~loc:mb.pmb_name.loc

let scopeModuleDeclaration ~scope (md : Parsetree.module_declaration) =
  scope |> Scope.addModule ~name:md.pmd_name.txt ~loc:md.pmd_name.loc

let rec completeFromStructure ~completionContext
    (structure : Parsetree.structure) : completionResult =
  (* TODO: Scope? *)
  structure
  |> Utils.findMap (fun (item : Parsetree.structure_item) ->
         completeStructureItem ~completionContext item)

and completeStructureItem ~completionContext (item : Parsetree.structure_item) :
    completionResult =
  match item.pstr_desc with
  | Pstr_value (recFlag, valueBindings) ->
    let scopeFromBindings =
      valueBindings
      |> List.map (fun (vb : Parsetree.value_binding) ->
             scopeValueBinding vb ~scope:completionContext.scope)
      |> List.concat
    in
    if
      item.pstr_loc
      |> CursorPosition.classifyLoc
           ~pos:completionContext.positionContext.beforeCursor
      = HasCursor
    then
      valueBindings
      |> Utils.findMap (fun (vb : Parsetree.value_binding) ->
             (* TODO: This will create duplicate scope entries for the current binding. Does it matter? *)
             completeValueBinding
               ~completionContext:
                 (if recFlag = Recursive then
                  completionContext |> contextWithNewScope scopeFromBindings
                 else completionContext)
               vb)
    else None
  | Pstr_eval _ | Pstr_primitive _ | Pstr_type _ | Pstr_typext _
  | Pstr_exception _ | Pstr_module _ | Pstr_recmodule _ | Pstr_modtype _
  | Pstr_open _ | Pstr_include _ | Pstr_attribute _ | Pstr_extension _ ->
    None
  | Pstr_class _ | Pstr_class_type _ ->
    (* These aren't relevant for ReScript *) None

and completeValueBinding ~completionContext (vb : Parsetree.value_binding) :
    completionResult =
  let scopeWithPattern =
    scopePattern ~scope:completionContext.scope vb.pvb_pat
  in
  if
    vb.pvb_pat.ppat_loc
    |> CursorPosition.classifyLoc
         ~pos:completionContext.positionContext.beforeCursor
    = HasCursor
  then (
    print_endline "complete pattern";
    None)
  else if
    vb.pvb_expr.pexp_loc
    |> CursorPosition.classifyLoc
         ~pos:completionContext.positionContext.beforeCursor
    = HasCursor
  then (
    print_endline "completing expression";
    let currentlyExpecting =
      findCurrentlyLookingForInPattern ~completionContext vb.pvb_pat
    in
    completeExpr
      ~completionContext:
        {
          completionContext with
          scope = scopeWithPattern;
          currentlyExpecting =
            mergeCurrentlyLookingFor currentlyExpecting
              completionContext.currentlyExpecting;
        }
      vb.pvb_expr)
  else None

and completeExpr ~completionContext (expr : Parsetree.expression) :
    completionResult =
  let locHasPos loc =
    loc
    |> CursorPosition.locHasCursor
         ~pos:completionContext.positionContext.beforeCursor
  in
  match expr.pexp_desc with
  (* == VARIANTS == *)
  | Pexp_construct (_id, Some {pexp_desc = Pexp_tuple args; pexp_loc})
    when pexp_loc |> locHasPos ->
    (* A constructor with multiple payloads, like: `Co(true, false)` or `Somepath.Co(false, true)` *)
    args
    |> Utils.findMapWithIndex (fun itemNum (e : Parsetree.expression) ->
           completeExpr
             ~completionContext:
               {
                 completionContext with
                 ctxPath =
                   CVariantPayload {itemNum} :: completionContext.ctxPath;
               }
             e)
  | Pexp_construct (_id, Some payloadExpr)
    when payloadExpr.pexp_loc |> locHasPos ->
    (* A constructor with a single payload, like: `Co(true)` or `Somepath.Co(false)` *)
    completeExpr
      ~completionContext:
        {
          completionContext with
          ctxPath = CVariantPayload {itemNum = 0} :: completionContext.ctxPath;
        }
      payloadExpr
  | Pexp_construct ({txt = Lident txt; loc}, _) when loc |> locHasPos -> (
    (* A constructor, like: `Co` *)
    match completionContext.currentlyExpecting with
    | _ ->
      Some (CId ([txt], Module) :: completionContext.ctxPath, completionContext)
    )
  | Pexp_construct (id, _) when id.loc |> locHasPos ->
    (* A path, like: `Something.Co` *)
    let lid = flattenLidCheckDot ~completionContext id in
    Some (CId (lid, Module) :: completionContext.ctxPath, completionContext)
  (* == RECORDS == *)
  | Pexp_ident {txt = Lident prefix} when Utils.hasBraces expr.pexp_attributes
    ->
    (* An ident with braces attribute corresponds to for example `{n}`.
       Looks like a record but is parsed as an ident with braces. *)
    let prefix = if prefix = "()" then "" else prefix in
    Some
      ( CRecordField {prefix; seenFields = []} :: completionContext.ctxPath,
        completionContext (* TODO: This isn't correct *) )
  | Pexp_record ([], _) when expr.pexp_loc |> locHasPos ->
    (* No fields means we're in a record body `{}` *)
    Some
      ( CRecordField {prefix = ""; seenFields = []} :: completionContext.ctxPath,
        completionContext (* TODO: This isn't correct *) )
  | Pexp_record (fields, _) when expr.pexp_loc |> locHasPos -> (
    (* A record with fields *)
    let seenFields =
      fields
      |> List.map (fun (fieldName, _f) -> Longident.last fieldName.Location.txt)
    in
    let fieldToComplete =
      fields
      |> Utils.findMap
           (fun
             ((fieldName, fieldExpr) :
               Longident.t Location.loc * Parsetree.expression)
           ->
             (* Complete regular idents *)
             if locHasPos fieldName.loc then
               (* Cursor in field name, complete here *)
               match fieldName with
               | {txt = Lident prefix} ->
                 Some
                   ( CRecordField {prefix; seenFields}
                     :: completionContext.ctxPath,
                     completionContext (* TODO: This isn't correct *) )
               | fieldName ->
                 Some
                   ( CId (flattenLidCheckDot ~completionContext fieldName, Value)
                     :: completionContext.ctxPath,
                     completionContext )
             else if locHasPos fieldExpr.pexp_loc then
               completeExpr
                 ~completionContext:
                   {
                     completionContext with
                     ctxPath =
                       CRecordField
                         {prefix = fieldName.txt |> Longident.last; seenFields}
                       :: completionContext.ctxPath;
                   }
                 fieldExpr
             else None)
    in
    match fieldToComplete with
    | None -> (
      (* Check if there's a expr hole with an empty cursor for a field. This means completing for an empty field `{someField: <com>}`. *)
      let fieldNameWithExprHole =
        fields
        |> Utils.findMap (fun (fieldName, fieldExpr) ->
               if
                 CompletionExpressions.isExprHole fieldExpr
                 && CursorPosition.classifyLoc fieldExpr.pexp_loc
                      ~pos:completionContext.positionContext.beforeCursor
                    = EmptyLoc
               then Some (Longident.last fieldName.Location.txt)
               else None)
      in
      (* We found no field to complete, but we know the cursor is inside this record body.
         Check if the char to the left of the cursor is ',', if so, complete for record fields.*)
      match
        ( fieldNameWithExprHole,
          completionContext.positionContext.charBeforeNoWhitespace )
      with
      | Some fieldName, _ ->
        Some
          ( CRecordField {prefix = fieldName; seenFields}
            :: completionContext.ctxPath,
            completionContext (* TODO: This isn't correct *) )
      | None, Some ',' ->
        Some
          ( CRecordField {prefix = ""; seenFields} :: completionContext.ctxPath,
            completionContext (* TODO: This isn't correct *) )
      | _ -> None)
    | fieldToComplete -> fieldToComplete)
  (* == IDENTS == *)
  | Pexp_ident lid ->
    (* An identifier, like `aaa` *)
    let lidPath = flattenLidCheckDot lid ~completionContext in
    if lid.loc |> locHasPos then
      Some (CId (lidPath, Value) :: completionContext.ctxPath, completionContext)
    else None
  | Pexp_match _ | Pexp_unreachable | Pexp_constant _
  | Pexp_let (_, _, _)
  | Pexp_function _
  | Pexp_fun (_, _, _, _)
  | Pexp_apply (_, _)
  | Pexp_try (_, _)
  | Pexp_tuple _
  | Pexp_construct (_, _)
  | Pexp_variant (_, _)
  | Pexp_record (_, _)
  | Pexp_field (_, _)
  | Pexp_setfield (_, _, _)
  | Pexp_array _
  | Pexp_ifthenelse (_, _, _)
  | Pexp_sequence (_, _)
  | Pexp_while (_, _)
  | Pexp_for (_, _, _, _, _)
  | Pexp_constraint (_, _)
  | Pexp_coerce (_, _, _)
  | Pexp_send (_, _)
  | Pexp_setinstvar (_, _)
  | Pexp_override _
  | Pexp_letmodule (_, _, _)
  | Pexp_letexception (_, _)
  | Pexp_assert _ | Pexp_lazy _
  | Pexp_poly (_, _)
  | Pexp_newtype (_, _)
  | Pexp_pack _
  | Pexp_open (_, _, _)
  | Pexp_extension _ ->
    None
  | Pexp_object _ | Pexp_new _ -> (* These are irrelevant to ReScript *) None

let completion ~currentFile ~path ~debug ~offset ~posCursor text =
  let positionContext = PositionContext.make ~offset ~posCursor text in
  let completionContext : completionContext =
    {
      positionContext;
      scope = Scope.create ();
      currentlyExpecting = [];
      ctxPath = [];
    }
  in
  if Filename.check_suffix path ".res" then
    let parser =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:false
    in
    let {Res_driver.parsetree = str} = parser ~filename:currentFile in
    str |> completeFromStructure ~completionContext
  else if Filename.check_suffix path ".resi" then
    let parser = Res_driver.parsingEngine.parseInterface ~forPrinter:false in
    let {Res_driver.parsetree = signature} = parser ~filename:currentFile in
    None
  else None
