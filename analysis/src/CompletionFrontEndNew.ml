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
    locHasPos: Location.t -> bool;
        (** A helper for checking whether a loc has the cursor (beforeCursor). 
            This is the most natural position to check when figuring out if the user has the cursor in something. *)
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
    let locHasPos loc =
      loc |> CursorPosition.locHasCursor ~pos:posBeforeCursor
    in
    {
      offset;
      beforeCursor = posBeforeCursor;
      noWhitespace = posNoWhite;
      charBeforeNoWhitespace = firstCharBeforeCursorNoWhite;
      cursor = posCursor;
      charBeforeCursor;
      whitespaceAfterCursor;
      locHasPos;
    }
end

type completionCategory = Type | Value | Module | Field

type ctxPath =
  | CUnknown  (** Something that cannot be resolved right now *)
  | CId of string list * completionCategory
      (** A regular id of an expected category. `let fff = thisIsAnId<com>` and `let fff = SomePath.alsoAnId<com>` *)
  | CVariantPayload of {itemNum: int}
      (** A variant payload. `Some(<com>)` = itemNum 0, `Whatever(true, f<com>)` = itemNum 1*)
  | CRecordField of {seenFields: string list; prefix: string}
      (** A record field. `let f = {on: true, s<com>}` seenFields = [on], prefix = "s",*)
  | COption of ctxPath  (** An option with an inner type. *)
  | CArray of ctxPath option  (** An array with an inner type. *)
  | CTuple of ctxPath list  (** A tuple. *)
  | CBool
  | CString
  | CInt
  | CFloat
  | CFunction of {returnType: ctxPath}  (** A function *)

let rec ctxPathToString (ctxPath : ctxPath) =
  match ctxPath with
  | CUnknown -> "CUnknown"
  | CBool -> "CBool"
  | CFloat -> "CFloat"
  | CInt -> "CInt"
  | CString -> "CString"
  | CFunction {returnType} ->
    Printf.sprintf "CFunction () -> %s" (ctxPathToString returnType)
  | CTuple ctxPaths ->
    Printf.sprintf "CTuple(%s)"
      (ctxPaths |> List.map ctxPathToString |> String.concat ", ")
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

module CompletionInstruction = struct
  (** This is the completion instruction, that's responsible for resolving something at 
      context path X *)
  type t = CtxPath of ctxPath list

  let ctxPath ctxPath = CtxPath ctxPath

  let toString (c : t) =
    match c with
    | CtxPath ctxPath ->
      Printf.sprintf "CtxPath: %s"
        (ctxPath |> List.map ctxPathToString |> String.concat "->")
end

type currentlyExpecting =
  | Unit  (** Unit, (). Is what we reset to. *)
  | Type of ctxPath  (** A type at a context path. *)
  | TypeAtLoc of Location.t  (** A type at a location. *)
  | FunctionReturnType of ctxPath
      (** An instruction to resolve the return type of the type at the 
      provided context path, if it's a function (it should always be, 
      but you know...) *)

let currentlyExpectingToString (c : currentlyExpecting) =
  match c with
  | Unit -> "Unit"
  | Type ctxPath -> Printf.sprintf "Type<%s>" (ctxPathToString ctxPath)
  | TypeAtLoc loc -> Printf.sprintf "TypeAtLoc: %s" (Loc.toString loc)
  | FunctionReturnType ctxPath ->
    Printf.sprintf "FunctionReturnType<%s>" (ctxPathToString ctxPath)

module CompletionContext = struct
  type t = {
    positionContext: PositionContext.t;
    scope: Scope.t;
    currentlyExpecting: currentlyExpecting list;
    ctxPath: ctxPath list;
  }

  let make positionContext =
    {
      positionContext;
      scope = Scope.create ();
      currentlyExpecting = [];
      ctxPath = [];
    }

  let withResetCtx completionContext =
    {completionContext with currentlyExpecting = []; ctxPath = []}

  let withScope scope completionContext = {completionContext with scope}

  let addCurrentlyExpecting currentlyExpecting completionContext =
    {
      completionContext with
      currentlyExpecting =
        currentlyExpecting :: completionContext.currentlyExpecting;
    }

  let addCurrentlyExpectingOpt currentlyExpecting completionContext =
    match currentlyExpecting with
    | None -> completionContext
    | Some currentlyExpecting ->
      {
        completionContext with
        currentlyExpecting =
          currentlyExpecting :: completionContext.currentlyExpecting;
      }

  let currentlyExpectingOrReset currentlyExpecting completionContext =
    match currentlyExpecting with
    | None -> {completionContext with currentlyExpecting = []}
    | Some currentlyExpecting ->
      {
        completionContext with
        currentlyExpecting =
          currentlyExpecting :: completionContext.currentlyExpecting;
      }

  let currentlyExpectingOrTypeAtLoc ~loc currentlyExpecting completionContext =
    match currentlyExpecting with
    | None ->
      {
        completionContext with
        currentlyExpecting =
          TypeAtLoc loc :: completionContext.currentlyExpecting;
      }
    | Some currentlyExpecting ->
      {
        completionContext with
        currentlyExpecting =
          currentlyExpecting :: completionContext.currentlyExpecting;
      }

  let withResetCurrentlyExpecting completionContext =
    {completionContext with currentlyExpecting = [Unit]}

  let addCtxPathItem ctxPath completionContext =
    {completionContext with ctxPath = ctxPath :: completionContext.ctxPath}
end

module CompletionResult = struct
  type t = (CompletionInstruction.t * CompletionContext.t) option

  let ctxPath (ctxPath : ctxPath) (completionContext : CompletionContext.t) =
    let completionContext =
      completionContext |> CompletionContext.addCtxPathItem ctxPath
    in
    Some
      ( CompletionInstruction.ctxPath completionContext.ctxPath,
        completionContext )
end

let flattenLidCheckDot ?(jsx = true) ~(completionContext : CompletionContext.t)
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
  | Ptyp_constr ({txt = Lident "bool"}, []) -> Some CBool
  | Ptyp_constr ({txt = Lident "int"}, []) -> Some CInt
  | Ptyp_constr ({txt = Lident "float"}, []) -> Some CFloat
  | Ptyp_constr ({txt = Lident "string"}, []) -> Some CString
  | Ptyp_constr (lid, []) ->
    Some (CId (lid |> flattenLidCheckDot ~completionContext, Type))
  | Ptyp_tuple types ->
    let types =
      types
      |> List.map (fun (t : Parsetree.core_type) ->
             match t |> ctxPathFromCoreType ~completionContext with
             | None -> CUnknown
             | Some ctxPath -> ctxPath)
    in
    Some (CTuple types)
  | Ptyp_arrow _ -> (
    let rec loopFnTyp (ct : Parsetree.core_type) =
      match ct.ptyp_desc with
      | Ptyp_arrow (_arg, _argTyp, nextTyp) -> loopFnTyp nextTyp
      | _ -> ct
    in
    let returnType = loopFnTyp coreType in
    match ctxPathFromCoreType ~completionContext returnType with
    | None -> None
    | Some returnType -> Some (CFunction {returnType}))
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

let contextWithNewScope scope (context : CompletionContext.t) =
  {context with scope}

(* An expression with that's an expr hole and that has an empty cursor. TODO Explain *)
let checkIfExprHoleEmptyCursor ~(completionContext : CompletionContext.t)
    (exp : Parsetree.expression) =
  CompletionExpressions.isExprHole exp
  && CursorPosition.classifyLoc exp.pexp_loc
       ~pos:completionContext.positionContext.beforeCursor
     = EmptyLoc

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

let scopeValueBindings ~scope (valueBindings : Parsetree.value_binding list) =
  let newScope = ref scope in
  valueBindings
  |> List.iter (fun (vb : Parsetree.value_binding) ->
         newScope := scopeValueBinding vb ~scope:!newScope);
  !newScope

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
    (structure : Parsetree.structure) : CompletionResult.t =
  (* TODO: Scope? *)
  structure
  |> Utils.findMap (fun (item : Parsetree.structure_item) ->
         completeStructureItem ~completionContext item)

and completeStructureItem ~(completionContext : CompletionContext.t)
    (item : Parsetree.structure_item) : CompletionResult.t =
  let locHasPos = completionContext.positionContext.locHasPos in
  match item.pstr_desc with
  | Pstr_value (recFlag, valueBindings) ->
    if locHasPos item.pstr_loc then
      completeValueBindings ~completionContext ~recFlag valueBindings
    else None
  | Pstr_eval _ | Pstr_primitive _ | Pstr_type _ | Pstr_typext _
  | Pstr_exception _ | Pstr_module _ | Pstr_recmodule _ | Pstr_modtype _
  | Pstr_open _ | Pstr_include _ | Pstr_attribute _ | Pstr_extension _ ->
    None
  | Pstr_class _ | Pstr_class_type _ ->
    (* These aren't relevant for ReScript *) None

and completeValueBinding ~(completionContext : CompletionContext.t)
    (vb : Parsetree.value_binding) : CompletionResult.t =
  let locHasPos = completionContext.positionContext.locHasPos in
  if locHasPos vb.pvb_pat.ppat_loc then (
    print_endline "complete pattern";
    None)
  else if locHasPos vb.pvb_expr.pexp_loc then
    let bindingConstraint =
      findCurrentlyLookingForInPattern ~completionContext vb.pvb_pat
    in
    (* A let binding expression either has the constraint of the binding,
       or an inferred constraint (if it has been compiled), or no constraint. *)
    let completionContext =
      completionContext
      |> CompletionContext.currentlyExpectingOrTypeAtLoc
           ~loc:vb.pvb_pat.ppat_loc bindingConstraint
    in
    completeExpr ~completionContext vb.pvb_expr
  else None

and completeValueBindings ~(completionContext : CompletionContext.t)
    ~(recFlag : Asttypes.rec_flag)
    (valueBindings : Parsetree.value_binding list) : CompletionResult.t =
  let completionContext =
    if recFlag = Recursive then
      let scopeFromBindings =
        scopeValueBindings valueBindings ~scope:completionContext.scope
      in
      CompletionContext.withScope scopeFromBindings completionContext
    else completionContext
  in
  valueBindings
  |> Utils.findMap (fun (vb : Parsetree.value_binding) ->
         completeValueBinding ~completionContext vb)

and completeExpr ~completionContext (expr : Parsetree.expression) :
    CompletionResult.t =
  let locHasPos = completionContext.positionContext.locHasPos in
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
  | Pexp_construct ({txt = Lident txt; loc}, _) when loc |> locHasPos ->
    (* A constructor, like: `Co` *)
    CompletionResult.ctxPath (CId ([txt], Module)) completionContext
  | Pexp_construct (id, _) when id.loc |> locHasPos ->
    (* A path, like: `Something.Co` *)
    let lid = flattenLidCheckDot ~completionContext id in
    CompletionResult.ctxPath (CId (lid, Module)) completionContext
  (* == RECORDS == *)
  | Pexp_ident {txt = Lident prefix} when Utils.hasBraces expr.pexp_attributes
    ->
    (* An ident with braces attribute corresponds to for example `{n}`.
       Looks like a record but is parsed as an ident with braces. *)
    let prefix = if prefix = "()" then "" else prefix in
    CompletionResult.ctxPath
      (CRecordField {prefix; seenFields = []})
      completionContext
  | Pexp_record ([], _) when expr.pexp_loc |> locHasPos ->
    (* No fields means we're in a record body `{}` *)
    CompletionResult.ctxPath
      (CRecordField {prefix = ""; seenFields = []})
      completionContext
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
                 CompletionResult.ctxPath
                   (CRecordField {prefix; seenFields})
                   completionContext
               | fieldName ->
                 CompletionResult.ctxPath
                   (CId (flattenLidCheckDot ~completionContext fieldName, Value))
                   completionContext
             else if locHasPos fieldExpr.pexp_loc then
               completeExpr
                 ~completionContext:
                   (CompletionContext.addCtxPathItem
                      (CRecordField
                         {prefix = fieldName.txt |> Longident.last; seenFields})
                      completionContext)
                 fieldExpr
             else None)
    in
    match fieldToComplete with
    | None -> (
      (* Check if there's a expr hole with an empty cursor for a field.
         This means completing for an empty field `{someField: <com>}`. *)
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
        CompletionResult.ctxPath
          (CRecordField {prefix = fieldName; seenFields})
          completionContext
      | None, Some ',' ->
        CompletionResult.ctxPath
          (CRecordField {prefix = ""; seenFields})
          completionContext
      | _ -> None)
    | fieldToComplete -> fieldToComplete)
  (* == IDENTS == *)
  | Pexp_ident lid ->
    (* An identifier, like `aaa` *)
    let lidPath = flattenLidCheckDot lid ~completionContext in
    if lid.loc |> locHasPos then
      CompletionResult.ctxPath (CId (lidPath, Value)) completionContext
    else None
  | Pexp_let (recFlag, valueBindings, nextExpr) ->
    (* A let binding. `let a = b` *)
    let scopeFromBindings =
      scopeValueBindings valueBindings ~scope:completionContext.scope
    in
    let completionContextWithScopeFromBindings =
      CompletionContext.withScope scopeFromBindings completionContext
    in
    (* First check if the next expr is the thing with the cursor *)
    if locHasPos nextExpr.pexp_loc then
      completeExpr ~completionContext:completionContextWithScopeFromBindings
        nextExpr
    else if locHasPos expr.pexp_loc then
      (* The cursor is in the expression, but not in the next expression.
         Check the value bindings.*)
      completeValueBindings ~recFlag ~completionContext valueBindings
    else None
  | Pexp_ifthenelse (condition, then_, maybeElse) -> (
    if locHasPos condition.pexp_loc then
      (* TODO: I guess we could set looking for to "bool" here, since it's the if condition *)
      completeExpr
        ~completionContext:(CompletionContext.withResetCtx completionContext)
        condition
    else if locHasPos then_.pexp_loc then completeExpr ~completionContext then_
    else
      match maybeElse with
      | Some else_ ->
        if locHasPos else_.pexp_loc then completeExpr ~completionContext else_
        else if checkIfExprHoleEmptyCursor ~completionContext else_ then
          CompletionResult.ctxPath (CId ([], Value)) completionContext
        else None
      | _ ->
        (* Check then_ too *)
        if checkIfExprHoleEmptyCursor ~completionContext then_ then
          CompletionResult.ctxPath (CId ([], Value)) completionContext
        else None)
  | Pexp_sequence (evalExpr, nextExpr) ->
    if locHasPos evalExpr.pexp_loc then
      completeExpr
        ~completionContext:(CompletionContext.withResetCtx completionContext)
        evalExpr
    else if locHasPos nextExpr.pexp_loc then
      completeExpr ~completionContext nextExpr
    else None
  | Pexp_apply (fnExpr, _args) ->
    if locHasPos fnExpr.pexp_loc then
      completeExpr
        ~completionContext:(CompletionContext.withResetCtx completionContext)
        fnExpr
    else (* TODO: Complete args. Pipes *)
      None
  | Pexp_fun _ ->
    (* We've found a function definition, like `let whatever = (someStr: string) => {}` *)
    let rec loopFnExprs ~(completionContext : CompletionContext.t)
        (expr : Parsetree.expression) =
      (* TODO: Handle completing in default expressions and patterns *)
      match expr.pexp_desc with
      | Pexp_fun (_arg, _defaultExpr, pattern, nextExpr) ->
        let scopeFromPattern =
          scopePattern ~scope:completionContext.scope pattern
        in
        loopFnExprs
          ~completionContext:
            (completionContext |> CompletionContext.withScope scopeFromPattern)
          nextExpr
      | Pexp_constraint (expr, typ) ->
        (expr, completionContext, ctxPathFromCoreType ~completionContext typ)
      | _ -> (expr, completionContext, None)
    in
    let expr, completionContext, fnReturnConstraint =
      loopFnExprs ~completionContext expr
    in
    (* Set the expected type correctly for the expr body *)
    let completionContext =
      match fnReturnConstraint with
      | None -> (
        match completionContext.currentlyExpecting with
        | Type ctxPath :: _ ->
          (* Having a Type here already means the binding itself had a constraint on it. Since we're now moving into the function body,
             we'll need to ensure it's the function return type we use for completion, not the function type itself *)
          CompletionContext.addCurrentlyExpecting (FunctionReturnType ctxPath)
            completionContext
        | _ -> completionContext)
      | Some ctxPath ->
        CompletionContext.addCurrentlyExpecting (Type ctxPath) completionContext
    in
    if locHasPos expr.pexp_loc then completeExpr ~completionContext expr
    else if checkIfExprHoleEmptyCursor ~completionContext expr then
      CompletionResult.ctxPath (CId ([], Value)) completionContext
    else None
  | Pexp_match _ | Pexp_unreachable | Pexp_constant _ | Pexp_function _
  | Pexp_try (_, _)
  | Pexp_tuple _
  | Pexp_construct (_, _)
  | Pexp_variant (_, _)
  | Pexp_record (_, _)
  | Pexp_field (_, _)
  | Pexp_setfield (_, _, _)
  | Pexp_array _
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
  let completionContext = CompletionContext.make positionContext in
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
