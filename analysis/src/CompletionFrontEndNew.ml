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
      (** A variant payload. `Some(<com>)` = itemNum 0, `Whatever(true, f<com>)` = itemNum 1 *)
  | CTupleItem of {itemNum: int}
      (** A tuple item. `(true, false, <com>)` = itemNum 2 *)
  | CRecordField of {seenFields: string list; prefix: string}
      (** A record field. `let f = {on: true, s<com>}` seenFields = [on], prefix = "s",*)
  | COption of ctxPath  (** An option with an inner type. *)
  | CArray of ctxPath option  (** An array with an inner type. *)
  | CTuple of ctxPath list  (** A tuple. *)
  | CBool
  | CString
  | CInt
  | CFloat
  | CAwait of ctxPath  (** Awaiting a function call. *)
  | CFunction of {returnType: ctxPath}  (** A function *)
  | CField of ctxPath * string
      (** Field access. `whateverVariable.fieldName`. The ctxPath points to the value of `whateverVariable`, 
          and the string is the name of the field we're accessing. *)
  | CObj of ctxPath * string
      (** Object property access. `whateverVariable["fieldName"]`. The ctxPath points to the value of `whateverVariable`, 
          and the string is the name of the property we're accessing. *)
  | CApply of ctxPath * Asttypes.arg_label list
      (** Function application. `someFunction(someVar, ~otherLabel="hello")`. The ctxPath points to the function. *)

let rec ctxPathToString (ctxPath : ctxPath) =
  match ctxPath with
  | CUnknown -> "CUnknown"
  | CBool -> "bool"
  | CFloat -> "float"
  | CInt -> "int"
  | CString -> "string"
  | CAwait ctxPath -> Printf.sprintf "await %s" (ctxPathToString ctxPath)
  | CApply (ctxPath, args) ->
    Printf.sprintf "%s(%s)" (ctxPathToString ctxPath)
      (args
      |> List.map (function
           | Asttypes.Nolabel -> "Nolabel"
           | Labelled s -> "~" ^ s
           | Optional s -> "?" ^ s)
      |> String.concat ", ")
  | CField (ctxPath, fieldName) ->
    Printf.sprintf "(%s).%s" (ctxPathToString ctxPath) fieldName
  | CObj (ctxPath, fieldName) ->
    Printf.sprintf "(%s)[\"%s\"]" (ctxPathToString ctxPath) fieldName
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
  | CTupleItem {itemNum} -> Printf.sprintf "CTupleItem($%i)" itemNum
  | CRecordField {prefix} -> Printf.sprintf "CRecordField=%s" prefix
  | COption ctxPath -> Printf.sprintf "COption<%s>" (ctxPathToString ctxPath)
  | CArray ctxPath ->
    Printf.sprintf "array%s"
      (match ctxPath with
      | None -> ""
      | Some ctxPath -> "<" ^ ctxPathToString ctxPath ^ ">")

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
    currentlyExpecting: currentlyExpecting;
    ctxPath: ctxPath list;
  }

  let make positionContext =
    {
      positionContext;
      scope = Scope.create ();
      currentlyExpecting = Unit;
      ctxPath = [];
    }

  let withResetCtx completionContext =
    {completionContext with currentlyExpecting = Unit; ctxPath = []}

  let withScope scope completionContext = {completionContext with scope}

  let setCurrentlyExpecting currentlyExpecting completionContext =
    {completionContext with currentlyExpecting}

  let currentlyExpectingOrReset currentlyExpecting completionContext =
    match currentlyExpecting with
    | None -> {completionContext with currentlyExpecting = Unit}
    | Some currentlyExpecting -> {completionContext with currentlyExpecting}

  let currentlyExpectingOrTypeAtLoc ~loc currentlyExpecting completionContext =
    match currentlyExpecting with
    | None -> {completionContext with currentlyExpecting = TypeAtLoc loc}
    | Some currentlyExpecting -> {completionContext with currentlyExpecting}

  let withResetCurrentlyExpecting completionContext =
    {completionContext with currentlyExpecting = Unit}

  let addCtxPathItem ctxPath completionContext =
    {completionContext with ctxPath = ctxPath :: completionContext.ctxPath}
end

module CompletionInstruction = struct
  (** This is the completion instruction, that's responsible for resolving something at 
      context path X *)
  type t =
    | CtxPath of ctxPath list
    | Cpattern of {
        ctxPath: ctxPath list;
            (** This is the context path inside of the pattern itself. 
              Used to walk up to the type we're looking to complete. *)
        rootType: currentlyExpecting;
            (** This is the an instruction to find where completion starts 
              from. If we're completing inside of a record, it should resolve 
              to the record itself. *)
        prefix: string;
      }  (** Completing inside of a pattern. *)
    | Cexpression of {
        ctxPath: ctxPath list;
            (** This is the context path inside of the expression itself. 
              Used to walk up to the type we're looking to complete. *)
        rootType: currentlyExpecting;
            (** This is the an instruction to find where completion starts 
              from. If we're completing inside of a record, it should resolve 
              to the record itself. *)
        prefix: string;
      }  (** Completing inside of an expression. *)

  let ctxPath ctxPath = CtxPath ctxPath

  let pattern ~(completionContext : CompletionContext.t) ~prefix =
    Cpattern
      {
        prefix;
        rootType = completionContext.currentlyExpecting;
        ctxPath = completionContext.ctxPath;
      }

  let expression ~(completionContext : CompletionContext.t) ~prefix =
    Cexpression
      {
        prefix;
        rootType = completionContext.currentlyExpecting;
        ctxPath = completionContext.ctxPath;
      }

  let toString (c : t) =
    match c with
    | CtxPath ctxPath ->
      Printf.sprintf "CtxPath: %s"
        (ctxPath |> List.rev |> List.map ctxPathToString |> String.concat "->")
    | Cpattern {ctxPath; prefix; rootType} ->
      Printf.sprintf "Cpattern: ctxPath: %s, rootType: %s%s"
        (ctxPath |> List.rev |> List.map ctxPathToString |> String.concat "->")
        (currentlyExpectingToString rootType)
        (match prefix with
        | "" -> ""
        | prefix -> Printf.sprintf ", prefix: \"%s\"" prefix)
    | Cexpression {ctxPath; prefix; rootType} ->
      Printf.sprintf "Cexpression: ctxPath: %s, rootType: %s%s"
        (ctxPath |> List.rev |> List.map ctxPathToString |> String.concat "->")
        (currentlyExpectingToString rootType)
        (match prefix with
        | "" -> ""
        | prefix -> Printf.sprintf ", prefix: \"%s\"" prefix)
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

  let pattern ~(completionContext : CompletionContext.t) ~prefix =
    Some
      ( CompletionInstruction.pattern ~completionContext ~prefix,
        completionContext )

  let expression ~(completionContext : CompletionContext.t) ~prefix =
    Some
      ( CompletionInstruction.expression ~completionContext ~prefix,
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

(** This is for when you want a context path for an expression, without necessarily wanting 
    to do completion in that expression. For instance when completing patterns 
    `let {<com>} = someRecordVariable`, we want the context path to `someRecordVariable` to 
    be able to figure out the type we're completing in the pattern. *)
let rec exprToContextPathInner (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_constant (Pconst_string _) -> Some CString
  | Pexp_constant (Pconst_integer _) -> Some CInt
  | Pexp_constant (Pconst_float _) -> Some CFloat
  | Pexp_construct ({txt = Lident ("true" | "false")}, None) -> Some CBool
  | Pexp_array exprs ->
    Some
      (CArray
         (match exprs with
         | [] -> None
         | exp :: _ -> exprToContextPath exp))
  | Pexp_ident {txt = Lident ("|." | "|.u")} -> None
  | Pexp_ident {txt} -> Some (CId (Utils.flattenLongIdent txt, Value))
  | Pexp_field (e1, {txt = Lident name}) -> (
    match exprToContextPath e1 with
    | Some contextPath -> Some (CField (contextPath, name))
    | _ -> None)
  | Pexp_field (_, {txt = Ldot (lid, name)}) ->
    (* Case x.M.field ignore the x part *)
    Some (CField (CId (Utils.flattenLongIdent lid, Module), name))
  | Pexp_send (e1, {txt}) -> (
    match exprToContextPath e1 with
    | None -> None
    | Some contexPath -> Some (CObj (contexPath, txt)))
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Lident ("|." | "|.u")}},
        [
          (_, lhs);
          (_, {pexp_desc = Pexp_apply (d, args); pexp_loc; pexp_attributes});
        ] ) ->
    (* Transform away pipe with apply call *)
    exprToContextPath
      {
        pexp_desc = Pexp_apply (d, (Nolabel, lhs) :: args);
        pexp_loc;
        pexp_attributes;
      }
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Lident ("|." | "|.u")}},
        [(_, lhs); (_, {pexp_desc = Pexp_ident id; pexp_loc; pexp_attributes})]
      ) ->
    (* Transform away pipe with identifier *)
    exprToContextPath
      {
        pexp_desc =
          Pexp_apply
            ( {pexp_desc = Pexp_ident id; pexp_loc; pexp_attributes},
              [(Nolabel, lhs)] );
        pexp_loc;
        pexp_attributes;
      }
  | Pexp_apply (e1, args) -> (
    match exprToContextPath e1 with
    | None -> None
    | Some contexPath -> Some (CApply (contexPath, args |> List.map fst)))
  | Pexp_tuple exprs ->
    let exprsAsContextPaths = exprs |> List.filter_map exprToContextPath in
    if List.length exprs = List.length exprsAsContextPaths then
      Some (CTuple exprsAsContextPaths)
    else None
  | _ -> None

and exprToContextPath (e : Parsetree.expression) =
  match
    ( Res_parsetree_viewer.hasAwaitAttribute e.pexp_attributes,
      exprToContextPathInner e )
  with
  | true, Some ctxPath -> Some (CAwait ctxPath)
  | false, Some ctxPath -> Some ctxPath
  | _, None -> None

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

let checkIfPatternHoleEmptyCursor ~(completionContext : CompletionContext.t)
    (pat : Parsetree.pattern) =
  CompletionPatterns.isPatternHole pat
  && CursorPosition.classifyLoc pat.ppat_loc
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
  let bindingConstraint =
    findCurrentlyLookingForInPattern ~completionContext vb.pvb_pat
  in
  (* Always reset the context when completing value bindings,
     since they create their own context. *)
  let completionContext = CompletionContext.withResetCtx completionContext in
  if locHasPos vb.pvb_pat.ppat_loc then
    (* Completing the pattern of the binding. `let {<com>} = someRecordVariable`.
       Ensure the context carries the root type of `someRecordVariable`. *)
    let completionContext =
      CompletionContext.currentlyExpectingOrTypeAtLoc ~loc:vb.pvb_expr.pexp_loc
        (match exprToContextPath vb.pvb_expr with
        | None -> None
        | Some ctxPath -> Some (Type ctxPath))
        completionContext
    in
    completePattern ~completionContext vb.pvb_pat
  else if locHasPos vb.pvb_expr.pexp_loc then
    (* A let binding expression either has the constraint of the binding,
       or an inferred constraint (if it has been compiled), or no constraint. *)
    let completionContext =
      completionContext
      |> CompletionContext.currentlyExpectingOrTypeAtLoc
           ~loc:vb.pvb_pat.ppat_loc bindingConstraint
    in
    completeExpr ~completionContext vb.pvb_expr
  else if locHasPos vb.pvb_loc then
    (* In the binding but not in the pattern or expression means parser error recovery.
       We can still complete the pattern or expression if we have enough information. *)
    let exprHole = checkIfExprHoleEmptyCursor ~completionContext vb.pvb_expr in
    let patHole = checkIfPatternHoleEmptyCursor ~completionContext vb.pvb_pat in
    let exprCtxPath = exprToContextPath vb.pvb_expr in
    (* Try the expression. Example: `let someVar: someType = <com> *)
    if exprHole then
      let completionContext =
        completionContext
        |> CompletionContext.currentlyExpectingOrTypeAtLoc
             ~loc:vb.pvb_pat.ppat_loc bindingConstraint
      in
      CompletionResult.ctxPath (CId ([], Value)) completionContext
    else if patHole then
      let completionContext =
        CompletionContext.currentlyExpectingOrTypeAtLoc
          ~loc:vb.pvb_expr.pexp_loc
          (match exprCtxPath with
          | None -> None
          | Some ctxPath -> Some (Type ctxPath))
          completionContext
      in
      CompletionResult.pattern ~prefix:"" ~completionContext
    else None
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
    let completionContext =
      completionContext
      |> CompletionContext.addCtxPathItem (CId ([txt], Module))
    in
    CompletionResult.expression ~completionContext ~prefix:txt
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
    let completionContext =
      completionContext
      |> CompletionContext.addCtxPathItem
           (CRecordField {prefix; seenFields = []})
    in
    CompletionResult.expression ~completionContext ~prefix
  | Pexp_record ([], _) when expr.pexp_loc |> locHasPos ->
    (* No fields means we're in a record body `{}` *)
    let completionContext =
      completionContext
      |> CompletionContext.addCtxPathItem
           (CRecordField {prefix = ""; seenFields = []})
    in
    CompletionResult.expression ~completionContext ~prefix:""
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
               if checkIfExprHoleEmptyCursor ~completionContext fieldExpr then
                 Some (Longident.last fieldName.Location.txt)
               else None)
      in
      (* We found no field to complete, but we know the cursor is inside this record body.
         Check if the char to the left of the cursor is ',', if so, complete for record fields.*)
      match
        ( fieldNameWithExprHole,
          completionContext.positionContext.charBeforeNoWhitespace )
      with
      | Some fieldName, _ ->
        let completionContext =
          completionContext
          |> CompletionContext.addCtxPathItem
               (CRecordField {prefix = fieldName; seenFields})
        in
        CompletionResult.expression ~completionContext ~prefix:""
      | None, Some ',' ->
        let completionContext =
          completionContext
          |> CompletionContext.addCtxPathItem
               (CRecordField {prefix = ""; seenFields})
        in
        CompletionResult.expression ~completionContext ~prefix:""
      | _ -> None)
    | fieldToComplete -> fieldToComplete)
  (* == IDENTS == *)
  | Pexp_ident lid ->
    (* An identifier, like `aaa` *)
    let lidPath = flattenLidCheckDot lid ~completionContext in
    if lid.loc |> locHasPos then
      let completionContext =
        completionContext
        |> CompletionContext.addCtxPathItem (CId (lidPath, Value))
      in
      CompletionResult.expression ~completionContext ~prefix:""
    else None
  | Pexp_let (recFlag, valueBindings, nextExpr) ->
    (* A let binding. `let a = b` *)
    let scopeFromBindings =
      scopeValueBindings valueBindings ~scope:completionContext.scope
    in
    let completionContextWithScopeFromBindings =
      completionContext |> CompletionContext.withScope scopeFromBindings
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
          let completionContext =
            completionContext
            |> CompletionContext.addCtxPathItem (CId ([], Value))
          in
          CompletionResult.expression ~completionContext ~prefix:""
        else None
      | _ ->
        (* Check then_ too *)
        if checkIfExprHoleEmptyCursor ~completionContext then_ then
          let completionContext =
            completionContext
            |> CompletionContext.addCtxPathItem (CId ([], Value))
          in
          CompletionResult.expression ~completionContext ~prefix:""
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
        | Type ctxPath ->
          (* Having a Type here already means the binding itself had a constraint on it. Since we're now moving into the function body,
             we'll need to ensure it's the function return type we use for completion, not the function type itself *)
          completionContext
          |> CompletionContext.setCurrentlyExpecting
               (FunctionReturnType ctxPath)
        | _ -> completionContext)
      | Some ctxPath ->
        completionContext
        |> CompletionContext.setCurrentlyExpecting (Type ctxPath)
    in
    if locHasPos expr.pexp_loc then completeExpr ~completionContext expr
    else if checkIfExprHoleEmptyCursor ~completionContext expr then
      let completionContext =
        completionContext |> CompletionContext.addCtxPathItem (CId ([], Value))
      in
      CompletionResult.expression ~completionContext ~prefix:""
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

and completePattern ~(completionContext : CompletionContext.t)
    (pat : Parsetree.pattern) : CompletionResult.t =
  let locHasPos = completionContext.positionContext.locHasPos in
  match pat.ppat_desc with
  | Ppat_lazy p
  | Ppat_constraint (p, _)
  | Ppat_alias (p, _)
  | Ppat_exception p
  | Ppat_open (_, p) ->
    (* Can just continue into these patterns. *)
    if locHasPos pat.ppat_loc then p |> completePattern ~completionContext
    else None
  | Ppat_or (p1, p2) -> (
    (* Try to complete each `or` pattern *)
    let orPatCompleted =
      [p1; p2]
      |> List.find_map (fun p ->
             if locHasPos p.Parsetree.ppat_loc then
               completePattern ~completionContext p
             else None)
    in
    match orPatCompleted with
    | None
      when CompletionPatterns.isPatternHole p1
           || CompletionPatterns.isPatternHole p2 ->
      (* TODO(1) explain this *)
      CompletionResult.pattern ~completionContext ~prefix:""
    | res -> res)
  | Ppat_var {txt; loc} ->
    (* A variable, like `{ someThing: someV<com>}*)
    if locHasPos loc then
      CompletionResult.pattern ~completionContext ~prefix:txt
    else None
  | Ppat_record ([], _) ->
    (* Empty fields means we're in a record body `{}`. Complete for the fields. *)
    if locHasPos pat.ppat_loc then
      let completionContext =
        CompletionContext.addCtxPathItem
          (CRecordField {seenFields = []; prefix = ""})
          completionContext
      in
      CompletionResult.pattern ~completionContext ~prefix:""
    else None
  | Ppat_record (fields, _) -> (
    (* Record body with fields, where we know the cursor is inside of the record body somewhere. *)
    let seenFields =
      fields
      |> List.filter_map (fun (fieldName, _f) ->
             match fieldName with
             | {Location.txt = Longident.Lident fieldName} -> Some fieldName
             | _ -> None)
    in
    let fieldNameWithCursor =
      fields
      |> List.find_map
           (fun ((fieldName : Longident.t Location.loc), _fieldPattern) ->
             if locHasPos fieldName.Location.loc then Some fieldName else None)
    in
    let fieldPatternWithCursor =
      fields
      |> List.find_map (fun (fieldName, fieldPattern) ->
             if locHasPos fieldPattern.Parsetree.ppat_loc then
               Some (fieldName, fieldPattern)
             else None)
    in
    match (fieldNameWithCursor, fieldPatternWithCursor) with
    | Some fieldName, _ ->
      (* {someFieldName<com>: someValue} *)
      let prefix = Longident.last fieldName.txt in
      CompletionResult.pattern ~prefix
        ~completionContext:
          (CompletionContext.addCtxPathItem
             (CRecordField {seenFields; prefix})
             completionContext)
    | None, Some (fieldName, fieldPattern) ->
      (* {someFieldName: someOtherPattern<com>} *)
      let prefix = Longident.last fieldName.txt in
      let completionContext =
        CompletionContext.addCtxPathItem
          (CRecordField {seenFields; prefix})
          completionContext
      in
      completePattern ~completionContext fieldPattern
    | None, None ->
      if locHasPos pat.ppat_loc then
        (* We know the cursor is here, but it's not in a field name nor a field pattern.
           Check empty field patterns. TODO(1) *)
        None
      else None)
  | Ppat_tuple tupleItems -> (
    let tupleItemWithCursor =
      tupleItems
      |> Utils.findMapWithIndex (fun index (tupleItem : Parsetree.pattern) ->
             if locHasPos tupleItem.ppat_loc then Some (index, tupleItem)
             else None)
    in
    match tupleItemWithCursor with
    | Some (itemNum, tupleItem) ->
      let completionContext =
        completionContext
        |> CompletionContext.addCtxPathItem (CTupleItem {itemNum})
      in
      completePattern ~completionContext tupleItem
    | None ->
      if locHasPos pat.ppat_loc then
        (* We found no tuple item with the cursor, but we know the cursor is in the
           pattern. Check if the user is trying to complete an empty tuple item *)
        match completionContext.positionContext.charBeforeNoWhitespace with
        | Some ',' ->
          (* `(true, false, <com>)` itemNum = 2, or `(true, <com>, false)` itemNum = 1 *)
          (* Figure out which tuple item is active. *)
          let itemNum = ref (-1) in
          tupleItems
          |> List.iteri (fun index (pat : Parsetree.pattern) ->
                 if
                   completionContext.positionContext.beforeCursor
                   >= Loc.start pat.ppat_loc
                 then itemNum := index);
          if !itemNum > -1 then
            let completionContext =
              completionContext
              |> CompletionContext.addCtxPathItem
                   (CTupleItem {itemNum = !itemNum + 1})
            in
            CompletionResult.pattern ~completionContext ~prefix:""
          else None
        | Some '(' ->
          (* TODO: This should work (start of tuple), but the parser is broken for this case:
             let (<com> , true) = someRecordVar. If we fix that completing in the first position
             could work too. *)
          let completionContext =
            completionContext
            |> CompletionContext.addCtxPathItem (CTupleItem {itemNum = 0})
          in
          CompletionResult.pattern ~completionContext ~prefix:""
        | _ -> None
      else None)
  | Ppat_array items ->
    if locHasPos pat.ppat_loc then
      if List.length items = 0 then
        (* {someArr: [<com>]} *)
        let completionContext =
          completionContext |> CompletionContext.addCtxPathItem (CArray None)
        in
        CompletionResult.pattern ~completionContext ~prefix:""
      else
        let arrayItemWithCursor =
          items
          |> List.find_opt (fun (item : Parsetree.pattern) ->
                 locHasPos item.ppat_loc)
        in
        match
          ( arrayItemWithCursor,
            completionContext.positionContext.charBeforeNoWhitespace )
        with
        | Some item, _ ->
          (* Found an array item with the cursor. *)
          let completionContext =
            completionContext |> CompletionContext.addCtxPathItem (CArray None)
          in
          completePattern ~completionContext item
        | None, Some ',' ->
          (* No array item with the cursor, but we know the cursor is in the pattern.
             Check for "," which would signify the user is looking to add another
             array item to the pattern. *)
          let completionContext =
            completionContext |> CompletionContext.addCtxPathItem (CArray None)
          in
          CompletionResult.pattern ~completionContext ~prefix:""
        | _ -> None
    else None
  | Ppat_any ->
    (* We treat any `_` as an empty completion. This is mainly because we're
       inserting `_` in snippets and automatically put the cursor there. So
       letting it trigger an empty completion improves the ergonomics by a
       lot. *)
    if locHasPos pat.ppat_loc then
      CompletionResult.pattern ~completionContext ~prefix:""
    else None
  | Ppat_construct (_, _)
  | Ppat_variant (_, _)
  | Ppat_type _ | Ppat_unpack _ | Ppat_extension _ | Ppat_constant _
  | Ppat_interval _ ->
    None

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
