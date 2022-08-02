open SharedTypes

let rec skipWhite text i =
  if i < 0 then 0
  else
    match text.[i] with
    | ' ' | '\n' | '\r' | '\t' -> skipWhite text (i - 1)
    | _ -> i

let offsetOfLine text line =
  let ln = String.length text in
  let rec loop i lno =
    if i >= ln then None
    else
      match text.[i] with
      | '\n' -> if lno = line - 1 then Some (i + 1) else loop (i + 1) (lno + 1)
      | _ -> loop (i + 1) lno
  in
  match line with
  | 0 -> Some 0
  | _ -> loop 0 0

let positionToOffset text (line, character) =
  match offsetOfLine text line with
  | None -> None
  | Some bol ->
    if bol + character <= String.length text then Some (bol + character)
    else None

let getIdentFromExpr exp =
  match exp.Parsetree.pexp_desc with
  | Pexp_ident {txt} -> txt
  | Pexp_construct ({txt}, _) -> txt
  | Pexp_variant (label, _) -> Lident label
  | Pexp_field (_, {txt}) -> txt
  | _ -> Lident ""

let getPrefixFromExpr exp = getIdentFromExpr exp |> Longident.last

let rec exprToContextPath (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_constant (Pconst_string _) -> Some Completable.CPString
  | Pexp_array _ -> Some CPArray
  | Pexp_ident {txt} -> Some (CPId (Utils.flattenLongIdent txt, Value))
  | Pexp_field (e1, {txt = Lident name}) -> (
    match exprToContextPath e1 with
    | Some contextPath -> Some (CPField (contextPath, name))
    | _ -> None)
  | Pexp_field (_, {txt = Ldot (lid, name)}) ->
    (* Case x.M.field ignore the x part *)
    Some (CPField (CPId (Utils.flattenLongIdent lid, Module), name))
  | Pexp_send (e1, {txt}) -> (
    match exprToContextPath e1 with
    | None -> None
    | Some contexPath -> Some (CPObj (contexPath, txt)))
  | Pexp_apply (e1, args) -> (
    match exprToContextPath e1 with
    | None -> None
    | Some contexPath -> Some (CPApply (contexPath, args |> List.map fst)))
  | _ -> None

type prop = {
  name: string;
  posStart: int * int;
  posEnd: int * int;
  exp: Parsetree.expression;
}

type jsxProps = {
  compName: Longident.t Location.loc;
  props: prop list;
  childrenStart: (int * int) option;
}

(* Checks if an expression is elgible for typed context completion. *)
let isElgibleForTypedContextCompletion exp =
  match exp.Parsetree.pexp_desc with
  | Pexp_variant _ | Pexp_field _ | Pexp_construct _
  | Pexp_extension ({txt = "rescript.exprhole"}, _) ->
    true
  | _ -> false

let findJsxPropsCompletable ~jsxProps ~endPos ~posBeforeCursor ~posAfterCompName
    ~setCurrentlyLookingForTypeOpt =
  let allLabels =
    List.fold_right
      (fun prop allLabels -> prop.name :: allLabels)
      jsxProps.props []
  in
  let rec loop props =
    match props with
    | prop :: rest ->
      if prop.posStart <= posBeforeCursor && posBeforeCursor < prop.posEnd then
        (* Cursor on the prop name *)
        Some
          (Completable.Cjsx
             ( Utils.flattenLongIdent ~jsx:true jsxProps.compName.txt,
               prop.name,
               allLabels ))
      else if
        prop.posEnd <= posBeforeCursor
        && posBeforeCursor < Loc.start prop.exp.pexp_loc
      then (* Cursor between the prop name and expr assigned *)
        None
      else if prop.exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then (
        (* Cursor on expr assigned *)
        setCurrentlyLookingForTypeOpt
          (Some
             (Completable.JsxProp
                {
                  componentPath =
                    Utils.flattenLongIdent ~jsx:true jsxProps.compName.txt;
                  propName = prop.name;
                }));
        None)
      else if prop.exp.pexp_loc |> Loc.end_ = (Location.none |> Loc.end_) then (
        (* Expr assigned presumably is "rescript.exprhole" after parser recovery.
             Complete for the value. *)
        setCurrentlyLookingForTypeOpt
          (Some
             (Completable.JsxProp
                {
                  componentPath =
                    Utils.flattenLongIdent ~jsx:true jsxProps.compName.txt;
                  propName = prop.name;
                }));
        None)
      else loop rest
    | [] ->
      let beforeChildrenStart =
        match jsxProps.childrenStart with
        | Some childrenPos -> posBeforeCursor < childrenPos
        | None -> posBeforeCursor <= endPos
      in
      let afterCompName = posBeforeCursor >= posAfterCompName in
      if afterCompName && beforeChildrenStart then
        Some
          (Cjsx
             ( Utils.flattenLongIdent ~jsx:true jsxProps.compName.txt,
               "",
               allLabels ))
      else None
  in
  loop jsxProps.props

let extractJsxProps ~(compName : Longident.t Location.loc) ~args =
  let thisCaseShouldNotHappen =
    {
      compName = Location.mknoloc (Longident.Lident "");
      props = [];
      childrenStart = None;
    }
  in
  let rec processProps ~acc args =
    match args with
    | (Asttypes.Labelled "children", {Parsetree.pexp_loc}) :: _ ->
      {
        compName;
        props = List.rev acc;
        childrenStart =
          (if pexp_loc.loc_ghost then None else Some (Loc.start pexp_loc));
      }
    | ((Labelled s | Optional s), (eProp : Parsetree.expression)) :: rest -> (
      let namedArgLoc =
        eProp.pexp_attributes
        |> List.find_opt (fun ({Asttypes.txt}, _) -> txt = "ns.namedArgLoc")
      in
      match namedArgLoc with
      | Some ({loc}, _) ->
        processProps
          ~acc:
            ({
               name = s;
               posStart = Loc.start loc;
               posEnd = Loc.end_ loc;
               exp = eProp;
             }
            :: acc)
          rest
      | None -> processProps ~acc rest)
    | _ -> thisCaseShouldNotHappen
  in
  args |> processProps ~acc:[]

type labelled = {
  name: string;
  opt: bool;
  posStart: int * int;
  posEnd: int * int;
}

type label = labelled option
type arg = {label: label; exp: Parsetree.expression}

let findNamedArgCompletable ~(args : arg list) ~endPos ~posBeforeCursor
    ~(contextPath : Completable.contextPath) ~posAfterFunExpr
    ~setCurrentlyLookingForTypeOpt =
  let allNames =
    List.fold_right
      (fun arg allLabels ->
        match arg with
        | {label = Some labelled} -> labelled.name :: allLabels
        | {label = None} -> allLabels)
      args []
  in
  let rec loop args =
    match args with
    | {label = Some labelled; exp} :: rest ->
      (* Figure out if we're completing the labelled argument name, or assigning to the labelled argument *)
      if
        labelled.posStart <= posBeforeCursor
        && posBeforeCursor < labelled.posEnd
      then Some (Completable.CnamedArg (contextPath, labelled.name, allNames))
      else if exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then (
        setCurrentlyLookingForTypeOpt
          (Some (Completable.NamedArg {contextPath; label = labelled.name}));
        None)
      else if exp.pexp_loc |> Loc.end_ = (Location.none |> Loc.end_) then (
        (* Expr assigned presumably is "rescript.exprhole" after parser recovery.
           Assume this is an empty expression. *)
        setCurrentlyLookingForTypeOpt
          (Some (NamedArg {contextPath; label = labelled.name}));
        None)
      else loop rest
    | {label = None; exp} :: rest ->
      if exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then None
      else loop rest
    | [] ->
      if posAfterFunExpr <= posBeforeCursor && posBeforeCursor < endPos then
        Some (CnamedArg (contextPath, "", allNames))
      else None
  in
  loop args

let extractExpApplyArgs ~args =
  let rec processArgs ~acc args =
    match args with
    | (((Asttypes.Labelled s | Optional s) as label), (e : Parsetree.expression))
      :: rest -> (
      let namedArgLoc =
        e.pexp_attributes
        |> List.find_opt (fun ({Asttypes.txt}, _) -> txt = "ns.namedArgLoc")
      in
      match namedArgLoc with
      | Some ({loc}, _) ->
        let labelled =
          {
            name = s;
            opt =
              (match label with
              | Optional _ -> true
              | _ -> false);
            posStart = Loc.start loc;
            posEnd = Loc.end_ loc;
          }
        in
        processArgs ~acc:({label = Some labelled; exp = e} :: acc) rest
      | None -> processArgs ~acc rest)
    | (Asttypes.Nolabel, (e : Parsetree.expression)) :: rest ->
      if e.pexp_loc.loc_ghost then processArgs ~acc rest
      else processArgs ~acc:({label = None; exp = e} :: acc) rest
    | [] -> List.rev acc
  in
  args |> processArgs ~acc:[]

let rec getSimpleFieldName txt =
  match txt with
  | Longident.Lident fieldName -> fieldName
  | Ldot (t, _) -> getSimpleFieldName t
  | _ -> ""

let completionWithParser1 ~currentFile ~debug ~offset ~path ~posCursor ~text =
  let offsetNoWhite = skipWhite text (offset - 1) in
  let posNoWhite =
    let line, col = posCursor in
    (line, max 0 col - offset + offsetNoWhite)
  in
  (* Identifies the first character before the cursor that's not white space.
     Should be used very sparingly, but can be used to drive completion triggering
     in scenarios where the parser eats things we'd need to complete.
     Example: let {whatever,     <cursor>}, char is ','. *)
  let firstCharBeforeCursorNoWhite =
    if offsetNoWhite < String.length text && offsetNoWhite >= 0 then
      Some text.[offsetNoWhite]
    else None
  in
  let posBeforeCursor = (fst posCursor, max 0 (snd posCursor - 1)) in
  let charBeforeCursor, blankAfterCursor =
    match positionToOffset text posCursor with
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
  let flattenLidCheckDot ?(jsx = true) (lid : Longident.t Location.loc) =
    (* Flatten an identifier keeping track of whether the current cursor
       is after a "." in the id followed by a blank character.
       In that case, cut the path after ".". *)
    let cutAtOffset =
      let idStart = Loc.start lid.loc in
      match blankAfterCursor with
      | Some '.' ->
        if fst posBeforeCursor = fst idStart then
          Some (snd posBeforeCursor - snd idStart)
        else None
      | _ -> None
    in
    Utils.flattenLongIdent ~cutAtOffset ~jsx lid.txt
  in

  let found = ref false in
  let result = ref None in
  let scope = ref (Scope.create ()) in
  let setResultOpt x =
    if !result = None then
      match x with
      | None -> ()
      | Some x -> result := Some (x, !scope)
  in
  let setResult x = setResultOpt (Some x) in
  let currentlyLookingForType = ref None in
  let setCurrentlyLookingForTypeOpt x =
    if !currentlyLookingForType = None then
      match x with
      | None -> if debug then Printf.printf "Typed context, unsetting\n"
      | Some x ->
        if debug then
          Printf.printf "found typed context: %s\n"
            (match x with
            | Completable.JsxProp {propName} -> "jsxProp:" ^ propName
            | NamedArg {label} -> "namedArg:" ^ label
            | CtxPath contextPath ->
              "ctxPath:" ^ Completable.contextPathToString contextPath);
        currentlyLookingForType := Some x
  in
  let scopeValueDescription (vd : Parsetree.value_description) =
    scope :=
      !scope |> Scope.addValue ~name:vd.pval_name.txt ~loc:vd.pval_name.loc
  in
  let rec scopePattern (pat : Parsetree.pattern) =
    match pat.ppat_desc with
    | Ppat_any -> ()
    | Ppat_var {txt; loc} -> scope := !scope |> Scope.addValue ~name:txt ~loc
    | Ppat_alias (p, asA) ->
      scopePattern p;
      scope := !scope |> Scope.addValue ~name:asA.txt ~loc:asA.loc
    | Ppat_constant _ | Ppat_interval _ -> ()
    | Ppat_tuple pl -> pl |> List.iter scopePattern
    | Ppat_construct (_, None) -> ()
    | Ppat_construct (_, Some p) -> scopePattern p
    | Ppat_variant (_, None) -> ()
    | Ppat_variant (_, Some p) -> scopePattern p
    | Ppat_record (fields, _) ->
      fields |> List.iter (fun (_, p) -> scopePattern p)
    | Ppat_array pl -> pl |> List.iter scopePattern
    | Ppat_or (p1, _) -> scopePattern p1
    | Ppat_constraint (p, _) -> scopePattern p
    | Ppat_type _ -> ()
    | Ppat_lazy p -> scopePattern p
    | Ppat_unpack {txt; loc} -> scope := !scope |> Scope.addValue ~name:txt ~loc
    | Ppat_exception p -> scopePattern p
    | Ppat_extension _ -> ()
    | Ppat_open (_, p) -> scopePattern p
  in

  let scopeValueBinding (vb : Parsetree.value_binding) =
    scopePattern vb.pvb_pat
  in

  let scopeTypeKind (tk : Parsetree.type_kind) =
    match tk with
    | Ptype_variant constrDecls ->
      constrDecls
      |> List.iter (fun (cd : Parsetree.constructor_declaration) ->
             scope :=
               !scope
               |> Scope.addConstructor ~name:cd.pcd_name.txt ~loc:cd.pcd_loc)
    | Ptype_record labelDecls ->
      labelDecls
      |> List.iter (fun (ld : Parsetree.label_declaration) ->
             scope :=
               !scope |> Scope.addField ~name:ld.pld_name.txt ~loc:ld.pld_loc)
    | _ -> ()
  in
  let scopeTypeDeclaration (td : Parsetree.type_declaration) =
    scope :=
      !scope |> Scope.addType ~name:td.ptype_name.txt ~loc:td.ptype_name.loc;
    scopeTypeKind td.ptype_kind
  in
  let scopeModuleBinding (mb : Parsetree.module_binding) =
    scope :=
      !scope |> Scope.addModule ~name:mb.pmb_name.txt ~loc:mb.pmb_name.loc
  in
  let scopeModuleDeclaration (md : Parsetree.module_declaration) =
    scope :=
      !scope |> Scope.addModule ~name:md.pmd_name.txt ~loc:md.pmd_name.loc
  in

  let case (iterator : Ast_iterator.iterator) (case : Parsetree.case) =
    let oldScope = !scope in
    scopePattern case.pc_lhs;
    Ast_iterator.default_iterator.case iterator case;
    scope := oldScope
  in
  let structure (iterator : Ast_iterator.iterator)
      (structure : Parsetree.structure) =
    let oldScope = !scope in
    Ast_iterator.default_iterator.structure iterator structure;
    scope := oldScope
  in
  let structure_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.structure_item) =
    let processed = ref false in
    (match item.pstr_desc with
    | Pstr_open {popen_lid} ->
      scope := !scope |> Scope.addOpen ~lid:popen_lid.txt
    | Pstr_primitive vd -> scopeValueDescription vd
    | Pstr_value (recFlag, bindings) ->
      if recFlag = Recursive then bindings |> List.iter scopeValueBinding;

      (* Check for: let {destructuringSomething} = someIdentifier *)
      (* Ensure cursor is inside of record pattern. *)
      (* TODO: Tuples, etc... *)
      (* TODO: Handle let {SomeModule.recordField} = ...*)
      (match bindings with
      | [{pvb_pat = {ppat_desc = Ppat_record (fields, _)}; pvb_expr = expr}]
        when !result = None -> (
        (* The contextPath is what we'll use to look up the root record type for this completion.
           Depending on if the destructure is nested or not, we may or may not use that directly.*)
        let contextPath = exprToContextPath expr in
        let prefix, nestedContextPath =
          let prefix = ref "" in
          let rec findNestedContextPath ~path fields =
            (* This will descend through the record, following where the cursor is located,
               to pull out a nested context we can use to figure out what type
               we should complete from. *)
            fields
            |> List.find_map
                 (fun ({Location.txt}, {Parsetree.ppat_loc; ppat_desc}) ->
                   if ppat_loc |> Loc.hasPos ~pos:posBeforeCursor then
                     (* Covers when we're still inside the cursor position - continue descending
                        and build the nested context appropriately. *)
                     match ppat_desc with
                     | Ppat_record (fields, _) ->
                       if List.length fields = 0 then
                         (* Empty records are fine to complete from *)
                         Some
                           ([
                              Completable.RField
                                {
                                  fieldName = getSimpleFieldName txt;
                                  alreadySeenFields = [];
                                };
                            ]
                           @ path)
                       else
                         fields
                         |> findNestedContextPath
                              ~path:
                                ([
                                   Completable.RField
                                     {
                                       fieldName = getSimpleFieldName txt;
                                       alreadySeenFields =
                                         fields
                                         |> List.map (fun ({Location.txt}, _) ->
                                                getSimpleFieldName txt);
                                     };
                                 ]
                                @ path)
                     | Ppat_var {txt} ->
                       (* Whenever we encounter a var, that means we've hit an identifier the user has started typing.
                          We can then use that as the prefix hint for what to filter completions on. *)
                       prefix := txt;
                       Some path
                     | _ ->
                       (* This can be extended to understand more nested contexts, like tuples, etc. *)
                       Some path
                   else if ppat_loc |> Loc.end_ = (Location.none |> Loc.end_)
                   then
                     (* This means that an empty location was found, which typically means that the parser has made
                        recovery here. Complete wherever we are. *)
                     Some path
                   else
                     match firstCharBeforeCursorNoWhite with
                     | Some ',' -> Some path
                     | _ -> None)
          in

          ( !prefix,
            match List.length fields with
            | 0 ->
              (* A root record with 0 fields, `let {} = someVar`, is still valid to complete from. But it has no nested context. *)
              Some []
            | _ -> fields |> findNestedContextPath ~path:[] )
        in

        match (contextPath, nestedContextPath) with
        | Some contextPath, Some nestedContextPath ->
          setResultOpt
            (Some
               (Completable.CtypedContext
                  {
                    howToRetrieveSourceType = CtxPath contextPath;
                    patternPath = Some (nestedContextPath |> List.rev);
                    prefix = Some prefix;
                    alreadySeenIdents =
                      (match nestedContextPath with
                      | RField {alreadySeenFields} :: _rest ->
                        Some alreadySeenFields
                      | _ -> None);
                  }))
        | _ -> ())
      | _ -> ());

      bindings |> List.iter (fun vb -> iterator.value_binding iterator vb);
      if recFlag = Nonrecursive then bindings |> List.iter scopeValueBinding;
      processed := true
    | Pstr_type (recFlag, decls) ->
      if recFlag = Recursive then decls |> List.iter scopeTypeDeclaration;
      decls |> List.iter (fun td -> iterator.type_declaration iterator td);
      if recFlag = Nonrecursive then decls |> List.iter scopeTypeDeclaration;
      processed := true
    | Pstr_module mb ->
      iterator.module_binding iterator mb;
      scopeModuleBinding mb;
      processed := true
    | Pstr_recmodule mbs ->
      mbs |> List.iter scopeModuleBinding;
      mbs |> List.iter (fun b -> iterator.module_binding iterator b);
      processed := true
    | _ -> ());
    if not !processed then
      Ast_iterator.default_iterator.structure_item iterator item
  in
  let signature (iterator : Ast_iterator.iterator)
      (signature : Parsetree.signature) =
    let oldScope = !scope in
    Ast_iterator.default_iterator.signature iterator signature;
    scope := oldScope
  in
  let signature_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.signature_item) =
    let processed = ref false in
    (match item.psig_desc with
    | Psig_open {popen_lid} ->
      scope := !scope |> Scope.addOpen ~lid:popen_lid.txt
    | Psig_value vd -> scopeValueDescription vd
    | Psig_type (recFlag, decls) ->
      if recFlag = Recursive then decls |> List.iter scopeTypeDeclaration;
      decls |> List.iter (fun td -> iterator.type_declaration iterator td);
      if recFlag = Nonrecursive then decls |> List.iter scopeTypeDeclaration;
      processed := true
    | Psig_module md ->
      iterator.module_declaration iterator md;
      scopeModuleDeclaration md;
      processed := true
    | Psig_recmodule mds ->
      mds |> List.iter scopeModuleDeclaration;
      mds |> List.iter (fun d -> iterator.module_declaration iterator d);
      processed := true
    | _ -> ());
    if not !processed then
      Ast_iterator.default_iterator.signature_item iterator item
  in
  let attribute (iterator : Ast_iterator.iterator)
      ((id, payload) : Parsetree.attribute) =
    (if String.length id.txt >= 3 && String.sub id.txt 0 3 = "ns." then
     (* skip: internal parser attribute *) ()
    else if id.loc.loc_ghost then ()
    else if id.loc |> Loc.hasPos ~pos:posBeforeCursor then
      let posStart, posEnd = Loc.range id.loc in
      match (positionToOffset text posStart, positionToOffset text posEnd) with
      | Some offsetStart, Some offsetEnd ->
        (* Can't trust the parser's location
           E.g. @foo. let x... gives as label @foo.let *)
        let label =
          let rawLabel =
            String.sub text offsetStart (offsetEnd - offsetStart)
          in
          let ( ++ ) x y =
            match (x, y) with
            | Some i1, Some i2 -> Some (min i1 i2)
            | Some _, None -> x
            | None, _ -> y
          in
          let label =
            match
              String.index_opt rawLabel ' '
              ++ String.index_opt rawLabel '\t'
              ++ String.index_opt rawLabel '\r'
              ++ String.index_opt rawLabel '\n'
            with
            | None -> rawLabel
            | Some i -> String.sub rawLabel 0 i
          in
          if label <> "" && label.[0] = '@' then
            String.sub label 1 (String.length label - 1)
          else label
        in
        found := true;
        if debug then
          Printf.printf "Attribute id:%s:%s label:%s\n" id.txt
            (Loc.toString id.loc) label;
        setResult (Completable.Cdecorator label)
      | _ -> ());
    Ast_iterator.default_iterator.attribute iterator (id, payload)
  in
  let expr (iterator : Ast_iterator.iterator) (expr : Parsetree.expression) =
    let processed = ref false in
    let setFound () =
      found := true;
      if debug then
        Printf.printf "posCursor:[%s] posNoWhite:[%s] Found expr:%s\n"
          (Pos.toString posCursor) (Pos.toString posNoWhite)
          (Loc.toString expr.pexp_loc)
    in
    let setPipeResult ~(lhs : Parsetree.expression) ~id =
      match exprToContextPath lhs with
      | Some pipe ->
        setResult (Cpath (CPPipe (pipe, id)));
        true
      | None -> false
    in
    match expr.pexp_desc with
    | Pexp_apply
        ( {pexp_desc = Pexp_ident {txt = Lident "|."; loc = opLoc}},
          [
            (_, lhs);
            (_, {pexp_desc = Pexp_extension _; pexp_loc = {loc_ghost = true}});
          ] )
      when opLoc |> Loc.hasPos ~pos:posBeforeCursor ->
      (* Case foo-> when the parser adds a ghost expression to the rhs
         so the apply expression does not include the cursor *)
      if setPipeResult ~lhs ~id:"" then setFound ()
    | _ ->
      if expr.pexp_loc |> Loc.hasPos ~pos:posNoWhite && !result = None then (
        setFound ();
        match expr.pexp_desc with
        | Pexp_constant _ -> setResult Cnone
        | Pexp_ident lid ->
          let lidPath = flattenLidCheckDot lid in
          if debug then
            Printf.printf "Pexp_ident %s:%s\n"
              (lidPath |> String.concat ".")
              (Loc.toString lid.loc);
          if lid.loc |> Loc.hasPos ~pos:posBeforeCursor then
            setResult (Cpath (CPId (lidPath, Value)))
        | Pexp_construct (lid, eOpt) ->
          let lidPath = flattenLidCheckDot lid in
          if debug then
            Printf.printf "Pexp_construct %s:%s %s\n"
              (lidPath |> String.concat "\n")
              (Loc.toString lid.loc)
              (match eOpt with
              | None -> "None"
              | Some e -> Loc.toString e.pexp_loc);
          if
            eOpt = None && (not lid.loc.loc_ghost)
            && lid.loc |> Loc.hasPos ~pos:posBeforeCursor
          then setResult (Cpath (CPId (lidPath, Value)))
        | Pexp_field (e, fieldName) -> (
          if debug then
            Printf.printf "Pexp_field %s %s:%s\n" (Loc.toString e.pexp_loc)
              (Utils.flattenLongIdent fieldName.txt |> String.concat ".")
              (Loc.toString fieldName.loc);
          if fieldName.loc |> Loc.hasPos ~pos:posBeforeCursor then
            match fieldName.txt with
            | Lident name -> (
              match exprToContextPath e with
              | Some contextPath ->
                let contextPath = Completable.CPField (contextPath, name) in
                setResult (Cpath contextPath)
              | None -> ())
            | Ldot (id, name) ->
              (* Case x.M.field ignore the x part *)
              let contextPath =
                Completable.CPField
                  ( CPId (Utils.flattenLongIdent id, Module),
                    if blankAfterCursor = Some '.' then
                      (* x.M. field  --->  M. *) ""
                    else if name = "_" then ""
                    else name )
              in
              setResult (Cpath contextPath)
            | Lapply _ -> ()
          else if Loc.end_ e.pexp_loc = posBeforeCursor then
            match exprToContextPath e with
            | Some contextPath -> setResult (Cpath (CPField (contextPath, "")))
            | None -> ())
        | Pexp_apply ({pexp_desc = Pexp_ident compName}, args)
          when Res_parsetree_viewer.isJsxExpression expr ->
          let jsxProps = extractJsxProps ~compName ~args in
          let compNamePath = flattenLidCheckDot ~jsx:true compName in
          if debug then
            Printf.printf "JSX <%s:%s %s> _children:%s\n"
              (compNamePath |> String.concat ".")
              (Loc.toString compName.loc)
              (jsxProps.props
              |> List.map (fun {name; posStart; posEnd; exp} ->
                     Printf.sprintf "%s[%s->%s]=...%s" name
                       (Pos.toString posStart) (Pos.toString posEnd)
                       (Loc.toString exp.pexp_loc))
              |> String.concat " ")
              (match jsxProps.childrenStart with
              | None -> "None"
              | Some childrenPosStart -> Pos.toString childrenPosStart);
          let jsxCompletable =
            findJsxPropsCompletable ~jsxProps ~endPos:(Loc.end_ expr.pexp_loc)
              ~posBeforeCursor ~posAfterCompName:(Loc.end_ compName.loc)
              ~setCurrentlyLookingForTypeOpt
          in
          if jsxCompletable <> None then setResultOpt jsxCompletable
          else if compName.loc |> Loc.hasPos ~pos:posBeforeCursor then
            setResult (Cpath (CPId (compNamePath, Module)))
        | Pexp_apply
            ( {pexp_desc = Pexp_ident {txt = Lident "|."}},
              [
                (_, lhs);
                (_, {pexp_desc = Pexp_ident {txt = Longident.Lident id; loc}});
              ] )
          when loc |> Loc.hasPos ~pos:posBeforeCursor ->
          (* Case foo->id *)
          setPipeResult ~lhs ~id |> ignore
        | Pexp_apply
            ( {pexp_desc = Pexp_ident {txt = Lident "|."; loc = opLoc}},
              [(_, lhs); _] )
          when Loc.end_ opLoc = posCursor ->
          (* Case foo-> *)
          setPipeResult ~lhs ~id:"" |> ignore
        | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "|."}}, [_; _]) ->
          ()
        | Pexp_apply (funExpr, args)
          when (* Normally named arg completion fires when the cursor is right after the expression.
                  E.g in foo(~<---there
                  But it should not fire in foo(~a)<---there *)
               not
                 (Loc.end_ expr.pexp_loc = posCursor
                 && charBeforeCursor = Some ')') ->
          let args = extractExpApplyArgs ~args in
          if debug then
            Printf.printf "Pexp_apply ...%s (%s)\n"
              (Loc.toString funExpr.pexp_loc)
              (args
              |> List.map (fun {label; exp} ->
                     Printf.sprintf "%s...%s"
                       (match label with
                       | None -> ""
                       | Some {name; opt; posStart; posEnd} ->
                         "~" ^ name ^ Pos.toString posStart ^ "->"
                         ^ Pos.toString posEnd ^ "="
                         ^ if opt then "?" else "")
                       (Loc.toString exp.pexp_loc))
              |> String.concat ", ");

          let namedArgCompletable =
            match exprToContextPath funExpr with
            | Some contextPath ->
              findNamedArgCompletable ~contextPath ~args
                ~endPos:(Loc.end_ expr.pexp_loc) ~posBeforeCursor
                ~posAfterFunExpr:(Loc.end_ funExpr.pexp_loc)
                ~setCurrentlyLookingForTypeOpt
            | None -> None
          in

          setResultOpt namedArgCompletable
        | Pexp_send (lhs, {txt; loc}) -> (
          (* e["txt"]
             If the string for txt is not closed, it could go over several lines.
             Only take the first like to represent the label *)
          let txtLines = txt |> String.split_on_char '\n' in
          let label = List.hd txtLines in
          let label =
            if label <> "" && label.[String.length label - 1] = '\r' then
              String.sub label 0 (String.length label - 1)
            else label
          in
          let labelRange =
            let l, c = Loc.start loc in
            ((l, c + 1), (l, c + 1 + String.length label))
          in
          if debug then
            Printf.printf "Pexp_send %s%s e:%s\n" label
              (Range.toString labelRange)
              (Loc.toString lhs.pexp_loc);
          if
            labelRange |> Range.hasPos ~pos:posBeforeCursor
            || (label = "" && posCursor = fst labelRange)
          then
            match exprToContextPath lhs with
            | Some contextPath -> setResult (Cpath (CPObj (contextPath, label)))
            | None -> ())
        | Pexp_fun (_lbl, defaultExpOpt, pat, e) ->
          let oldScope = !scope in
          (match defaultExpOpt with
          | None -> ()
          | Some defaultExp -> iterator.expr iterator defaultExp);
          scopePattern pat;
          iterator.pat iterator pat;
          iterator.expr iterator e;
          scope := oldScope;
          processed := true
        | Pexp_let (recFlag, bindings, e) ->
          let oldScope = !scope in
          if recFlag = Recursive then bindings |> List.iter scopeValueBinding;
          bindings |> List.iter (fun vb -> iterator.value_binding iterator vb);
          if recFlag = Nonrecursive then bindings |> List.iter scopeValueBinding;
          iterator.expr iterator e;
          scope := oldScope;
          processed := true
        | Pexp_letmodule (name, modExpr, modBody) ->
          let oldScope = !scope in
          iterator.location iterator name.loc;
          iterator.module_expr iterator modExpr;
          scope := !scope |> Scope.addModule ~name:name.txt ~loc:name.loc;
          iterator.expr iterator modBody;
          scope := oldScope;
          processed := true
        | Pexp_open (_, lid, e) ->
          let oldScope = !scope in
          iterator.location iterator lid.loc;
          scope := !scope |> Scope.addOpen ~lid:lid.txt;
          iterator.expr iterator e;
          scope := oldScope;
          processed := true
        | _ -> ());
      if not !processed then Ast_iterator.default_iterator.expr iterator expr
  in
  let typ (iterator : Ast_iterator.iterator) (core_type : Parsetree.core_type) =
    if core_type.ptyp_loc |> Loc.hasPos ~pos:posNoWhite then (
      found := true;
      if debug then
        Printf.printf "posCursor:[%s] posNoWhite:[%s] Found type:%s\n"
          (Pos.toString posCursor) (Pos.toString posNoWhite)
          (Loc.toString core_type.ptyp_loc);
      match core_type.ptyp_desc with
      | Ptyp_constr (lid, _args) ->
        let lidPath = flattenLidCheckDot lid in
        if debug then
          Printf.printf "Ptyp_constr %s:%s\n"
            (lidPath |> String.concat ".")
            (Loc.toString lid.loc);
        if lid.loc |> Loc.hasPos ~pos:posBeforeCursor then
          setResult (Cpath (CPId (lidPath, Type)))
      | _ -> ());
    Ast_iterator.default_iterator.typ iterator core_type
  in
  let pat (iterator : Ast_iterator.iterator) (pat : Parsetree.pattern) =
    if pat.ppat_loc |> Loc.hasPos ~pos:posNoWhite then (
      found := true;
      if debug then
        Printf.printf "posCursor:[%s] posNoWhite:[%s] Found pattern:%s\n"
          (Pos.toString posCursor) (Pos.toString posNoWhite)
          (Loc.toString pat.ppat_loc);
      (match pat.ppat_desc with
      | Ppat_construct (lid, _) ->
        let lidPath = flattenLidCheckDot lid in
        if debug then
          Printf.printf "Ppat_construct %s:%s\n"
            (lidPath |> String.concat ".")
            (Loc.toString lid.loc);
        setResult (Cpath (CPId (lidPath, Value)))
      | _ -> ());
      Ast_iterator.default_iterator.pat iterator pat)
  in
  let module_expr (iterator : Ast_iterator.iterator)
      (me : Parsetree.module_expr) =
    (match me.pmod_desc with
    | Pmod_ident lid when lid.loc |> Loc.hasPos ~pos:posBeforeCursor ->
      let lidPath = flattenLidCheckDot lid in
      if debug then
        Printf.printf "Pmod_ident %s:%s\n"
          (lidPath |> String.concat ".")
          (Loc.toString lid.loc);
      found := true;
      setResult (Cpath (CPId (lidPath, Module)))
    | _ -> ());
    Ast_iterator.default_iterator.module_expr iterator me
  in
  let module_type (iterator : Ast_iterator.iterator)
      (mt : Parsetree.module_type) =
    (match mt.pmty_desc with
    | Pmty_ident lid when lid.loc |> Loc.hasPos ~pos:posBeforeCursor ->
      let lidPath = flattenLidCheckDot lid in
      if debug then
        Printf.printf "Pmty_ident %s:%s\n"
          (lidPath |> String.concat ".")
          (Loc.toString lid.loc);
      found := true;
      setResult (Cpath (CPId (lidPath, Module)))
    | _ -> ());
    Ast_iterator.default_iterator.module_type iterator mt
  in
  let type_kind (iterator : Ast_iterator.iterator)
      (type_kind : Parsetree.type_kind) =
    (match type_kind with
    | Ptype_variant [decl]
      when decl.pcd_name.loc |> Loc.hasPos ~pos:posNoWhite
           && decl.pcd_args = Pcstr_tuple [] ->
      (* "type t = Pre" could signal the intent to complete variant "Prelude",
         or the beginning of "Prefix.t" *)
      if debug then
        Printf.printf "Ptype_variant unary %s:%s\n" decl.pcd_name.txt
          (Loc.toString decl.pcd_name.loc);
      found := true;
      setResult (Cpath (CPId ([decl.pcd_name.txt], Value)))
    | _ -> ());
    Ast_iterator.default_iterator.type_kind iterator type_kind
  in

  let lastScopeBeforeCursor = ref (Scope.create ()) in
  let location (_iterator : Ast_iterator.iterator) (loc : Location.t) =
    if Loc.end_ loc <= posCursor then lastScopeBeforeCursor := !scope
  in

  let iterator =
    {
      Ast_iterator.default_iterator with
      attribute;
      case;
      expr;
      location;
      module_expr;
      module_type;
      pat;
      signature;
      signature_item;
      structure;
      structure_item;
      typ;
      type_kind;
    }
  in

  if Filename.check_suffix path ".res" then (
    let parser =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:false
    in
    let {Res_driver.parsetree = str} = parser ~filename:currentFile in
    iterator.structure iterator str |> ignore;
    if blankAfterCursor = Some ' ' || blankAfterCursor = Some '\n' then (
      scope := !lastScopeBeforeCursor;
      setResult (Cpath (CPId ([""], Value))));
    if !found = false then if debug then Printf.printf "XXX Not found!\n";
    !result)
  else if Filename.check_suffix path ".resi" then (
    let parser = Res_driver.parsingEngine.parseInterface ~forPrinter:false in
    let {Res_driver.parsetree = signature} = parser ~filename:currentFile in
    iterator.signature iterator signature |> ignore;
    if blankAfterCursor = Some ' ' || blankAfterCursor = Some '\n' then (
      scope := !lastScopeBeforeCursor;
      setResult (Cpath (CPId ([""], Type))));
    if !found = false then if debug then Printf.printf "XXX Not found!\n";
    !result)
  else None

let completionWithParser ~debug ~path ~posCursor ~currentFile ~text =
  match positionToOffset text posCursor with
  | Some offset ->
    completionWithParser1 ~currentFile ~debug ~offset ~path ~posCursor ~text
  | None -> None
