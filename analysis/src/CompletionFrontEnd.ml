open SharedTypes

let rec skipWhite text i =
  if i < 0 then 0
  else
    match text.[i] with
    | ' ' | '\n' | '\r' | '\t' -> skipWhite text (i - 1)
    | _ -> i

let extractCompletableArgValueInfo exp =
  match exp.Parsetree.pexp_desc with
  | Pexp_ident {txt = Lident txt} -> Some txt
  | Pexp_construct ({txt = Lident "()"}, _) -> Some ""
  | Pexp_construct ({txt = Lident txt}, None) -> Some txt
  | Pexp_variant (label, None) -> Some ("#" ^ label)
  | _ -> None

let isExprHole exp =
  match exp.Parsetree.pexp_desc with
  | Pexp_extension ({txt = "rescript.exprhole"}, _) -> true
  | _ -> false

let isPatternHole pat =
  match pat.Parsetree.ppat_desc with
  | Ppat_extension ({txt = "rescript.patternhole"}, _) -> true
  | _ -> false

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

let findJsxPropsCompletable ~jsxProps ~endPos ~posBeforeCursor ~posAfterCompName
    =
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
      else if prop.exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then
        (* Cursor on expr assigned *)
        match extractCompletableArgValueInfo prop.exp with
        | Some prefix ->
          Some
            (CjsxPropValue
               {
                 pathToComponent =
                   Utils.flattenLongIdent ~jsx:true jsxProps.compName.txt;
                 prefix;
                 propName = prop.name;
               })
        | _ -> None
      else if prop.exp.pexp_loc |> Loc.end_ = (Location.none |> Loc.end_) then
        if isExprHole prop.exp then
          Some
            (CjsxPropValue
               {
                 pathToComponent =
                   Utils.flattenLongIdent ~jsx:true jsxProps.compName.txt;
                 prefix = "";
                 propName = prop.name;
               })
        else None
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

let findArgCompletables ~(args : arg list) ~endPos ~posBeforeCursor
    ~(contextPath : Completable.contextPath) ~posAfterFunExpr ~charBeforeCursor
    ~isPipedExpr =
  let fnHasCursor =
    posAfterFunExpr <= posBeforeCursor && posBeforeCursor < endPos
  in
  let allNames =
    List.fold_right
      (fun arg allLabels ->
        match arg with
        | {label = Some labelled} -> labelled.name :: allLabels
        | {label = None} -> allLabels)
      args []
  in
  let unlabelledCount = ref (if isPipedExpr then 1 else 0) in
  let rec loop args =
    match args with
    | {label = Some labelled; exp} :: rest ->
      if
        labelled.posStart <= posBeforeCursor
        && posBeforeCursor < labelled.posEnd
      then Some (Completable.CnamedArg (contextPath, labelled.name, allNames))
      else if exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then
        (* Completing in the assignment of labelled argument *)
        match extractCompletableArgValueInfo exp with
        | None -> None
        | Some prefix ->
          Some
            (Cargument
               {
                 functionContextPath = contextPath;
                 argumentLabel = Labelled labelled.name;
                 prefix;
               })
      else if isExprHole exp then
        Some
          (Cargument
             {
               functionContextPath = contextPath;
               argumentLabel = Labelled labelled.name;
               prefix = "";
             })
      else loop rest
    | {label = None; exp} :: rest ->
      if Res_parsetree_viewer.isTemplateLiteral exp then None
      else if exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then
        (* Completing in an unlabelled argument *)
        match extractCompletableArgValueInfo exp with
        | None -> None
        | Some prefix ->
          Some
            (Cargument
               {
                 functionContextPath = contextPath;
                 argumentLabel =
                   Unlabelled {argumentPosition = !unlabelledCount};
                 prefix;
               })
      else if isExprHole exp then
        Some
          (Cargument
             {
               functionContextPath = contextPath;
               argumentLabel = Unlabelled {argumentPosition = !unlabelledCount};
               prefix = "";
             })
      else (
        unlabelledCount := !unlabelledCount + 1;
        loop rest)
    | [] ->
      if fnHasCursor then
        match charBeforeCursor with
        | Some '~' -> Some (Completable.CnamedArg (contextPath, "", allNames))
        | _ ->
          Some
            (Cargument
               {
                 functionContextPath = contextPath;
                 argumentLabel =
                   Unlabelled {argumentPosition = !unlabelledCount};
                 prefix = "";
               })
      else None
  in
  match args with
  (* Special handling for empty fn calls, e.g. `let _ = someFn(<com>)` *)
  | [
   {label = None; exp = {pexp_desc = Pexp_construct ({txt = Lident "()"}, _)}};
  ]
    when fnHasCursor ->
    Some
      (Completable.Cargument
         {
           functionContextPath = contextPath;
           argumentLabel = Unlabelled {argumentPosition = 0};
           prefix = "";
         })
  | _ -> loop args

let rec exprToContextPath (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_constant (Pconst_string _) -> Some Completable.CPString
  | Pexp_constant (Pconst_integer _) -> Some CPInt
  | Pexp_constant (Pconst_float _) -> Some CPFloat
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

let rec getUnqualifiedName txt =
  match txt with
  | Longident.Lident fieldName -> fieldName
  | Ldot (t, _) -> getUnqualifiedName t
  | _ -> ""

let completePipeChain ~(lhs : Parsetree.expression) =
  (* Complete the end of pipe chains by reconstructing the pipe chain as a single pipe,
     so it can be completed.
     Example:
      someArray->Js.Array2.filter(v => v > 10)->Js.Array2.map(v => v + 2)->
        will complete as:
      Js.Array2.map(someArray->Js.Array2.filter(v => v > 10), v => v + 2)->
  *)
  match lhs.pexp_desc with
  (* When the left side of the pipe we're completing is a function application.
     Example: someArray->Js.Array2.map(v => v + 2)-> *)
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Lident "|."}},
        [
          (_, lhs);
          (_, {pexp_desc = Pexp_apply (d, args); pexp_loc; pexp_attributes});
        ] ) ->
    exprToContextPath
      {
        pexp_desc = Pexp_apply (d, (Nolabel, lhs) :: args);
        pexp_loc;
        pexp_attributes;
      }
    |> Option.map (fun ctxPath -> (ctxPath, d.pexp_loc))
    (* When the left side of the pipe we're completing is an identifier application.
       Example: someArray->filterAllTheGoodStuff-> *)
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Lident "|."}},
        [(_, lhs); (_, {pexp_desc = Pexp_ident id; pexp_loc; pexp_attributes})]
      ) ->
    exprToContextPath
      {
        pexp_desc =
          Pexp_apply
            ( {pexp_desc = Pexp_ident id; pexp_loc; pexp_attributes},
              [(Nolabel, lhs)] );
        pexp_loc;
        pexp_attributes;
      }
    |> Option.map (fun ctxPath -> (ctxPath, pexp_loc))
  | _ -> None

let findPatTupleItemWithCursor patterns ~pos =
  let patCount = ref None in
  let patCountWithPatHole = ref None in
  patterns
  |> List.iteri (fun index p ->
         match p.Parsetree.ppat_loc |> CursorPosition.classifyLoc ~pos with
         | HasCursor -> patCount := Some index
         | EmptyLoc -> patCountWithPatHole := Some index
         | _ -> ());
  match (!patCount, !patCountWithPatHole) with
  | Some patCount, _ -> Some patCount
  | None, Some patHoleCount -> Some patHoleCount
  | _ -> None

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
  let posBeforeCursor = Pos.posBeforeCursor posCursor in
  let charBeforeCursor, blankAfterCursor =
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
  let lookingForPat = ref None in
  let setLookingForPat ctxPath =
    lookingForPat :=
      Some (Completable.Cpattern {typ = ctxPath; prefix = ""; nested = None});
    if debug then
      Printf.printf "looking for: %s \n" (Completable.toString (Cpath ctxPath))
  in
  let unsetLookingForPat () = lookingForPat := None in
  let appendNestedPat patternPat =
    match !lookingForPat with
    | Some (Completable.Cpattern ({nested = None} as p)) ->
      lookingForPat := Some (Cpattern {p with nested = Some [patternPat]})
    | Some (Cpattern ({nested = Some nested} as p)) ->
      lookingForPat :=
        Some (Cpattern {p with nested = Some (nested @ [patternPat])})
    | _ -> ()
  in
  let commitFoundPat ?prefix () =
    match !lookingForPat with
    | Some (Completable.Cpattern p as cpattern) ->
      let cpattern =
        match prefix with
        | None -> cpattern
        | Some prefix -> Cpattern {p with prefix}
      in
      setResult cpattern
    | _ -> ()
  in
  (* Identifies expressions where we can do typed pattern or expr completion. *)
  let typedCompletionExpr (exp : Parsetree.expression) =
    if
      exp.pexp_loc
      |> CursorPosition.classifyLoc ~pos:posBeforeCursor
      = HasCursor
    then
      match exp.pexp_desc with
      | Pexp_match (_exp, []) ->
        (* No cases means there's no `|` yet in the switch *) ()
      | Pexp_match
          ( exp,
            [
              {
                pc_lhs =
                  {
                    ppat_desc =
                      Ppat_extension ({txt = "rescript.patternhole"}, _);
                  };
              };
            ] ) -> (
        (* A single case that's a pattern hole typically means `switch x { | }`. Complete as the pattern itself with nothing nested. *)
        match exprToContextPath exp with
        | None -> ()
        | Some ctxPath ->
          setResult
            (Completable.Cpattern {typ = ctxPath; nested = None; prefix = ""}))
      | Pexp_match (exp, _cases) -> (
        (* If there's more than one case, or the case isn't a pattern hole, set that we're looking for this path currently. *)
        match exp |> exprToContextPath with
        | None -> ()
        | Some ctxPath -> setLookingForPat ctxPath)
      | _ -> unsetLookingForPat ()
  in
  (* Tracks the path through a pattern for where the cursor is. *)
  let typedCompletionPat (pat : Parsetree.pattern) =
    if
      pat.ppat_loc
      |> CursorPosition.classifyLoc ~pos:posBeforeCursor
      = HasCursor
    then
      match pat.ppat_desc with
      | Ppat_var {txt} -> commitFoundPat ~prefix:txt ()
      | Ppat_construct ({txt = Lident prefix}, None) ->
        commitFoundPat ~prefix ()
      | Ppat_variant (prefix, None) -> commitFoundPat ~prefix:("#" ^ prefix) ()
      | Ppat_array arrayPatterns ->
        appendNestedPat Completable.PArray;
        if List.length arrayPatterns = 0 then commitFoundPat ~prefix:"" ()
      | Ppat_tuple patterns -> (
        match patterns |> findPatTupleItemWithCursor ~pos:posBeforeCursor with
        | Some itemNum -> appendNestedPat (Completable.PTupleItem {itemNum})
        | _ -> ())
      | Ppat_record ([], _) ->
        (* Empty fields means we're in a record body `{}`. Complete for the fields. *)
        appendNestedPat (Completable.PRecordBody {seenFields = []});
        commitFoundPat ~prefix:"" ()
      | Ppat_record (fields, _) -> (
        let fieldWithCursor = ref None in
        let fieldWithPatHole = ref None in
        fields
        |> List.iter (fun (fname, f) ->
               match
                 ( fname.Location.txt,
                   f.Parsetree.ppat_loc
                   |> CursorPosition.classifyLoc ~pos:posBeforeCursor )
               with
               | Longident.Lident fname, HasCursor ->
                 fieldWithCursor := Some (fname, f)
               | Lident fname, _ when isPatternHole f ->
                 fieldWithPatHole := Some (fname, f)
               | _ -> ());
        let seenFields =
          fields
          |> List.filter_map (fun (fieldName, _f) ->
                 match fieldName with
                 | {Location.txt = Longident.Lident fieldName} -> Some fieldName
                 | _ -> None)
        in
        match (!fieldWithCursor, !fieldWithPatHole) with
        | Some (fname, f), _ | None, Some (fname, f) -> (
          match f.ppat_desc with
          | Ppat_record _ | Ppat_construct _ | Ppat_variant _ | Ppat_tuple _ ->
            (* These are things we can continue into in the pattern. *)
            appendNestedPat (Completable.PFollowRecordField {fieldName = fname})
          | Ppat_extension ({txt = "rescript.patternhole"}, _) ->
            (* A pattern hole means for example `{someField: <com>}`. We want to complete for the type of `someField`.  *)
            appendNestedPat (Completable.PFollowRecordField {fieldName = fname});
            commitFoundPat ~prefix:"" ()
          | Ppat_var {txt} ->
            (* A var means `{s}` or similar. Complete for fields. *)
            appendNestedPat (Completable.PRecordBody {seenFields});
            commitFoundPat ~prefix:txt ()
          | _ -> ())
        | None, None -> (
          (* Figure out if we're completing for a new field.
             If the cursor is inside of the record body, but no field has the cursor,
             and there's no pattern hole. Check the first char to the left of the cursor,
             ignoring white space. If that's a comma, we assume you're completing for a new field. *)
          match firstCharBeforeCursorNoWhite with
          | Some ',' ->
            appendNestedPat (Completable.PRecordBody {seenFields});
            commitFoundPat ~prefix:"" ()
          | _ -> ()))
      | Ppat_construct
          ( {txt},
            Some {ppat_loc; ppat_desc = Ppat_construct ({txt = Lident "()"}, _)}
          )
        when ppat_loc
             |> CursorPosition.classifyLoc ~pos:posBeforeCursor
             = HasCursor ->
        (* Empty payload *)
        appendNestedPat
          (Completable.PVariantPayload
             {constructorName = getUnqualifiedName txt});
        commitFoundPat ~prefix:"" ()
      | Ppat_construct
          ( {txt},
            Some
              {
                ppat_loc;
                ppat_desc =
                  Ppat_var _ | Ppat_record _ | Ppat_construct _ | Ppat_variant _;
              } )
        when ppat_loc
             |> CursorPosition.classifyLoc ~pos:posBeforeCursor
             = HasCursor ->
        (* Single payload *)
        appendNestedPat
          (Completable.PVariantPayload
             {constructorName = getUnqualifiedName txt})
      | Ppat_construct
          ({txt}, Some {ppat_loc; ppat_desc = Ppat_tuple tupleItems})
        when ppat_loc
             |> CursorPosition.classifyLoc ~pos:posBeforeCursor
             = HasCursor -> (
        (* Multiple payloads with cursor in item *)
        (* TODO: New item with comma *)
        match tupleItems |> findPatTupleItemWithCursor ~pos:posBeforeCursor with
        | None -> ()
        | Some _ ->
          appendNestedPat
            (Completable.PVariantPayload
               {constructorName = getUnqualifiedName txt}))
      | Ppat_variant
          ( txt,
            Some {ppat_loc; ppat_desc = Ppat_construct ({txt = Lident "()"}, _)}
          )
        when ppat_loc
             |> CursorPosition.classifyLoc ~pos:posBeforeCursor
             = HasCursor ->
        (* Empty payload *)
        appendNestedPat
          (Completable.PPolyvariantPayload {constructorName = txt});
        commitFoundPat ~prefix:"" ()
      | Ppat_variant
          ( txt,
            Some
              {
                ppat_loc;
                ppat_desc =
                  Ppat_var _ | Ppat_record _ | Ppat_construct _ | Ppat_variant _;
              } )
        when ppat_loc
             |> CursorPosition.classifyLoc ~pos:posBeforeCursor
             = HasCursor ->
        (* Single payload *)
        appendNestedPat
          (Completable.PPolyvariantPayload {constructorName = txt})
      | Ppat_variant (txt, Some {ppat_loc; ppat_desc = Ppat_tuple tupleItems})
        when ppat_loc
             |> CursorPosition.classifyLoc ~pos:posBeforeCursor
             = HasCursor -> (
        (* Multiple payloads with cursor in item *)
        (* TODO: New item with comma *)
        match tupleItems |> findPatTupleItemWithCursor ~pos:posBeforeCursor with
        | None -> ()
        | Some _ ->
          appendNestedPat
            (Completable.PPolyvariantPayload {constructorName = txt}))
      | _ -> ()
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
    unsetLookingForPat ();
    let processed = ref false in
    (match item.pstr_desc with
    | Pstr_open {popen_lid} ->
      scope := !scope |> Scope.addOpen ~lid:popen_lid.txt
    | Pstr_primitive vd -> scopeValueDescription vd
    | Pstr_value (recFlag, bindings) ->
      (* Identify relevant destructures for completion, like `let {<com>} = someVar` or `let (true, false) = someFn()`. *)
      (match bindings with
      | [{pvb_pat = {ppat_desc = Ppat_record _ | Ppat_tuple _}; pvb_expr}] -> (
        match exprToContextPath pvb_expr with
        | None -> ()
        | Some ctxPath -> setLookingForPat ctxPath)
      | _ -> ());
      if recFlag = Recursive then bindings |> List.iter scopeValueBinding;
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
      match
        (Pos.positionToOffset text posStart, Pos.positionToOffset text posEnd)
      with
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
      match completePipeChain ~lhs with
      | None -> (
        match exprToContextPath lhs with
        | Some pipe ->
          setResult
            (Cpath (CPPipe {contextPath = pipe; id; lhsLoc = lhs.pexp_loc}));
          true
        | None -> false)
      | Some (pipe, lhsLoc) ->
        setResult (Cpath (CPPipe {contextPath = pipe; id; lhsLoc}));
        true
    in
    typedCompletionExpr expr;
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
        | Pexp_apply
            ( {pexp_desc = Pexp_ident {txt = Lident "|."}},
              [_; (_, {pexp_desc = Pexp_apply (funExpr, args)})] )
          when (* Normally named arg completion fires when the cursor is right after the expression.
                  E.g in foo(~<---there
                  But it should not fire in foo(~a)<---there *)
               not
                 (Loc.end_ expr.pexp_loc = posCursor
                 && charBeforeCursor = Some ')') ->
          (* Complete fn argument values and named args when the fn call is piped. E.g. someVar->someFn(<com>). *)
          let args = extractExpApplyArgs ~args in
          let argCompletable =
            match exprToContextPath funExpr with
            | Some contextPath ->
              findArgCompletables ~contextPath ~args
                ~endPos:(Loc.end_ expr.pexp_loc) ~posBeforeCursor
                ~posAfterFunExpr:(Loc.end_ funExpr.pexp_loc)
                ~charBeforeCursor ~isPipedExpr:true
            | None -> None
          in

          setResultOpt argCompletable
        | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "|."}}, [_; _]) ->
          (* Ignore any other pipe. *)
          ()
        | Pexp_apply (funExpr, args)
          when not
                 (Loc.end_ expr.pexp_loc = posCursor
                 && charBeforeCursor = Some ')') ->
          (* Complete fn argument values and named args when the fn call is _not_ piped. E.g. someFn(<com>). *)
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

          let argCompletable =
            match exprToContextPath funExpr with
            | Some contextPath ->
              findArgCompletables ~contextPath ~args
                ~endPos:(Loc.end_ expr.pexp_loc) ~posBeforeCursor
                ~posAfterFunExpr:(Loc.end_ funExpr.pexp_loc)
                ~charBeforeCursor ~isPipedExpr:false
            | None -> None
          in

          setResultOpt argCompletable
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
      let oldLookingForPat = !lookingForPat in
      typedCompletionPat pat;
      if debug then
        Printf.printf "posCursor:[%s] posNoWhite:[%s] Found pattern:%s\n"
          (Pos.toString posCursor) (Pos.toString posNoWhite)
          (Loc.toString pat.ppat_loc);
      (* TODO:
          This change breaks old behavior of completing constructors in scope.
          Either be fine with it, fix it somehow, or incorporate completing
          constructors in scope when regular completion for variants can't
          be done. *)
      (match (!lookingForPat, pat.ppat_desc) with
      | None, Ppat_construct (lid, _) ->
        let lidPath = flattenLidCheckDot lid in
        if debug then
          Printf.printf "Ppat_construct %s:%s\n"
            (lidPath |> String.concat ".")
            (Loc.toString lid.loc);
        setResult (Cpath (CPId (lidPath, Value)))
      | _ -> ());
      Ast_iterator.default_iterator.pat iterator pat;
      lookingForPat := oldLookingForPat)
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
  match Pos.positionToOffset text posCursor with
  | Some offset ->
    completionWithParser1 ~currentFile ~debug ~offset ~path ~posCursor ~text
  | None -> None
