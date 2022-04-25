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
  match line with 0 -> Some 0 | _ -> loop 0 0

let positionToOffset text (line, character) =
  match offsetOfLine text line with
  | None -> None
  | Some bol -> Some (bol + character)

type prop = {
  name : string;
  posStart : int * int;
  posEnd : int * int;
  exp : Parsetree.expression;
}

type jsxProps = {
  componentPath : string list;
  props : prop list;
  childrenStart : (int * int) option;
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
        Some (Completable.Cjsx (jsxProps.componentPath, prop.name, allLabels))
      else if
        prop.posEnd <= posBeforeCursor
        && posBeforeCursor < Loc.start prop.exp.pexp_loc
      then None
      else if prop.exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then None
      else loop rest
    | [] ->
      let beforeChildrenStart =
        match jsxProps.childrenStart with
        | Some childrenPos -> posBeforeCursor < childrenPos
        | None -> posBeforeCursor <= endPos
      in
      let afterCompName = posBeforeCursor >= posAfterCompName in
      if afterCompName && beforeChildrenStart then
        Some (Cjsx (jsxProps.componentPath, "", allLabels))
      else None
  in
  loop jsxProps.props

let rec skipLineComment ~pos ~i str =
  if i < String.length str then
    match str.[i] with
    | '\n' -> Some ((fst pos + 1, 0), i + 1)
    | _ -> skipLineComment ~pos:(fst pos, snd pos + 1) ~i:(i + 1) str
  else None

let rec skipComment ~pos ~i ~depth str =
  if i < String.length str then
    match str.[i] with
    | '\n' -> skipComment ~depth ~pos:(fst pos + 1, 0) ~i:(i + 1) str
    | '/' when i + 1 < String.length str && str.[i + 1] = '*' ->
      skipComment ~depth:(depth + 1) ~pos:(fst pos, snd pos + 2) ~i:(i + 2) str
    | '*' when i + 1 < String.length str && str.[i + 1] = '/' ->
      if depth > 1 then
        skipComment ~depth:(depth - 1)
          ~pos:(fst pos, snd pos + 2)
          ~i:(i + 2) str
      else Some ((fst pos, snd pos + 2), i + 2)
    | _ -> skipComment ~depth ~pos:(fst pos, snd pos + 1) ~i:(i + 1) str
  else None

let extractJsxProps ~text ~(compName : Longident.t Location.loc) ~args =
  let rec extractLabelPos ~pos ~i str =
    if i < String.length str then
      match str.[i] with
      | '/' when i + 1 < String.length str && str.[i + 1] = '/' -> (
        match skipLineComment ~pos ~i str with
        | Some (pos, i) -> extractLabelPos ~pos ~i str
        | None -> None)
      | '/' when i + 1 < String.length str && str.[i + 1] = '*' -> (
        match skipComment ~depth:0 ~pos ~i str with
        | Some (pos, i) -> extractLabelPos ~pos ~i str
        | None -> None)
      | ' ' | '\r' | '\t' ->
        extractLabelPos ~pos:(fst pos, snd pos + 1) ~i:(i + 1) str
      | '\n' -> extractLabelPos ~pos:(fst pos + 1, 0) ~i:(i + 1) str
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> Some pos
      | _ -> None
    else None
  in
  let thisCaseShouldNotHappen =
    {componentPath = []; props = []; childrenStart = None}
  in
  let rec processProps ~lastOffset ~lastPos ~acc args =
    match args with
    | (Asttypes.Labelled "children", {Parsetree.pexp_loc}) :: _ ->
      {
        componentPath = Utils.flattenLongIdent ~jsx:true compName.txt;
        props = List.rev acc;
        childrenStart =
          (if pexp_loc.loc_ghost then None else Some (Loc.start pexp_loc));
      }
    | ((Labelled s | Optional s), (eProp : Parsetree.expression)) :: rest -> (
      let ePosStart, ePosEnd = Loc.range eProp.pexp_loc in
      match
        (positionToOffset text ePosStart, positionToOffset text ePosEnd)
      with
      | Some offsetStart, Some offsetEnd when not eProp.pexp_loc.loc_ghost ->
        let label = String.sub text lastOffset (offsetStart - lastOffset) in
        let labelPos =
          match extractLabelPos ~pos:lastPos ~i:0 label with
          | Some pos -> pos
          | None -> (* Must be punned *) ePosStart
        in
        processProps
          ~acc:
            ({
               name = s;
               posStart = labelPos;
               posEnd = (fst labelPos, snd labelPos + String.length s);
               exp = eProp;
             }
            :: acc)
          ~lastOffset:offsetEnd ~lastPos:ePosEnd rest
      | _ -> thisCaseShouldNotHappen)
    | _ -> thisCaseShouldNotHappen
  in
  let posAfterCompName = Loc.end_ compName.loc in
  let offsetAfterCompName =
    match positionToOffset text posAfterCompName with
    | None -> assert false
    | Some offset -> offset
  in
  args
  |> processProps ~lastOffset:offsetAfterCompName ~lastPos:posAfterCompName
       ~acc:[]

type labelled = {
  name : string;
  opt : bool;
  posStart : int * int;
  posEnd : int * int;
}

type label = labelled option
type arg = {label : label; exp : Parsetree.expression}

let findExpApplyCompletable ~(args : arg list) ~endPos ~posBeforeCursor
    ~(funName : Longident.t Location.loc) =
  let funPath = Utils.flattenLongIdent funName.txt in
  let posAfterFunName = Loc.end_ funName.loc in
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
      if
        labelled.posStart <= posBeforeCursor
        && posBeforeCursor < labelled.posEnd
      then Some (Completable.Clabel (funPath, labelled.name, allNames))
      else if exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then None
      else loop rest
    | {label = None; exp} :: rest ->
      if exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then None
      else loop rest
    | [] ->
      if posAfterFunName <= posBeforeCursor && posBeforeCursor < endPos then
        Some (Clabel (funPath, "", allNames))
      else None
  in
  loop args

let extractExpApplyArgs ~text ~(funName : Longident.t Location.loc) ~args =
  let rec extractLabelPos ~pos ~i str =
    if i < String.length str then
      match str.[i] with
      | '/' when i + 1 < String.length str && str.[i + 1] = '/' -> (
        match skipLineComment ~pos ~i str with
        | Some (pos, i) -> extractLabelPos ~pos ~i str
        | None -> None)
      | '/' when i + 1 < String.length str && str.[i + 1] = '*' -> (
        match skipComment ~depth:0 ~pos ~i str with
        | Some (pos, i) -> extractLabelPos ~pos ~i str
        | None -> None)
      | ' ' | '\r' | '\t' | ',' | '(' | '~' ->
        extractLabelPos ~pos:(fst pos, snd pos + 1) ~i:(i + 1) str
      | '\n' -> extractLabelPos ~pos:(fst pos + 1, 0) ~i:(i + 1) str
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> Some pos
      | _ -> None
    else None
  in
  let rec processArgs ~lastOffset ~lastPos ~acc args =
    match args with
    | (((Asttypes.Labelled s | Optional s) as label), (e : Parsetree.expression))
      :: rest -> (
      let ePosStart, ePosEnd = Loc.range e.pexp_loc in
      match
        (positionToOffset text ePosStart, positionToOffset text ePosEnd)
      with
      | Some offsetStart, Some offsetEnd ->
        let labelText = String.sub text lastOffset (offsetStart - lastOffset) in
        let labelPos =
          match extractLabelPos ~pos:lastPos ~i:0 labelText with
          | Some pos -> pos
          | None -> (* Must be punned *) ePosStart
        in
        let labelled =
          {
            name = s;
            opt = (match label with Optional _ -> true | _ -> false);
            posStart = labelPos;
            posEnd = (fst labelPos, snd labelPos + String.length s);
          }
        in
        processArgs
          ~acc:({label = Some labelled; exp = e} :: acc)
          ~lastOffset:offsetEnd ~lastPos:ePosEnd rest
      | _ -> assert false)
    | (Asttypes.Nolabel, (e : Parsetree.expression)) :: rest -> (
      if e.pexp_loc.loc_ghost then processArgs ~acc ~lastOffset ~lastPos rest
      else
        let ePosEnd = Loc.end_ e.pexp_loc in
        match positionToOffset text ePosEnd with
        | Some offsetEnd ->
          processArgs
            ~acc:({label = None; exp = e} :: acc)
            ~lastOffset:offsetEnd ~lastPos:ePosEnd rest
        | _ ->
          (* should not happen *)
          assert false)
    | [] -> List.rev acc
  in
  let lastPos = Loc.end_ funName.loc in
  let lastOffset =
    match positionToOffset text lastPos with
    | Some offset -> offset
    | None -> assert false
  in
  args |> processArgs ~lastOffset ~lastPos ~acc:[]

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
  | _ -> None

let completionWithParser ~debug ~path ~posCursor ~currentFile ~text =
  let offset =
    match positionToOffset text posCursor with
    | Some offset -> offset
    | None -> assert false
  in
  let offsetNoWhite = skipWhite text (offset - 1) in
  let posNoWhite =
    let line, col = posCursor in
    (line, max 0 col - offset + offsetNoWhite)
  in
  let posBeforeCursor = (fst posCursor, max 0 (snd posCursor - 1)) in
  let blankAfterCursor =
    match positionToOffset text posCursor with
    | Some offset when offset > 0 -> (
      let charBeforeCursor = text.[offset - 1] in
      let charAtCursor =
        if offset < String.length text then text.[offset] else '\n'
      in
      match charAtCursor with
      | ' ' | '\t' | '\r' | '\n' -> Some charBeforeCursor
      | _ -> None)
    | _ -> None
  in

  let found = ref false in
  let result = ref None in
  let scope = ref (Scope.create ()) in
  let setResultOpt x =
    if !result = None then
      match x with None -> () | Some x -> result := Some (x, !scope)
  in
  let setResult x = setResultOpt (Some x) in
  let scopeValueDescription (vd : Parsetree.value_description) =
    scope :=
      !scope |> Scope.addValue ~name:vd.pval_name.txt ~loc:vd.pval_name.loc
  in
  let scopeValueBinding (vb : Parsetree.value_binding) =
    match vb.pvb_pat.ppat_desc with
    | Ppat_var {txt; loc}
    | Ppat_constraint ({ppat_desc = Ppat_var {txt; loc}}, _) ->
      scope := !scope |> Scope.addValue ~name:txt ~loc
    | _ -> ()
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
      if expr.pexp_loc |> Loc.hasPos ~pos:posNoWhite then (
        setFound ();
        match expr.pexp_desc with
        | Pexp_constant _ -> setResult Cnone
        | Pexp_ident id ->
          if debug then
            Printf.printf "Pexp_ident %s:%s\n"
              (Utils.flattenLongIdent id.txt |> String.concat ".")
              (Loc.toString id.loc);
          if id.loc |> Loc.hasPos ~pos:posBeforeCursor then
            let path_ = id.txt |> Utils.flattenLongIdent in
            let path =
              if blankAfterCursor = Some '.' then (
                (* Sometimes "Foo. " is followed by "bar" and the parser's
                   behaviour is to parse as "Foo.bar".
                   This gets back the intended path "Foo." *)
                let path =
                  match path_ |> List.rev with
                  | _last :: pathRev -> List.rev ("" :: pathRev)
                  | path -> path
                in
                if debug then
                  Printf.printf "Id breaks up. New path:%s\n"
                    (path |> String.concat ".");
                path)
              else path_
            in
            setResult (Cpath (CPId (path, Value)))
        | Pexp_construct (id, eOpt) ->
          if debug then
            Printf.printf "Pexp_construct %s:%s %s\n"
              (Utils.flattenLongIdent id.txt |> String.concat "\n")
              (Loc.toString id.loc)
              (match eOpt with
              | None -> "None"
              | Some e -> Loc.toString e.pexp_loc);
          if
            eOpt = None && (not id.loc.loc_ghost)
            && id.loc |> Loc.hasPos ~pos:posBeforeCursor
          then setResult (Cpath (CPId (Utils.flattenLongIdent id.txt, Value)))
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
                    if name = "_" then "" else name )
              in
              setResult (Cpath contextPath)
            | Lapply _ -> ()
          else if Loc.end_ e.pexp_loc = posBeforeCursor then
            match exprToContextPath e with
            | Some contextPath -> setResult (Cpath (CPField (contextPath, "")))
            | None -> ())
        | Pexp_apply ({pexp_desc = Pexp_ident compName}, args)
          when Res_parsetree_viewer.isJsxExpression expr ->
          let jsxProps = extractJsxProps ~text ~compName ~args in
          if debug then
            Printf.printf "JSX <%s:%s %s> _children:%s\n"
              (jsxProps.componentPath |> String.concat ",")
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
            setResult
              (Cpath
                 (CPId (Utils.flattenLongIdent ~jsx:true compName.txt, Module)))
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
        | Pexp_apply ({pexp_desc = Pexp_ident funName}, args) ->
          let args = extractExpApplyArgs ~text ~funName ~args in
          if debug then
            Printf.printf "Pexp_apply ...%s (%s)\n" (Loc.toString funName.loc)
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
          let expApplyCompletable =
            findExpApplyCompletable ~funName ~args
              ~endPos:(Loc.end_ expr.pexp_loc) ~posBeforeCursor
          in
          setResultOpt expApplyCompletable
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
      | Ptyp_constr (id, _args) ->
        if debug then
          Printf.printf "Ptyp_constr %s:%s\n"
            (Utils.flattenLongIdent id.txt |> String.concat ".")
            (Loc.toString id.loc);
        if id.loc |> Loc.hasPos ~pos:posBeforeCursor then
          setResult (Cpath (CPId (Utils.flattenLongIdent id.txt, Type)))
      | _ -> ());
    Ast_iterator.default_iterator.typ iterator core_type
  in
  let module_expr (iterator : Ast_iterator.iterator)
      (me : Parsetree.module_expr) =
    (match me.pmod_desc with
    | Pmod_ident id when id.loc |> Loc.hasPos ~pos:posBeforeCursor ->
      if debug then
        Printf.printf "Pmod_ident %s:%s\n"
          (Utils.flattenLongIdent id.txt |> String.concat ".")
          (Loc.toString id.loc);
      found := true;
      setResult (Cpath (CPId (Utils.flattenLongIdent id.txt, Module)))
    | _ -> ());
    Ast_iterator.default_iterator.module_expr iterator me
  in
  let module_type (iterator : Ast_iterator.iterator)
      (mt : Parsetree.module_type) =
    (match mt.pmty_desc with
    | Pmty_ident id when id.loc |> Loc.hasPos ~pos:posBeforeCursor ->
      if debug then
        Printf.printf "Pmty_ident %s:%s\n"
          (Utils.flattenLongIdent id.txt |> String.concat ".")
          (Loc.toString id.loc);
      found := true;
      setResult (Cpath (CPId (Utils.flattenLongIdent id.txt, Module)))
    | _ -> ());
    Ast_iterator.default_iterator.module_type iterator mt
  in

  let iterator =
    {
      Ast_iterator.default_iterator with
      attribute;
      expr;
      module_expr;
      module_type;
      signature;
      signature_item;
      structure;
      structure_item;
      typ;
    }
  in

  if Filename.check_suffix path ".res" then (
    let parser =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:false
    in
    let {Res_driver.parsetree = str} = parser ~filename:currentFile in
    iterator.structure iterator str |> ignore;
    if blankAfterCursor = Some ' ' || blankAfterCursor = Some '\n' then
      setResult (Cpath (CPId ([""], Value)));
    if !found = false then if debug then Printf.printf "XXX Not found!\n";
    !result)
  else if Filename.check_suffix path ".resi" then (
    let parser = Res_driver.parsingEngine.parseInterface ~forPrinter:false in
    let {Res_driver.parsetree = signature} = parser ~filename:currentFile in
    iterator.signature iterator signature |> ignore;
    if blankAfterCursor = Some ' ' || blankAfterCursor = Some '\n' then
      setResult (Cpath (CPId ([""], Type)));
    if !found = false then if debug then Printf.printf "XXX Not found!\n";
    !result)
  else None
