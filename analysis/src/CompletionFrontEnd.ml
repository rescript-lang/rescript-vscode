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

let rec getSimpleFieldName txt =
  match txt with
  | Longident.Lident fieldName -> fieldName
  | Ldot (t, _) -> getSimpleFieldName t
  | _ -> ""

(* Variants can have payloads. We need to figure out which of the payloads we're in, so we can find the type at that position as we try to do completion.
   We also need to continue descending into the pattern of the argument under the cursor to find any inner type. *)
let findVariantPayloadItemNumWithCursor pat ~pos =
  match pat.Parsetree.ppat_desc with
  | Ppat_tuple patterns ->
    let res = ref None in
    patterns
    |> List.iteri (fun index tuplePat ->
           match pat.Parsetree.ppat_loc |> CursorPosition.classifyLoc ~pos with
           | HasCursor -> res := Some (index, tuplePat)
           | _ -> ());
    !res
  | _ -> Some (0, pat)

let identFromPat pat =
  match pat.Parsetree.ppat_desc with
  | Ppat_var loc -> Some loc.txt
  | Ppat_construct (loc, _) -> Some (getSimpleFieldName loc.txt)
  | Ppat_variant (label, _) -> Some label
  | _ -> None

let rec identListFromPat pat =
  match pat.Parsetree.ppat_desc with
  | Ppat_var loc -> [loc.txt]
  (* Allow moving one level into variants, just to be able to complete Some(One) etc successfully *)
  (* E.g. Some(One)*)
  | Ppat_construct (loc, Some {ppat_desc = Ppat_construct (innerLoc, _)}) ->
    [getSimpleFieldName loc.txt ^ "(" ^ getSimpleFieldName innerLoc.txt ^ ")"]
    (* E.g. Some(#One)*)
  | Ppat_construct (loc, Some {ppat_desc = Ppat_variant (innerLabel, _)}) ->
    [getSimpleFieldName loc.txt ^ "(" ^ innerLabel ^ ")"]
    (* E.g. #Whatever(One)*)
  | Ppat_variant (label, Some {ppat_desc = Ppat_construct (innerLoc, _)}) ->
    [label ^ "(" ^ getSimpleFieldName innerLoc.txt ^ ")"]
    (* E.g. #Whatever(#One)*)
  | Ppat_variant (label, Some {ppat_desc = Ppat_variant (innerLabel, _)}) ->
    [label ^ "(" ^ innerLabel ^ ")"]
  | Ppat_construct (loc, _) -> [getSimpleFieldName loc.txt]
  | Ppat_variant (label, _) -> [label]
  | Ppat_or (pat1, pat2) -> identListFromPat pat1 @ identListFromPat pat2
  | _ -> []

let rec findAllOrBranches pat ~branches =
  match pat.Parsetree.ppat_desc with
  | Ppat_or (pat1, pat2) ->
    pat1 |> findAllOrBranches ~branches:([pat2] @ branches)
  | _ -> [pat] @ branches

(* Extracted for reuse. *)
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
        (* TODO: Add expr completion *)
        None
      else if prop.exp.pexp_loc |> Loc.end_ = (Location.none |> Loc.end_) then
        (* Expr assigned presumably is "rescript.exprhole" after parser recovery.
             Complete for the value. *)
        (* TODO: Add expr completion *)
        None
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
    ~(contextPath : Completable.contextPath) ~posAfterFunExpr =
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
      else if exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then
        (* Ok, were completing in the expression. Inspect what the expression is. *)
        (* TODO: Apply the same thing to JSX *)
        match exp.pexp_desc with
        | Pexp_fun _ -> (
          (* If this is a function expression, there might be function argument patterns we can
             complete (like the record destructuring in `someFunc(~someArg=({}) => {...})`) *)
          let idx = ref 0 in
          let rec findTargetArg exp =
            match exp.Parsetree.pexp_desc with
            | Pexp_fun (label, _, fnArgPattern, nextFunExpr) -> (
              let currentUnlabelledArgIdx = !idx in
              (match label with
              | Nolabel -> idx := !idx + 1
              | _ -> ());
              (* Check if the cursor is inside this args pattern. *)
              match
                ( fnArgPattern.ppat_loc
                  |> CursorPosition.classifyLoc ~pos:posBeforeCursor,
                  label )
              with
              | (EmptyLoc | HasCursor), Nolabel ->
                Some
                  (Completable.Unlabelled currentUnlabelledArgIdx, fnArgPattern)
              | (EmptyLoc | HasCursor), (Labelled label | Optional label) ->
                Some (Completable.Labelled label, fnArgPattern)
              | NoCursor, _ -> findTargetArg nextFunExpr)
            | _ -> None
          in
          match findTargetArg exp with
          | None -> None
          | Some (_arg, _argPattern) -> (* TODO: Add expr completion *) None)
        | _ ->
          (* TODO: Add expr completion *)
          None
      else if exp.pexp_loc |> Loc.end_ = (Location.none |> Loc.end_) then
        (* Expr assigned presumably is "rescript.exprhole" after parser recovery.
           Assume this is an empty expression. *)
        (* TODO: Add expr completion *)
        None
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

type findContextInPatternRes = {
  lookingToComplete: Completable.lookingToComplete;
  patternPath: Completable.patternPathItem list;
  prefix: string;
  alreadySeenIdents: string list;
}

let rec findTupleItemWithCursor items ~index ~pos =
  match items with
  | [] -> None
  | item :: rest ->
    if CursorPosition.classifyLoc item.Parsetree.ppat_loc ~pos = HasCursor then
      Some (Some item, index)
    else findTupleItemWithCursor rest ~index:(index + 1) ~pos

let rec findContextInPattern pattern ~pos ~patternPath ~debug
    ~seenIdentsFromParent ~firstCharBeforeCursorNoWhite =
  match pattern.Parsetree.ppat_desc with
  | Ppat_extension ({txt = "rescript.patternhole"}, _) ->
    (* This is printed when the parser has made recovery.
       E.g. `| Something => () | <com>` *)
    Some
      {
        (* We're completing vars as variants for now, since bool (which is a variant) is the only thing
           I can think of that needs completion here. *)
        lookingToComplete = CNoContext;
        patternPath;
        prefix = "";
        alreadySeenIdents = seenIdentsFromParent;
      }
  | Ppat_var loc when CursorPosition.classifyLoc loc.loc ~pos = HasCursor ->
    (* E.g. something: someVar *)
    Some
      {
        (* We're completing vars as variants for now, since bool (which is a variant) is the only thing
           I can think of that needs completion here. *)
        lookingToComplete = CVariant;
        patternPath;
        prefix = loc.txt;
        alreadySeenIdents = [];
      }
  (* Variants etc *)
  | Ppat_construct ({txt}, Some payload)
    when CursorPosition.classifyLoc payload.ppat_loc ~pos = HasCursor ->
    (* When there's a variant with a payload, and the cursor is in the pattern. Some(S) for example. *)
    let payloadNum =
      match payload.ppat_desc with
      | Ppat_tuple items ->
        (* Find the tuple item that has the cursor, so we can add that as payload num for the variant.*)
        findTupleItemWithCursor items ~index:0 ~pos
      | _ -> Some (None, 0)
    in
    let patternToContinueFrom =
      match payloadNum with
      | Some (Some pat, _) -> pat
      | _ -> payload
    in
    findContextInPattern patternToContinueFrom ~pos ~debug
      ~firstCharBeforeCursorNoWhite ~seenIdentsFromParent:[]
      ~patternPath:
        ([
           Completable.Variant
             {
               constructorName = getSimpleFieldName txt;
               payloadNum =
                 (match payloadNum with
                 | Some (_, payloadNum) -> Some payloadNum
                 | _ -> None);
             };
         ]
        @ patternPath)
  | Ppat_variant (constructorName, Some pat)
    when CursorPosition.classifyLoc pat.ppat_loc ~pos = HasCursor -> (
    (* When there's a polyvariant with a payload, and the cursor is in the pattern. Some(S) for example. *)
    match pat.ppat_desc with
    | Ppat_tuple _ ->
      (* Continue down here too, but let us discover the tuple in the next step. *)
      findContextInPattern pat ~firstCharBeforeCursorNoWhite ~pos ~patternPath
        ~debug ~seenIdentsFromParent:[]
    | _ ->
      findContextInPattern pat ~firstCharBeforeCursorNoWhite ~pos ~debug
        ~seenIdentsFromParent:[]
        ~patternPath:
          ([Completable.Variant {constructorName; payloadNum = Some 0}]
          @ patternPath))
  | Ppat_construct ({txt}, None) -> (
    (* Payload-less variant *)
    match CursorPosition.classifyLoc pattern.ppat_loc ~pos with
    | HasCursor ->
      Some
        {
          lookingToComplete = CVariant;
          patternPath;
          prefix = getSimpleFieldName txt;
          alreadySeenIdents = seenIdentsFromParent;
        }
    | NoCursor -> None
    | EmptyLoc -> None)
  | Ppat_variant (label, None) -> (
    (* Payload-less polyvariant *)
    match CursorPosition.classifyLoc pattern.ppat_loc ~pos with
    | HasCursor ->
      Some
        {
          lookingToComplete = CPolyvariant;
          patternPath;
          prefix = label;
          alreadySeenIdents = seenIdentsFromParent;
        }
    | NoCursor -> None
    | EmptyLoc -> None)
  | Ppat_construct
      ( {txt},
        Some {ppat_loc; ppat_desc = Ppat_construct ({txt = Lident "()"}, _)} )
    -> (
    (* A variant like SomeVariant(). Interpret as completing the payload of the variant  *)
    match CursorPosition.classifyLoc ppat_loc ~pos with
    | HasCursor ->
      Some
        {
          lookingToComplete = CNoContext;
          patternPath =
            [
              Completable.Variant
                {constructorName = getSimpleFieldName txt; payloadNum = Some 0};
            ]
            @ patternPath;
          prefix = "";
          alreadySeenIdents = [];
        }
    | NoCursor -> None
    | EmptyLoc -> None)
  | Ppat_variant
      ( label,
        Some {ppat_loc; ppat_desc = Ppat_construct ({txt = Lident "()"}, _)} )
    -> (
    (* A polyvariant like #SomeVariant(). Interpret as completing the payload of the variant  *)
    match CursorPosition.classifyLoc ppat_loc ~pos with
    | HasCursor ->
      Some
        {
          lookingToComplete = CNoContext;
          patternPath =
            [Completable.Polyvariant {name = label; payloadNum = Some 0}]
            @ patternPath;
          prefix = "";
          alreadySeenIdents = [];
        }
    | NoCursor -> None
    | EmptyLoc -> None)
  (* Records *)
  | Ppat_record ([], _) ->
    (* No fields mean we're in an empty record body. We can complete for that. *)
    Some
      {
        lookingToComplete = CRecordField;
        patternPath;
        prefix = "";
        alreadySeenIdents = [];
      }
  | Ppat_record (fields, _) -> (
    let seenFields =
      fields |> List.map (fun ({Location.txt}, _) -> getSimpleFieldName txt)
    in
    let fieldWithCursorExists =
      fields
      |> List.exists (fun (_, fieldPat) ->
             CursorPosition.classifyLoc fieldPat.Parsetree.ppat_loc ~pos
             = HasCursor)
    in
    let completableFromField =
      fields
      |> List.find_map (fun (loc, fieldPat) ->
             match
               CursorPosition.classifyLoc fieldPat.Parsetree.ppat_loc ~pos
             with
             | HasCursor -> (
               (* handle `{something}`, which is parsed as `{something: something}` because of punning *)
               match (loc, fieldPat.Parsetree.ppat_desc) with
               | ( {Location.txt = Longident.Lident fieldName},
                   Ppat_var {txt = varName} )
                 when fieldName = varName ->
                 Some
                   {
                     lookingToComplete = CRecordField;
                     patternPath;
                     prefix = varName;
                     alreadySeenIdents = seenFields;
                   }
               | _ ->
                 (* We can continue down into anything else *)
                 findContextInPattern fieldPat ~pos
                   ~firstCharBeforeCursorNoWhite ~debug ~seenIdentsFromParent:[]
                   ~patternPath:
                     ([
                        Completable.RField
                          {fieldName = getSimpleFieldName loc.txt};
                      ]
                     @ patternPath))
             | NoCursor -> None
             | EmptyLoc ->
               if
                 (* We only care about empty locations if there's no field with the cursor *)
                 fieldWithCursorExists
               then None
               else
                 findContextInPattern fieldPat ~pos ~debug
                   ~firstCharBeforeCursorNoWhite ~seenIdentsFromParent:[]
                   ~patternPath:
                     ([
                        Completable.RField
                          {fieldName = getSimpleFieldName loc.txt};
                      ]
                     @ patternPath))
    in
    match
      (completableFromField, fieldWithCursorExists, firstCharBeforeCursorNoWhite)
    with
    | None, false, Some ',' ->
      (* If there's no field with the cursor, and we found no completable, and the first char before the
         cursor is ',' we can assume that this is a pattern like `{someField, <com>}`.
         The parser discards any unecessary commas, so we can't leverage the parser to figure out that
         we're looking to complete for a new field. *)
      Some
        {
          lookingToComplete = CRecordField;
          patternPath;
          prefix = "";
          alreadySeenIdents = seenFields;
        }
    | completableFromField, _, _ -> completableFromField)
  | Ppat_tuple items -> (
    match findTupleItemWithCursor items ~pos ~index:0 with
    | Some (Some tuplePatternWithCursor, itemNumber) ->
      findContextInPattern tuplePatternWithCursor ~pos ~debug
        ~seenIdentsFromParent:[] ~firstCharBeforeCursorNoWhite
        ~patternPath:([Completable.PTuple {itemNumber}] @ patternPath)
    | None ->
      (* TODO: No tuple item had the cursor, but we might still be able to complete
         for the next tuple item. We just need to figure out where in the pattern
         we are. E.g. `(_, , A)` is parsed the same as `(_, A, )` or `(_, A, ,)`.
         Figuring out exactly which tuple item we're in is problematic because of that. *)
      None
    | _ -> None)
  | Ppat_or (pat1, pat2) -> (
    (* There's a few things that can happen as we're looking for or patterns.
       a. First off, we can find an or pattern with the cursor, in which case we should try to descend into it.
          E.g. `One | Two | Three({someField: s<com>})`
       b. Secondly, if there's no pattern with the cursor, there might be a or pattern with parser recovery in it.
          That means the user has written an empty or pattern, in which case we should complete for the current type.
          E.g. `One | Two | <com>` *)
    let branches =
      pat1
      |> findAllOrBranches ~branches:[pat2]
      |> List.filter (fun pat ->
             match pat.Parsetree.ppat_desc with
             | Ppat_or _ -> false
             | _ -> true)
    in
    let branchAtCursor =
      branches
      |> List.find_opt (fun pat ->
             pat.Parsetree.ppat_loc
             |> CursorPosition.classifyLoc ~pos
             = HasCursor)
    in
    let identsFromBranches =
      branches
      |> List.map (fun branch -> branch |> identListFromPat)
      |> List.flatten
    in
    match branchAtCursor with
    | None -> (
      let branchWithEmptyLoc =
        branches
        |> List.find_opt (fun pat ->
               pat.Parsetree.ppat_loc
               |> CursorPosition.classifyLoc ~pos
               = EmptyLoc)
      in
      (* If we found no branch with the cursor, look for one with an empty loc
         that's a patternhole (error recovery done by the parser). *)
      match branchWithEmptyLoc with
      | Some {ppat_desc = Ppat_extension ({txt = "rescript.patternhole"}, _)} ->
        Some
          {
            lookingToComplete = CNoContext;
            patternPath;
            prefix = "";
            alreadySeenIdents = identsFromBranches;
          }
      | _ -> None)
    | Some pattern ->
      pattern
      |> findContextInPattern ~pos ~patternPath ~debug
           ~firstCharBeforeCursorNoWhite
           ~seenIdentsFromParent:identsFromBranches)
  | _v ->
    (* if debug then
       Printf.printf "warning: unhandled pattern %s\n" (Utils.identifyPpat v);*)
    None

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

      (* This is an experiment and should most likely not live here in its final form. *)
      (* Check for: let {destructuringSomething} = someIdentifier *)
      (* Ensure cursor is inside of record pattern. *)
      (* TODO: Handle let {SomeModule.recordField} = ...*)
      (match bindings with
      | [{pvb_pat; pvb_expr = expr}] when !result = None -> (
        (* The contextPath is what we'll use to look up the root record type for this completion.
           Depending on if the destructure is nested or not, we may or may not use that directly.*)
        match exprToContextPath expr with
        | None -> ()
        | Some contextPath -> (
          match
            findContextInPattern pvb_pat ~firstCharBeforeCursorNoWhite
              ~pos:posBeforeCursor ~patternPath:[] ~seenIdentsFromParent:[]
              ~debug
          with
          | None -> ()
          | Some res ->
            setResultOpt
              (Some
                 (Completable.CtypedPattern
                    {
                      howToRetrieveSourceType = CtxPath contextPath;
                      patternPath = res.patternPath |> List.rev;
                      patternType = Destructure;
                      lookingToComplete = res.lookingToComplete;
                      prefix = res.prefix;
                      alreadySeenIdents = res.alreadySeenIdents;
                    }))))
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
        | Pexp_match (expr, cases) -> (
          (* Completes switch case destructuring
             Example: switch someIdentifier { | {completeRecordFieldsHere} => ...}*)
          let rec findCaseWithCursor cases ~typ ~identsFromCases =
            match cases with
            | case :: nextCases ->
              if
                case.Parsetree.pc_lhs.ppat_loc
                |> CursorPosition.classifyLoc ~pos:posBeforeCursor
                = typ
              then
                case.pc_lhs
                |> findContextInPattern ~firstCharBeforeCursorNoWhite
                     ~pos:posBeforeCursor ~patternPath:[] ~debug
                     ~seenIdentsFromParent:identsFromCases
              else findCaseWithCursor nextCases ~typ ~identsFromCases
            | [] -> None
          in
          match exprToContextPath expr with
          | None -> ()
          | Some contextPath -> (
            (* Collect all of the seen idents from the cases themselves. *)
            let identsFromCases =
              cases
              |> List.map (fun case ->
                     case.Parsetree.pc_lhs |> identListFromPat)
              |> List.flatten
            in
            (* First, look for a case with the cursor. *)
            let res =
              match
                findCaseWithCursor cases ~typ:HasCursor ~identsFromCases
              with
              | None ->
                (* If there's no case with the cursor, look for a case with a broken loc. That means parser recovery has happened somewhere. *)
                findCaseWithCursor cases ~typ:EmptyLoc ~identsFromCases
              | v -> v
            in
            match res with
            | None -> ()
            | Some res ->
              setResultOpt
                (Some
                   (CtypedPattern
                      {
                        howToRetrieveSourceType = CtxPath contextPath;
                        patternPath = res.patternPath |> List.rev;
                        patternType = Switch;
                        lookingToComplete = res.lookingToComplete;
                        prefix = res.prefix;
                        alreadySeenIdents = res.alreadySeenIdents;
                      }))))
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
