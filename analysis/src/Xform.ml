(** Code transformations using the parser/printer and ast operations *)

let isBracedExpr = Res_parsetree_viewer.isBracedExpr
let posInLoc ~pos ~loc = Loc.start loc <= pos && pos < Loc.end_ loc

let mkPosition (p : Lexing.position) =
  let line, character = Pos.ofLexing p in
  {Protocol.line; character}

let rangeOfLoc (loc : Location.t) =
  let start = mkPosition loc.loc_start in
  let end_ = mkPosition loc.loc_end in
  {Protocol.start; end_}

module IfThenElse = struct
  (* Convert if-then-else to switch *)

  let rec listToPat ~itemToPat = function
    | [] -> Some []
    | x :: xList -> (
      match (itemToPat x, listToPat ~itemToPat xList) with
      | Some p, Some pList -> Some (p :: pList)
      | _ -> None)

  let rec expToPat (exp : Parsetree.expression) =
    let mkPat ppat_desc =
      Ast_helper.Pat.mk ~loc:exp.pexp_loc ~attrs:exp.pexp_attributes ppat_desc
    in
    match exp.pexp_desc with
    | Pexp_construct (lid, None) -> Some (mkPat (Ppat_construct (lid, None)))
    | Pexp_construct (lid, Some e1) -> (
      match expToPat e1 with
      | None -> None
      | Some p1 -> Some (mkPat (Ppat_construct (lid, Some p1))))
    | Pexp_variant (label, None) -> Some (mkPat (Ppat_variant (label, None)))
    | Pexp_variant (label, Some e1) -> (
      match expToPat e1 with
      | None -> None
      | Some p1 -> Some (mkPat (Ppat_variant (label, Some p1))))
    | Pexp_constant c -> Some (mkPat (Ppat_constant c))
    | Pexp_tuple eList -> (
      match listToPat ~itemToPat:expToPat eList with
      | None -> None
      | Some patList -> Some (mkPat (Ppat_tuple patList)))
    | Pexp_record (items, None) -> (
      let itemToPat (x, e) =
        match expToPat e with None -> None | Some p -> Some (x, p)
      in
      match listToPat ~itemToPat items with
      | None -> None
      | Some patItems -> Some (mkPat (Ppat_record (patItems, Closed))))
    | Pexp_record (_, Some _) -> None
    | _ -> None

  let mkIterator ~pos ~changed =
    let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
      let newExp =
        match e.pexp_desc with
        | Pexp_ifthenelse
            ( {
                pexp_desc =
                  Pexp_apply
                    ( {
                        pexp_desc =
                          Pexp_ident {txt = Lident (("=" | "<>") as op)};
                      },
                      [(Nolabel, arg1); (Nolabel, arg2)] );
              },
              e1,
              Some e2 )
          when posInLoc ~pos ~loc:e.pexp_loc -> (
          let e1, e2 = if op = "=" then (e1, e2) else (e2, e1) in
          let mkMatch ~arg ~pat =
            let cases =
              [
                Ast_helper.Exp.case pat e1;
                Ast_helper.Exp.case (Ast_helper.Pat.any ()) e2;
              ]
            in
            Ast_helper.Exp.match_ ~loc:e.pexp_loc ~attrs:e.pexp_attributes arg
              cases
          in

          match expToPat arg2 with
          | None -> (
            match expToPat arg1 with
            | None -> None
            | Some pat1 ->
              let newExp = mkMatch ~arg:arg2 ~pat:pat1 in
              Some newExp)
          | Some pat2 ->
            let newExp = mkMatch ~arg:arg1 ~pat:pat2 in
            Some newExp)
        | _ -> None
      in
      match newExp with
      | Some newExp -> changed := Some newExp
      | None -> Ast_iterator.default_iterator.expr iterator e
    in

    {Ast_iterator.default_iterator with expr}

  let xform ~pos structure =
    let changed = ref None in
    let iterator = mkIterator ~pos ~changed in
    iterator.structure iterator structure;
    !changed
end

module AddBracesToFn = struct
  (* Add braces to fn without braces *)

  let mkIterator ~pos ~changed =
    (* While iterating the AST, keep info on which structure item we are in.
       Printing from the structure item, rather than the body of the function,
       gives better local pretty printing *)
    let currentStructureItem = ref None in

    let structure_item (iterator : Ast_iterator.iterator)
        (item : Parsetree.structure_item) =
      let saved = !currentStructureItem in
      currentStructureItem := Some item;
      Ast_iterator.default_iterator.structure_item iterator item;
      currentStructureItem := saved
    in
    let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
      let bracesAttribute =
        let loc =
          {
            Location.none with
            loc_start = Lexing.dummy_pos;
            loc_end =
              {
                Lexing.dummy_pos with
                pos_lnum = Lexing.dummy_pos.pos_lnum + 1 (* force line break *);
              };
          }
        in
        (Location.mkloc "ns.braces" loc, Parsetree.PStr [])
      in
      let isFunction = function
        | {Parsetree.pexp_desc = Pexp_fun _} -> true
        | _ -> false
      in
      (match e.pexp_desc with
      | Pexp_fun (_, _, _, bodyExpr)
        when posInLoc ~pos ~loc:bodyExpr.pexp_loc
             && isBracedExpr bodyExpr = false
             && isFunction bodyExpr = false ->
        bodyExpr.pexp_attributes <- bracesAttribute :: bodyExpr.pexp_attributes;
        changed := !currentStructureItem
      | _ -> ());
      Ast_iterator.default_iterator.expr iterator e
    in

    {Ast_iterator.default_iterator with expr; structure_item}

  let xform ~pos structure =
    let changed = ref None in
    let iterator = mkIterator ~pos ~changed in
    iterator.structure iterator structure;
    !changed
end

module AddTypeAnnotation = struct
  (* Add type annotation to value declaration *)

  type annotation = Plain | WithParens

  let mkIterator ~pos ~result =
    let processPattern ?(isUnlabeledOnlyArg = false) (pat : Parsetree.pattern) =
      match pat.ppat_desc with
      | Ppat_var {loc} when posInLoc ~pos ~loc ->
        result := Some (if isUnlabeledOnlyArg then WithParens else Plain)
      | _ -> ()
    in
    let rec processFunction ~argNum (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_fun (argLabel, _, pat, e) ->
        let isUnlabeledOnlyArg =
          argNum = 1 && argLabel = Nolabel
          && match e.pexp_desc with Pexp_fun _ -> false | _ -> true
        in
        processPattern ~isUnlabeledOnlyArg pat;
        processFunction ~argNum:(argNum + 1) e
      | _ -> ()
    in
    let structure_item (iterator : Ast_iterator.iterator)
        (si : Parsetree.structure_item) =
      match si.pstr_desc with
      | Pstr_value (_recFlag, bindings) ->
        let processBinding (vb : Parsetree.value_binding) =
          let isReactComponent =
            (* Can't add a type annotation to a react component, or the compiler crashes *)
            vb.pvb_attributes
            |> List.exists (function
                 | {Location.txt = "react.component"}, _payload -> true
                 | _ -> false)
          in
          if not isReactComponent then processPattern vb.pvb_pat;
          processFunction vb.pvb_expr
        in
        bindings |> List.iter (processBinding ~argNum:1);
        Ast_iterator.default_iterator.structure_item iterator si
      | _ -> Ast_iterator.default_iterator.structure_item iterator si
    in
    {Ast_iterator.default_iterator with structure_item}

  let getAction ~path ~pos ~full ~structure =
    let line, col = pos in

    let result = ref None in
    let iterator = mkIterator ~pos ~result in
    iterator.structure iterator structure;
    match !result with
    | None -> None
    | Some annotation -> (
      match References.getLocItem ~full ~line ~col with
      | None -> None
      | Some locItem -> (
        match locItem.locType with
        | Typed (name, typ, _) ->
          let range, newText =
            match annotation with
            | Plain ->
              ( rangeOfLoc {locItem.loc with loc_start = locItem.loc.loc_end},
                ": " ^ (typ |> Shared.typeToString) )
            | WithParens ->
              ( rangeOfLoc locItem.loc,
                "(" ^ name ^ ": " ^ (typ |> Shared.typeToString) ^ ")" )
          in
          let codeAction =
            CodeActions.make ~title:"Add type annotation" ~kind:RefactorRewrite
              ~uri:path ~newText ~range
          in
          Some codeAction
        | _ -> None))
end

let indent n text =
  let spaces = String.make n ' ' in
  let len = String.length text in
  let text =
    if len != 0 && text.[len - 1] = '\n' then String.sub text 0 (len - 1)
    else text
  in
  let lines = String.split_on_char '\n' text in
  match lines with
  | [] -> ""
  | [line] -> line
  | line :: lines ->
    line ^ "\n"
    ^ (lines |> List.map (fun line -> spaces ^ line) |> String.concat "\n")

let parse ~filename =
  let {Res_driver.parsetree = structure; comments} =
    Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename
  in
  let filterComments ~loc comments =
    (* Relevant comments in the range of the expression *)
    let filter comment =
      posInLoc ~pos:(Loc.start (Res_comment.loc comment)) ~loc
    in
    comments |> List.filter filter
  in
  let printExpr ~(range : Protocol.range) (expr : Parsetree.expression) =
    let structure = [Ast_helper.Str.eval ~loc:expr.pexp_loc expr] in
    structure
    |> Res_printer.printImplementation ~width:!Res_cli.ResClflags.width
         ~comments:(comments |> filterComments ~loc:expr.pexp_loc)
    |> indent range.start.character
  in
  let printStructureItem ~(range : Protocol.range)
      (item : Parsetree.structure_item) =
    let structure = [item] in
    structure
    |> Res_printer.printImplementation ~width:!Res_cli.ResClflags.width
         ~comments:(comments |> filterComments ~loc:item.pstr_loc)
    |> indent range.start.character
  in
  (structure, printExpr, printStructureItem)

let diff ~filename ~newContents =
  match Files.readFile ~filename with
  | None -> assert false
  | Some oldContents ->
    let rec findFirstLineDifferent n old new_ =
      match (old, new_) with
      | old1 :: oldRest, new1 :: newRest ->
        if old1 = new1 then findFirstLineDifferent (n + 1) oldRest newRest
        else (n, old, new_)
      | _ -> (n, old, new_)
    in
    let oldLines = String.split_on_char '\n' oldContents in
    let newLines = String.split_on_char '\n' newContents in
    let firstLineDifferent, old, new_ =
      findFirstLineDifferent 0 oldLines newLines
    in
    let firstLineR, _oldR, newR =
      findFirstLineDifferent 0 (List.rev old) (List.rev new_)
    in
    let lastLineEqual = firstLineDifferent + List.length old - firstLineR in
    let newLines = List.rev newR in
    (firstLineDifferent, lastLineEqual, newLines)

let extractCodeActions ~path ~pos ~currentFile =
  let codeActions = ref [] in
  if Filename.check_suffix currentFile ".res" then (
    let structure, printExpr, printStructureItem =
      parse ~filename:currentFile
    in
    let fullOpt = Cmt.fromPath ~path in
    (match fullOpt with
    | None -> ()
    | Some full -> (
      match AddTypeAnnotation.getAction ~path ~pos ~full ~structure with
      | None -> ()
      | Some action -> codeActions := action :: !codeActions));
    (match IfThenElse.xform ~pos structure with
    | None -> ()
    | Some newExpr ->
      let range = rangeOfLoc newExpr.pexp_loc in
      let newText = printExpr ~range newExpr in
      let codeAction =
        CodeActions.make ~title:"Replace with switch" ~kind:RefactorRewrite
          ~uri:path ~newText ~range
      in
      codeActions := codeAction :: !codeActions);

    match AddBracesToFn.xform ~pos structure with
    | None -> ()
    | Some newStructureItem ->
      let range = rangeOfLoc newStructureItem.pstr_loc in
      let newText = printStructureItem ~range newStructureItem in
      let codeAction =
        CodeActions.make ~title:"Add braces to function" ~kind:RefactorRewrite
          ~uri:path ~newText ~range
      in
      codeActions := codeAction :: !codeActions);
  !codeActions
