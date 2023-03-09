(** Code transformations using the parser/printer and ast operations *)

let isBracedExpr = Res_parsetree_viewer.isBracedExpr

let mkPosition (pos : Pos.t) =
  let line, character = pos in
  {Protocol.line; character}

let rangeOfLoc (loc : Location.t) =
  let start = loc |> Loc.start |> mkPosition in
  let end_ = loc |> Loc.end_ |> mkPosition in
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
        match expToPat e with
        | None -> None
        | Some p -> Some (x, p)
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
          when Loc.hasPos ~pos e.pexp_loc -> (
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

  let xform ~pos ~codeActions ~printExpr ~path structure =
    let changed = ref None in
    let iterator = mkIterator ~pos ~changed in
    iterator.structure iterator structure;
    match !changed with
    | None -> ()
    | Some newExpr ->
      let range = rangeOfLoc newExpr.pexp_loc in
      let newText = printExpr ~range newExpr in
      let codeAction =
        CodeActions.make ~title:"Replace with switch" ~kind:RefactorRewrite
          ~uri:path ~newText ~range
      in
      codeActions := codeAction :: !codeActions
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
        (Location.mkloc "res.braces" loc, Parsetree.PStr [])
      in
      let isFunction = function
        | {Parsetree.pexp_desc = Pexp_fun _} -> true
        | _ -> false
      in
      (match e.pexp_desc with
      | Pexp_fun (_, _, _, bodyExpr)
        when Loc.hasPos ~pos bodyExpr.pexp_loc
             && isBracedExpr bodyExpr = false
             && isFunction bodyExpr = false ->
        bodyExpr.pexp_attributes <- bracesAttribute :: bodyExpr.pexp_attributes;
        changed := !currentStructureItem
      | _ -> ());
      Ast_iterator.default_iterator.expr iterator e
    in

    {Ast_iterator.default_iterator with expr; structure_item}

  let xform ~pos ~codeActions ~path ~printStructureItem structure =
    let changed = ref None in
    let iterator = mkIterator ~pos ~changed in
    iterator.structure iterator structure;
    match !changed with
    | None -> ()
    | Some newStructureItem ->
      let range = rangeOfLoc newStructureItem.pstr_loc in
      let newText = printStructureItem ~range newStructureItem in
      let codeAction =
        CodeActions.make ~title:"Add braces to function" ~kind:RefactorRewrite
          ~uri:path ~newText ~range
      in
      codeActions := codeAction :: !codeActions
end

module AddTypeAnnotation = struct
  (* Add type annotation to value declaration *)

  type annotation = Plain | WithParens

  let mkIterator ~pos ~result =
    let processPattern ?(isUnlabeledOnlyArg = false) (pat : Parsetree.pattern) =
      match pat.ppat_desc with
      | Ppat_var {loc} when Loc.hasPos ~pos loc ->
        result := Some (if isUnlabeledOnlyArg then WithParens else Plain)
      | _ -> ()
    in
    let rec processFunction ~argNum (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_fun (argLabel, _, pat, e) ->
        let isUnlabeledOnlyArg =
          argNum = 1 && argLabel = Nolabel
          &&
          match e.pexp_desc with
          | Pexp_fun _ -> false
          | _ -> true
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
          (* Can't add a type annotation to a react component, or the compiler crashes *)
          let isReactComponent = Utils.isReactComponent vb in
          if not isReactComponent then processPattern vb.pvb_pat;
          processFunction vb.pvb_expr
        in
        bindings |> List.iter (processBinding ~argNum:1);
        Ast_iterator.default_iterator.structure_item iterator si
      | _ -> Ast_iterator.default_iterator.structure_item iterator si
    in
    {Ast_iterator.default_iterator with structure_item}

  let xform ~path ~pos ~full ~structure ~codeActions ~debug =
    let result = ref None in
    let iterator = mkIterator ~pos ~result in
    iterator.structure iterator structure;
    match !result with
    | None -> ()
    | Some annotation -> (
      match References.getLocItem ~full ~pos ~debug with
      | None -> ()
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
          codeActions := codeAction :: !codeActions
        | _ -> ()))
end

let parse ~filename =
  let {Res_driver.parsetree = structure; comments} =
    Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename
  in
  let filterComments ~loc comments =
    (* Relevant comments in the range of the expression *)
    let filter comment =
      Loc.hasPos ~pos:(Loc.start (Res_comment.loc comment)) loc
    in
    comments |> List.filter filter
  in
  let printExpr ~(range : Protocol.range) (expr : Parsetree.expression) =
    let structure = [Ast_helper.Str.eval ~loc:expr.pexp_loc expr] in
    structure
    |> Res_printer.printImplementation ~width:!Res_cli.ResClflags.width
         ~comments:(comments |> filterComments ~loc:expr.pexp_loc)
    |> Utils.indent range.start.character
  in
  let printStructureItem ~(range : Protocol.range)
      (item : Parsetree.structure_item) =
    let structure = [item] in
    structure
    |> Res_printer.printImplementation ~width:!Res_cli.ResClflags.width
         ~comments:(comments |> filterComments ~loc:item.pstr_loc)
    |> Utils.indent range.start.character
  in
  (structure, printExpr, printStructureItem)

let extractCodeActions ~path ~pos ~currentFile ~debug =
  match Cmt.loadFullCmtFromPath ~path with
  | Some full when Files.classifySourceFile currentFile = Res ->
    let structure, printExpr, printStructureItem =
      parse ~filename:currentFile
    in
    let codeActions = ref [] in
    AddTypeAnnotation.xform ~path ~pos ~full ~structure ~codeActions ~debug;
    IfThenElse.xform ~pos ~codeActions ~printExpr ~path structure;
    AddBracesToFn.xform ~pos ~codeActions ~path ~printStructureItem structure;
    !codeActions
  | _ -> []
