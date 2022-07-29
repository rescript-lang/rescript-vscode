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
      let uri = Uri.fromPath path |> Uri.toString in
      let codeAction =
        CodeActions.make ~title:"Replace with switch" ~kind:RefactorRewrite
          ~edit:
            {
              documentChanges =
                [
                  TextDocumentEdit
                    {
                      textDocument = {version = None; uri};
                      edits = [{newText; range}];
                    };
                ];
            }
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
        (Location.mkloc "ns.braces" loc, Parsetree.PStr [])
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
      let uri = Uri.fromPath path |> Uri.toString in
      let codeAction =
        CodeActions.make ~title:"Add braces to function" ~kind:RefactorRewrite
          ~edit:
            {
              documentChanges =
                [
                  TextDocumentEdit
                    {
                      textDocument = {version = None; uri};
                      edits = [{newText; range}];
                    };
                ];
            }
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
          let uri = Uri.fromPath path |> Uri.toString in
          let codeAction =
            CodeActions.make ~title:"Add type annotation" ~kind:RefactorRewrite
              ~edit:
                {
                  documentChanges =
                    [
                      TextDocumentEdit
                        {
                          textDocument = {version = None; uri};
                          edits = [{newText; range}];
                        };
                    ];
                }
          in
          codeActions := codeAction :: !codeActions
        | _ -> ()))
end

module TypeToModule = struct
  (* Convert a type it into its own submodule *)
  let mkIterator ~pos ~result ~newTypeName =
    let changeTypeDecl (typ : Parsetree.type_declaration) ~txt =
      match typ.ptype_manifest with
      | None ->
        Ast_helper.Type.mk
          {txt; loc = typ.ptype_name.loc}
          ~loc:typ.ptype_loc ~attrs:typ.ptype_attributes
          ~params:typ.ptype_params ~cstrs:typ.ptype_cstrs ~kind:typ.ptype_kind
          ~priv:typ.ptype_private
      | Some manifest ->
        Ast_helper.Type.mk
          {txt; loc = typ.ptype_name.loc}
          ~loc:typ.ptype_loc ~attrs:typ.ptype_attributes
          ~params:typ.ptype_params ~cstrs:typ.ptype_cstrs ~kind:typ.ptype_kind
          ~priv:typ.ptype_private ~manifest
    in

    let structure_item (iterator : Ast_iterator.iterator)
        (si : Parsetree.structure_item) =
      match si.pstr_desc with
      | Pstr_type (Nonrecursive, firstypeDec :: rest)
        when Loc.hasPos ~pos si.pstr_loc ->
        let loc = si.pstr_loc in
        let firsLetterOfModule =
          String.get firstypeDec.ptype_name.txt 0
          |> Char.uppercase_ascii |> String.make 1
        in
        let restName =
          String.sub firstypeDec.ptype_name.txt 1
            (String.length firstypeDec.ptype_name.txt - 1)
        in
        let modName = firsLetterOfModule ^ restName in
        let newFistType = changeTypeDecl firstypeDec ~txt:newTypeName in
        let restStrucTypes =
          Ast_helper.Str.type_ ~loc Nonrecursive ([newFistType] @ rest)
        in
        let pmb_expr = Ast_helper.Mod.structure ~loc [restStrucTypes] in
        let mod_expr =
          Parsetree.Pstr_module
            {
              pmb_name = {txt = modName; loc};
              pmb_expr;
              pmb_attributes = [];
              pmb_loc = loc;
            }
        in
        result :=
          Some
            ( Ast_helper.Str.mk ~loc mod_expr,
              firstypeDec.ptype_name.loc,
              modName );
        Ast_iterator.default_iterator.structure_item iterator si
      | _ -> ()
    in
    {Ast_iterator.default_iterator with structure_item}

  let xform ~path ~pos ~codeActions ~printStructureItem structure ~debug =
    let result = ref None in
    let newTypeName = "t" in
    let iterator = mkIterator ~pos ~result ~newTypeName in
    iterator.structure iterator structure;
    match !result with
    | None -> ()
    | Some (newStructureItem, locOfRef, modName) ->
      let range = rangeOfLoc newStructureItem.pstr_loc in
      let newText = printStructureItem ~range newStructureItem in
      let rangeRef = rangeOfLoc locOfRef in
      let newName = modName ^ "." ^ newTypeName in
      let uri = Uri.fromPath path |> Uri.toString in
      let renameReferences =
        Rename.command ~path
          ~pos:(rangeRef.start.line, rangeRef.start.character)
          ~newName ~debug
      in
      let initialChange =
        Protocol.TextDocumentEdit
          {textDocument = {version = None; uri}; edits = [{newText; range}]}
      in
      let documentChanges =
        match renameReferences with
        | None -> [initialChange]
        | Some workspaceEdit ->
          let referencesChanged =
            workspaceEdit.documentChanges
            |> List.map (fun (documentChange : Protocol.documentChange) ->
                   match documentChange with
                   | TextDocumentEdit textEdit ->
                     let textDocument = textEdit.textDocument in
                     let edits =
                       textEdit.edits
                       |> List.filter (fun (edits : Protocol.textEdit) ->
                              edits.range <> rangeRef)
                     in
                     Protocol.TextDocumentEdit {textDocument; edits}
                   | _ -> documentChange)
          in
          [initialChange] @ referencesChanged
      in
      let codeAction =
        CodeActions.make
          ~title:("Convert type to module " ^ modName)
          ~kind:RefactorRewrite ~edit:{documentChanges}
      in
      codeActions := codeAction :: !codeActions
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
      Loc.hasPos ~pos:(Loc.start (Res_comment.loc comment)) loc
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

let extractCodeActions ~path ~pos ~currentFile ~debug =
  match Cmt.fullFromPath ~path with
  | Some full when Filename.check_suffix currentFile ".res" ->
    let structure, printExpr, printStructureItem =
      parse ~filename:currentFile
    in
    let codeActions = ref [] in
    AddTypeAnnotation.xform ~path ~pos ~full ~structure ~codeActions ~debug;
    IfThenElse.xform ~pos ~codeActions ~printExpr ~path structure;
    AddBracesToFn.xform ~pos ~codeActions ~path ~printStructureItem structure;
    TypeToModule.xform ~path ~pos ~codeActions ~printStructureItem structure
      ~debug;
    !codeActions
  | _ -> []
