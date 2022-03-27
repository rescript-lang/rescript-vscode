(** Code transformations using the parser/printer and ast operations *)

let posInLoc ~pos ~loc =
  Utils.tupleOfLexing loc.Location.loc_start <= pos
  && pos < Utils.tupleOfLexing loc.loc_end

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

  let mkMapper ~pos ~changed =
    let expr (mapper : Ast_mapper.mapper) (e : Parsetree.expression) =
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
      | Some newExp ->
        changed := true;
        newExp
      | None -> Ast_mapper.default_mapper.expr mapper e
    in

    {Ast_mapper.default_mapper with expr}

  let xform ~pos structure =
    let changed = ref false in
    let mapper = mkMapper ~pos ~changed in
    let newStructure = mapper.structure mapper structure in
    if !changed then Some newStructure else None
end

let parse ~filename =
  let {Res_driver.parsetree = structure; comments} =
    Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename
  in
  let print ~structure =
    Res_printer.printImplementation ~width:!Res_cli.ResClflags.width ~comments
      structure
  in
  (structure, print)

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

let command ~path ~pos =
  if Filename.check_suffix path ".res" then
    let structure, print = parse ~filename:path in
    match IfThenElse.xform ~pos structure with
    | None -> ()
    | Some newStructure ->
      let formatted = print newStructure in
      let firstLineDifferent, lastLineEqual, newLines =
        diff ~filename:path ~newContents:formatted
      in
      Printf.printf
        "Hit IfThenElse firstLineDifferent:%d lastLineEqual:%d newLines:\n%s\n"
        firstLineDifferent lastLineEqual
        (newLines |> String.concat "\n")
