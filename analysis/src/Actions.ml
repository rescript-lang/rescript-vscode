let posInLoc ~pos ~loc =
  Utils.tupleOfLexing loc.Location.loc_start <= pos
  && pos < Utils.tupleOfLexing loc.loc_end

let rec expToPat (exp : Parsetree.expression) =
  let mkPat ppat_desc =
    Ast_helper.Pat.mk ~loc:exp.pexp_loc ~attrs:exp.pexp_attributes ppat_desc
  in
  match exp.pexp_desc with
  | Pexp_construct (lid, None) ->
    Some (mkPat (Parsetree.Ppat_construct (lid, None)))
  | Pexp_construct (lid, Some e1) -> (
    match expToPat e1 with
    | None -> None
    | Some p1 -> Some (mkPat (Parsetree.Ppat_construct (lid, Some p1))))
  | Pexp_constant c -> Some (mkPat (Parsetree.Ppat_constant c))
  | _ -> None

let mkMapper ~pos ~changed =
  let value_binding (mapper : Ast_mapper.mapper) (vb : Parsetree.value_binding)
      =
    let newExp =
      match vb.pvb_pat.ppat_desc with
      | Ppat_var {txt; loc} when posInLoc ~pos ~loc -> (
        match vb.pvb_expr.pexp_desc with
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
              Some e2 ) -> (
          let e1, e2 = if op = "=" then (e1, e2) else (e2, e1) in
          let mkMatch ~arg ~pat =
            let cases =
              [
                Ast_helper.Exp.case pat e1;
                Ast_helper.Exp.case (Ast_helper.Pat.any ()) e2;
              ]
            in
            Ast_helper.Exp.match_ ~loc:vb.pvb_expr.pexp_loc
              ~attrs:vb.pvb_expr.pexp_attributes arg cases
          in

          match expToPat arg2 with
          | None -> (
            match expToPat arg1 with
            | None -> None
            | Some pat1 ->
              let newExp = mkMatch ~arg:arg2 ~pat:pat1 in
              Printf.printf "Hit %s\n" txt;
              Some newExp)
          | Some pat2 ->
            let newExp = mkMatch ~arg:arg1 ~pat:pat2 in
            Printf.printf "Hit %s\n" txt;
            Some newExp)
        | _ -> None)
      | _ -> None
    in
    match newExp with
    | Some newExp ->
      changed := true;
      {vb with pvb_expr = newExp}
    | None -> Ast_mapper.default_mapper.value_binding mapper vb
  in

  {Ast_mapper.default_mapper with value_binding}

let command ~path ~pos =
  if Filename.check_suffix path ".res" then
    let parser =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:false
    in
    let {Res_driver.parsetree = structure; comments; filename} =
      parser ~filename:path
    in
    let printer =
      Res_driver.printEngine.printImplementation
        ~width:!Res_cli.ResClflags.width ~comments ~filename
    in
    let changed = ref false in
    let mapper = mkMapper ~pos ~changed in
    let newStructure = mapper.structure mapper structure in
    if !changed then printer newStructure
