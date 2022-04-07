module SymbolKind = struct
  type t =
    | Module
    | Property
    | Function
    | Variable
    | Constant
    | String
    | Number
    | EnumMember
    | TypeParameter
end

let symbolKind = function
  | SymbolKind.Module -> 2
  | Property -> 7
  | Function -> 12
  | Variable -> 13
  | Constant -> 14
  | String -> 15
  | Number -> 16
  | EnumMember -> 22
  | TypeParameter -> 26

let command ~path =
  let symbols = ref [] in
  let rec exprKind (exp : Parsetree.expression) =
    match exp.pexp_desc with
    | Pexp_fun _ -> SymbolKind.Function
    | Pexp_function _ -> SymbolKind.Function
    | Pexp_constraint (e, _) -> exprKind e
    | Pexp_constant (Pconst_string _) -> SymbolKind.String
    | Pexp_constant (Pconst_float _ | Pconst_integer _) -> SymbolKind.Number
    | Pexp_constant _ -> SymbolKind.Constant
    | _ -> SymbolKind.Variable
  in
  let processValueBinding (vb : Parsetree.value_binding) =
    match vb.pvb_pat.ppat_desc with
    | Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _) ->
      symbols := (txt, vb.pvb_loc, exprKind vb.pvb_expr) :: !symbols
    | _ -> ()
  in
  let processTypeKind (tk : Parsetree.type_kind) =
    match tk with
    | Ptype_variant constrDecls ->
      constrDecls
      |> List.iter (fun (cd : Parsetree.constructor_declaration) ->
             symbols :=
               (cd.pcd_name.txt, cd.pcd_loc, SymbolKind.EnumMember) :: !symbols)
    | Ptype_record labelDecls ->
      labelDecls
      |> List.iter (fun (ld : Parsetree.label_declaration) ->
             symbols :=
               (ld.pld_name.txt, ld.pld_loc, SymbolKind.Property) :: !symbols)
    | _ -> ()
  in
  let processTypeDeclaration (td : Parsetree.type_declaration) =
    symbols :=
      (td.ptype_name.txt, td.ptype_loc, SymbolKind.TypeParameter) :: !symbols;
    processTypeKind td.ptype_kind
  in
  let processValueDescription (vd : Parsetree.value_description) =
    symbols := (vd.pval_name.txt, vd.pval_loc, SymbolKind.Variable) :: !symbols
  in
  let processModuleBinding (mb : Parsetree.module_binding) =
    symbols := (mb.pmb_name.txt, mb.pmb_loc, SymbolKind.Module) :: !symbols
  in
  let processModuleDeclaration (md : Parsetree.module_declaration) =
    symbols := (md.pmd_name.txt, md.pmd_loc, SymbolKind.Module) :: !symbols
  in
  let structure_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.structure_item) =
    (match item.pstr_desc with
    | Pstr_value (_recFlag, valueBindings) ->
      valueBindings |> List.iter processValueBinding
    | Pstr_primitive vd -> processValueDescription vd
    | Pstr_type (_, typDecls) -> typDecls |> List.iter processTypeDeclaration
    | Pstr_module mb -> processModuleBinding mb
    | Pstr_recmodule mbs -> mbs |> List.iter processModuleBinding
    | _ -> Ast_iterator.default_iterator.structure_item iterator item);
    Ast_iterator.default_iterator.structure_item iterator item
  in
  let signature_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.signature_item) =
    (match item.psig_desc with
    | Psig_value vd -> processValueDescription vd
    | Psig_type (_, typDecls) -> typDecls |> List.iter processTypeDeclaration
    | Psig_module md -> processModuleDeclaration md
    | Psig_recmodule mds -> mds |> List.iter processModuleDeclaration
    | _ -> ());
    Ast_iterator.default_iterator.signature_item iterator item
  in
  let module_expr (iterator : Ast_iterator.iterator)
      (me : Parsetree.module_expr) =
    match me.pmod_desc with
    | Pmod_constraint (modExpr, _modTyp) ->
      (* Don't double-list items in implementation and interface *)
      Ast_iterator.default_iterator.module_expr iterator modExpr
    | _ -> Ast_iterator.default_iterator.module_expr iterator me
  in
  let iterator =
    {
      Ast_iterator.default_iterator with
      structure_item;
      signature_item;
      module_expr;
    }
  in

  (if Filename.check_suffix path ".res" then
   let parser =
     Res_driver.parsingEngine.parseImplementation ~forPrinter:false
   in
   let {Res_driver.parsetree = structure} = parser ~filename:path in
   iterator.structure iterator structure |> ignore
  else
    let parser = Res_driver.parsingEngine.parseInterface ~forPrinter:false in
    let {Res_driver.parsetree = signature} = parser ~filename:path in
    iterator.signature iterator signature |> ignore);
  let result =
    !symbols
    |> List.rev_map (fun (name, loc, kind) ->
           Protocol.stringifyDocumentSymbolItem
             {
               name;
               location =
                 {
                   uri = Uri2.toString (Uri2.fromPath path);
                   range = Utils.cmtLocToRange loc;
                 };
               kind = symbolKind kind;
             })
    |> String.concat ",\n"
  in
  print_endline ("[\n" ^ result ^ "\n]")
