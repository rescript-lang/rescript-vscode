(* https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_documentSymbol *)

type kind =
  | Module
  | Property
  | Constructor
  | Function
  | Variable
  | Constant
  | String
  | Number
  | EnumMember
  | TypeParameter

let kindNumber = function
  | Module -> 2
  | Property -> 7
  | Constructor -> 9
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
    | Pexp_fun _ -> Function
    | Pexp_function _ -> Function
    | Pexp_constraint (e, _) -> exprKind e
    | Pexp_constant (Pconst_string _) -> String
    | Pexp_constant (Pconst_float _ | Pconst_integer _) -> Number
    | Pexp_constant _ -> Constant
    | _ -> Variable
  in
  let processTypeKind (tk : Parsetree.type_kind) =
    match tk with
    | Ptype_variant constrDecls ->
      constrDecls
      |> List.iter (fun (cd : Parsetree.constructor_declaration) ->
             symbols := (cd.pcd_name.txt, cd.pcd_loc, EnumMember) :: !symbols)
    | Ptype_record labelDecls ->
      labelDecls
      |> List.iter (fun (ld : Parsetree.label_declaration) ->
             symbols := (ld.pld_name.txt, ld.pld_loc, Property) :: !symbols)
    | _ -> ()
  in
  let processTypeDeclaration (td : Parsetree.type_declaration) =
    symbols := (td.ptype_name.txt, td.ptype_loc, TypeParameter) :: !symbols;
    processTypeKind td.ptype_kind
  in
  let processValueDescription (vd : Parsetree.value_description) =
    symbols := (vd.pval_name.txt, vd.pval_loc, Variable) :: !symbols
  in
  let processModuleBinding (mb : Parsetree.module_binding) =
    symbols := (mb.pmb_name.txt, mb.pmb_loc, Module) :: !symbols
  in
  let processModuleDeclaration (md : Parsetree.module_declaration) =
    symbols := (md.pmd_name.txt, md.pmd_loc, Module) :: !symbols
  in
  let processExtensionConstructor (et : Parsetree.extension_constructor) =
    symbols := (et.pext_name.txt, et.pext_loc, Constructor) :: !symbols
  in
  let value_binding (iterator : Ast_iterator.iterator)
      (vb : Parsetree.value_binding) =
    (match vb.pvb_pat.ppat_desc with
    | Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _) ->
      symbols := (txt, vb.pvb_loc, exprKind vb.pvb_expr) :: !symbols
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator vb
  in
  let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
    (match e.pexp_desc with
    | Pexp_letmodule ({txt}, modExpr, _) ->
      symbols :=
        (txt, {e.pexp_loc with loc_end = modExpr.pmod_loc.loc_end}, Module)
        :: !symbols
    | Pexp_letexception (ec, _) -> processExtensionConstructor ec
    | _ -> ());
    Ast_iterator.default_iterator.expr iterator e
  in
  let structure_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.structure_item) =
    (match item.pstr_desc with
    | Pstr_value _ -> ()
    | Pstr_primitive vd -> processValueDescription vd
    | Pstr_type (_, typDecls) -> typDecls |> List.iter processTypeDeclaration
    | Pstr_module mb -> processModuleBinding mb
    | Pstr_recmodule mbs -> mbs |> List.iter processModuleBinding
    | Pstr_exception ec -> processExtensionConstructor ec
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
    | Psig_exception ec -> processExtensionConstructor ec
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
      expr;
      module_expr;
      signature_item;
      structure_item;
      value_binding;
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
               kind = kindNumber kind;
             })
    |> String.concat ",\n"
  in
  print_endline ("[\n" ^ result ^ "\n]")
