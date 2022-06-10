open SharedTypes

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

let hintPrefix label = Printf.sprintf ": %s" label
let hintkindNumber = function TypeParameter | EnumMember -> 2 | _ -> 1

let locItemToTypeHint ~full:{file; package} locItem =
  match locItem.locType with
  (* | TypeDefinition (name, decl, _stamp) -> *)
  (*   let typeDef = Shared.declToString name decl in *)
  (*   Some (hintPrefix typeDef) *)
  (* | LModule (Definition (stamp, _tip)) | LModule (LocalReference (stamp, _tip)) *)
  (*   -> ( *)
  (*   match Stamps.findModule file.stamps stamp with *)
  (*   | None -> None *)
  (*   | Some md -> ( *)
  (*     match References.resolveModuleReference ~file ~package md with *)
  (*     | None -> None *)
  (*     | Some (file, declared) -> *)
  (*       let name, docstring = *)
  (*         match declared with *)
  (*         | Some d -> (d.name.txt, d.docstring) *)
  (*         | None -> (file.moduleName, file.structure.docstring) *)
  (*       in *)
  (*       Hover.showModule ~docstring ~name ~file declared)) *)
  (* | LModule (GlobalReference (moduleName, path, tip)) -> ( *)
  (*   match ProcessCmt.fileForModule ~package moduleName with *)
  (*   | None -> None *)
  (*   | Some file -> ( *)
  (*     let env = QueryEnv.fromFile file in *)
  (*     match ResolvePath.resolvePath ~env ~path ~package with *)
  (*     | None -> None *)
  (*     | Some (env, name) -> ( *)
  (*       match References.exportedForTip ~env name tip with *)
  (*       | None -> None *)
  (*       | Some stamp -> ( *)
  (*         match Stamps.findModule file.stamps stamp with *)
  (*         | None -> None *)
  (*         | Some md -> ( *)
  (*           match References.resolveModuleReference ~file ~package md with *)
  (*           | None -> None *)
  (*           | Some (file, declared) -> *)
  (*             let name, docstring = *)
  (*               match declared with *)
  (*               | Some d -> (d.name.txt, d.docstring) *)
  (*               | None -> (file.moduleName, file.structure.docstring) *)
  (*             in *)
  (*             Hover.showModule ~docstring ~name ~file declared))))) *)
  | LModule NotFound -> None
  (* | TopLevelModule name -> ( *)
  (*   match ProcessCmt.fileForModule ~package name with *)
  (*   | None -> None *)
  (*   | Some file -> *)
  (*     Hover.showModule ~docstring:file.structure.docstring ~name:file.moduleName *)
  (*       ~file None) *)
  | Typed (_, _, Definition (_, (Field _ | Constructor _))) -> None
  | Constant t ->
    Some
      (hintPrefix
         (match t with
         | Const_int _ -> "int"
         | Const_char _ -> "char"
         | Const_string _ -> "string"
         | Const_float _ -> "float"
         | Const_int32 _ -> "int32"
         | Const_int64 _ -> "int64"
         | Const_nativeint _ -> "int"))
  | Typed (_, t, locKind) ->
    (* let fromType ~docstring typ = *)
    (*   let typeString = Hover.codeBlock (typ |> Shared.typeToString) in *)
    (*   let extraTypeInfo = *)
    (*     let env = QueryEnv.fromFile file in *)
    (*     match typ |> Shared.digConstructor with *)
    (*     | None -> None *)
    (*     | Some path -> ( *)
    (*       match References.digConstructor ~env ~package path with *)
    (*       | None -> None *)
    (*       | Some (_env, {docstring; name = {txt}; item = {decl}}) -> *)
    (*         if Utils.isUncurriedInternal path then None *)
    (*         else Some (decl |> Shared.declToString txt, docstring)) *)
    (*   in *)
    (*   let typeString, docstring = *)
    (*     match extraTypeInfo with *)
    (*     | None -> (typeString, docstring) *)
    (*     | Some (extra, extraDocstring) -> *)
    (*       (typeString ^ "\n\n" ^ Hover.codeBlock extra, extraDocstring) *)
    (*   in *)
    (*   (typeString, docstring) *)
    (* in *)
    let parts =
      match References.definedForLoc ~file ~package locKind with
      | None ->
        let typeString = Shared.typeToString t in
        typeString
      | Some (docstring, res) -> (
        match res with
        | `Declared ->
          let typeString = Shared.typeToString t in
          typeString
        | `Constructor {cname = {txt}; args} ->
            let typeString = Shared.typeToString t in
            typeString
          (* let typeString, docstring = t |> fromType ~docstring in *)
          (* let argsString = *)
          (*   match args with *)
          (*   | [] -> "" *)
          (*   | _ -> *)
          (*     args *)
          (*     |> List.map (fun (t, _) -> (Shared.typeToString t) ^ "hre") *)
          (*     |> String.concat ", " |> Printf.sprintf "(%s)" *)
          (* in *)
          (* typeString *)
        | `Field ->
            let typeString = Shared.typeToString t in
            typeString
          (* let typeString, docstring = t |> fromType ~docstring in *)
          (* typeString) *)
          )
    in
    Some parts

let inlay ~path ~debug =
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
  (* let processTypeKind (tk : Parsetree.type_kind) = *)
  (*   match tk with *)
  (*   | Ptype_variant constrDecls -> *)
  (*     constrDecls *)
  (*     |> List.iter (fun (cd : Parsetree.constructor_declaration) -> *)
  (*            symbols := (cd.pcd_name.txt, cd.pcd_loc, EnumMember) :: !symbols) *)
  (*   | Ptype_record labelDecls -> *)
  (*     labelDecls *)
  (*     |> List.iter (fun (ld : Parsetree.label_declaration) -> *)
  (*            symbols := (ld.pld_name.txt, ld.pld_loc, Property) :: !symbols) *)
  (*   | _ -> () *)
  (* in *)
  (* let processTypeDeclaration (td : Parsetree.type_declaration) = *)
  (*   symbols := (td.ptype_name.txt, td.ptype_loc, TypeParameter) :: !symbols; *)
  (*   processTypeKind td.ptype_kind *)
  (* in *)
  let processValueDescription (vd : Parsetree.value_description) =
    symbols := (vd.pval_name.txt, vd.pval_loc, Variable) :: !symbols
  in
  (* let processModuleBinding (mb : Parsetree.module_binding) = *)
  (*   symbols := (mb.pmb_name.txt, mb.pmb_loc, Module) :: !symbols *)
  (* in *)
  (* let processModuleDeclaration (md : Parsetree.module_declaration) = *)
  (*   symbols := (md.pmd_name.txt, md.pmd_loc, Module) :: !symbols *)
  (* in *)
  (* let processExtensionConstructor (et : Parsetree.extension_constructor) = *)
  (*   symbols := (et.pext_name.txt, et.pext_loc, Constructor) :: !symbols *)
  (* in *)
  let value_binding (iterator : Ast_iterator.iterator)
      (vb : Parsetree.value_binding) =
    (match vb.pvb_pat.ppat_desc with
    | Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _) ->
      symbols := (txt, vb.pvb_loc, exprKind vb.pvb_expr) :: !symbols
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator vb
  in
  (* let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) = *)
  (*   (match e.pexp_desc with *)
  (*   | Pexp_letmodule ({txt}, modExpr, _) -> *)
  (*     symbols := *)
  (*       (txt, {e.pexp_loc with loc_end = modExpr.pmod_loc.loc_end}, Module) *)
  (*       :: !symbols *)
  (*   (* | Pexp_letexception (ec, _) -> processExtensionConstructor ec *) *)
  (*   | _ -> ()); *)
  (*   Ast_iterator.default_iterator.expr iterator e *)
  (* in *)
  let structure_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.structure_item) =
    (match item.pstr_desc with
    | Pstr_value _ -> ()
    | Pstr_primitive vd -> processValueDescription vd
    (* | Pstr_type (_, typDecls) -> typDecls |> List.iter processTypeDeclaration *)
    (* | Pstr_module mb -> processModuleBinding mb *)
    (* | Pstr_recmodule mbs -> mbs |> List.iter processModuleBinding *)
    (* | Pstr_exception ec -> processExtensionConstructor ec *)
    | _ -> ());
    Ast_iterator.default_iterator.structure_item iterator item
  in
  let signature_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.signature_item) =
    (match item.psig_desc with
    | Psig_value vd -> processValueDescription vd
    (* | Psig_type (_, typDecls) -> typDecls |> List.iter processTypeDeclaration *)
    (* | Psig_module md -> processModuleDeclaration md *)
    (* | Psig_recmodule mds -> mds |> List.iter processModuleDeclaration *)
    (* | Psig_exception ec -> processExtensionConstructor ec *)
    | _ -> ());
    Ast_iterator.default_iterator.signature_item iterator item
  in
  (* let module_expr (iterator : Ast_iterator.iterator) *)
  (*     (me : Parsetree.module_expr) = *)
  (*   match me.pmod_desc with *)
  (*   | Pmod_constraint (modExpr, _modTyp) -> *)
  (*     (* Don't double-list items in implementation and interface *) *)
  (*     Ast_iterator.default_iterator.module_expr iterator modExpr *)
  (*   | _ -> Ast_iterator.default_iterator.module_expr iterator me *)
  (* in *)
  let iterator =
    {
      Ast_iterator.default_iterator with
      (* expr; *)
      (* module_expr; *)
      (* signature_item; *)
      (* structure_item; *)
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
  !symbols
  |> List.rev_map (fun (name, loc, kind) ->
         let range = Utils.cmtLocToRange loc in
         let character_end_pos =
           4 + range.start.character + String.length name
         in
         let label =
           match Cmt.fullFromPath ~path with
           | None -> Protocol.null
           | Some full -> (
             match
               References.getLocItem ~full
                 ~pos:(range.start.line, character_end_pos)
                 ~debug:true
             with
             | None -> "refereces not found"
             | Some s -> (
               match locItemToTypeHint ~full s with
               | Some hint -> hint
               | None -> "TypeHint not found"))
         in
         Protocol.stringifyHint
           {
             kind = hintkindNumber kind;
             (* label is type *)
             (* label = name; *)
             tooltip = {
               kind = "markdown";
               value = Hover.codeBlock label;
             };
             paddingLeft = false;
             paddingRight = false;
             label = ": " ^ label;
             
             position =
               {
                 line = range.start.line;
                 (* From col 0 to last character
                    let name = "lol"
                           ^
                 *)
                 character =
                   character_end_pos
                   (* character = pos.start.character; *)
                   (* line = 0; *)
                   (* character = 0; *);
               };
           })
