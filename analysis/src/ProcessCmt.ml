open Typedtree
open SharedTypes

let addItem ~(name : string Location.loc) ~extent ~stamp ~(env : Env.t) ~item
    attributes addExported addStamp =
  let isExported = addExported name.txt stamp in
  let declared =
    ProcessAttributes.newDeclared ~item ~extent ~name ~stamp
      ~modulePath:env.modulePath isExported attributes
  in
  addStamp env.stamps stamp declared;
  declared

let rec forTypeSignatureItem ~env ~(exported : Exported.t)
    (item : Types.signature_item) =
  match item with
  | Sig_value (ident, {val_type; val_attributes; val_loc = loc}) ->
    let item = val_type in
    let declared =
      addItem
        ~name:(Location.mknoloc (Ident.name ident))
        ~extent:loc ~stamp:(Ident.binding_time ident) ~env ~item val_attributes
        (Exported.add exported Exported.Value)
        Stamps.addValue
    in
    [{Module.kind = Module.Value declared.item; name = declared.name.txt}]
  | Sig_type
      ( ident,
        ({type_loc; type_kind; type_manifest; type_attributes} as decl),
        recStatus ) ->
    let declared =
      let name = Location.mknoloc (Ident.name ident) in
      addItem ~extent:type_loc
        ~item:
          {
            Type.decl;
            kind =
              (match type_kind with
              | Type_abstract -> (
                match type_manifest with
                | Some {desc = Tconstr (path, args, _)} ->
                  Abstract (Some (path, args))
                | Some {desc = Ttuple items} -> Tuple items
                (* TODO dig *)
                | _ -> Abstract None)
              | Type_open -> Open
              | Type_variant constructors ->
                Variant
                  (constructors
                  |> List.map
                       (fun
                         {Types.cd_loc; cd_id; cd_args; cd_res; cd_attributes}
                       ->
                         let name = Ident.name cd_id in
                         let stamp = Ident.binding_time cd_id in
                         let item =
                           {
                             Constructor.stamp;
                             cname = Location.mknoloc name;
                             args =
                               (match cd_args with
                               | Cstr_tuple args ->
                                 args |> List.map (fun t -> (t, Location.none))
                               (* TODO(406): constructor record args support *)
                               | Cstr_record _ -> []);
                             res = cd_res;
                             typeDecl = (name, decl);
                           }
                         in
                         let declared =
                           ProcessAttributes.newDeclared ~item ~extent:cd_loc
                             ~name:(Location.mknoloc name)
                             ~stamp (* TODO maybe this needs another child *)
                             ~modulePath:env.modulePath true cd_attributes
                         in
                         Stamps.addConstructor env.stamps stamp declared;
                         item))
              | Type_record (fields, _) ->
                Record
                  (fields
                  |> List.map (fun {Types.ld_id; ld_type} ->
                         let astamp = Ident.binding_time ld_id in
                         let name = Ident.name ld_id in
                         {
                           stamp = astamp;
                           fname = Location.mknoloc name;
                           typ = ld_type;
                         })));
          }
        ~name ~stamp:(Ident.binding_time ident) ~env type_attributes
        (Exported.add exported Exported.Type)
        Stamps.addType
    in
    [{Module.kind = Type (declared.item, recStatus); name = declared.name.txt}]
  | Sig_module (ident, {md_type; md_attributes; md_loc}, _) ->
    let declared =
      addItem ~extent:md_loc
        ~item:(forTypeModule env md_type)
        ~name:(Location.mknoloc (Ident.name ident))
        ~stamp:(Ident.binding_time ident) ~env md_attributes
        (Exported.add exported Exported.Module)
        Stamps.addModule
    in
    [{Module.kind = Module declared.item; name = declared.name.txt}]
  | _ -> []

and forTypeSignature env signature =
  let exported = Exported.init () in
  let items =
    List.fold_right
      (fun item items -> forTypeSignatureItem ~env ~exported item @ items)
      signature []
  in
  {Module.docstring = []; exported; items}

and forTypeModule env moduleType =
  match moduleType with
  | Types.Mty_ident path -> Ident path
  | Mty_alias (_ (* 402 *), path) -> Ident path
  | Mty_signature signature -> Structure (forTypeSignature env signature)
  | Mty_functor (_argIdent, _argType, resultType) ->
    forTypeModule env resultType

let getModuleTypePath mod_desc =
  match mod_desc with
  | Tmty_ident (path, _) | Tmty_alias (path, _) -> Some path
  | Tmty_signature _ | Tmty_functor _ | Tmty_with _ | Tmty_typeof _ -> None

let forTypeDeclaration ~env ~(exported : Exported.t)
    {
      typ_id;
      typ_loc;
      typ_name = name;
      typ_attributes;
      typ_type;
      typ_kind;
      typ_manifest;
    } ~recStatus =
  let stamp = Ident.binding_time typ_id in
  let declared =
    addItem ~extent:typ_loc
      ~item:
        {
          Type.decl = typ_type;
          kind =
            (match typ_kind with
            | Ttype_abstract -> (
              match typ_manifest with
              | Some {ctyp_desc = Ttyp_constr (path, _lident, args)} ->
                Abstract (Some (path, args |> List.map (fun t -> t.ctyp_type)))
              | Some {ctyp_desc = Ttyp_tuple items} ->
                Tuple (items |> List.map (fun t -> t.ctyp_type))
              (* TODO dig *)
              | _ -> Abstract None)
            | Ttype_open -> Open
            | Ttype_variant constructors ->
              Variant
                (constructors
                |> List.map
                     (fun
                       {
                         cd_id;
                         cd_name = cname;
                         cd_args;
                         cd_res;
                         cd_attributes;
                         cd_loc;
                       }
                     ->
                       let stamp = Ident.binding_time cd_id in
                       let item =
                         {
                           Constructor.stamp;
                           cname;
                           args =
                             (match cd_args with
                             | Cstr_tuple args ->
                               args
                               |> List.map (fun t -> (t.ctyp_type, t.ctyp_loc))
                             (* TODO(406) *)
                             | Cstr_record _ -> []);
                           res =
                             (match cd_res with
                             | None -> None
                             | Some t -> Some t.ctyp_type);
                           typeDecl = (name.txt, typ_type);
                         }
                       in
                       let declared =
                         ProcessAttributes.newDeclared ~item ~extent:cd_loc
                           ~name:cname ~stamp ~modulePath:env.modulePath true
                           cd_attributes
                       in
                       Stamps.addConstructor env.stamps stamp declared;
                       item))
            | Ttype_record fields ->
              Record
                (fields
                |> List.map
                     (fun {ld_id; ld_name = fname; ld_type = {ctyp_type}} ->
                       let fstamp = Ident.binding_time ld_id in
                       {stamp = fstamp; fname; typ = ctyp_type})));
        }
      ~name ~stamp ~env typ_attributes
      (Exported.add exported Exported.Type)
      Stamps.addType
  in
  {
    Module.kind = Module.Type (declared.item, recStatus);
    name = declared.name.txt;
  }

let rec forSignatureItem ~env ~(exported : Exported.t)
    (item : Typedtree.signature_item) =
  match item.sig_desc with
  | Tsig_value {val_id; val_loc; val_name = name; val_desc; val_attributes} ->
    let declared =
      addItem ~name
        ~stamp:(Ident.binding_time val_id)
        ~extent:val_loc ~item:val_desc.ctyp_type ~env val_attributes
        (Exported.add exported Exported.Value)
        Stamps.addValue
    in
    [{Module.kind = Module.Value declared.item; name = declared.name.txt}]
  | Tsig_type (recFlag, decls) ->
    decls
    |> List.mapi (fun i decl ->
           let recStatus =
             match recFlag with
             | Recursive when i = 0 -> Types.Trec_first
             | Nonrecursive when i = 0 -> Types.Trec_not
             | _ -> Types.Trec_next
           in
           decl |> forTypeDeclaration ~env ~exported ~recStatus)
  | Tsig_module
      {md_id; md_attributes; md_loc; md_name = name; md_type = {mty_type}} ->
    let item = forTypeModule env mty_type in
    let declared =
      addItem ~item ~name ~extent:md_loc ~stamp:(Ident.binding_time md_id) ~env
        md_attributes
        (Exported.add exported Exported.Module)
        Stamps.addModule
    in
    [{Module.kind = Module declared.item; name = declared.name.txt}]
  | Tsig_recmodule modDecls ->
    modDecls
    |> List.map (fun modDecl ->
           forSignatureItem ~env ~exported
             {item with sig_desc = Tsig_module modDecl})
    |> List.flatten
  | Tsig_include {incl_mod; incl_type} ->
    let env =
      match getModuleTypePath incl_mod.mty_desc with
      | None -> env
      | Some path ->
        {env with modulePath = IncludedModule (path, env.modulePath)}
    in
    let topLevel =
      List.fold_right
        (fun item items -> forTypeSignatureItem ~env ~exported item @ items)
        incl_type []
    in
    topLevel
  (* TODO: process other things here *)
  | _ -> []

let forSignature ~env sigItems =
  let exported = Exported.init () in
  let items =
    sigItems |> List.map (forSignatureItem ~env ~exported) |> List.flatten
  in
  let attributes =
    match sigItems with
    | {sig_desc = Tsig_attribute attribute} :: _ -> [attribute]
    | _ -> []
  in
  let docstring =
    match ProcessAttributes.findDocAttribute attributes with
    | None -> []
    | Some d -> [d]
  in
  {Module.docstring; exported; items}

let forTreeModuleType ~env {mty_desc} =
  match mty_desc with
  | Tmty_ident _ -> None
  | Tmty_signature {sig_items} ->
    let contents = forSignature ~env sig_items in
    Some (Module.Structure contents)
  | _ -> None

let rec getModulePath mod_desc =
  match mod_desc with
  | Tmod_ident (path, _lident) -> Some path
  | Tmod_structure _ -> None
  | Tmod_functor (_ident, _argName, _maybeType, _resultExpr) -> None
  | Tmod_apply (functor_, _arg, _coercion) -> getModulePath functor_.mod_desc
  | Tmod_unpack (_expr, _moduleType) -> None
  | Tmod_constraint (expr, _typ, _constraint, _coercion) ->
    getModulePath expr.mod_desc

let rec forStructureItem ~env ~(exported : Exported.t) item =
  match item.str_desc with
  | Tstr_value (_isRec, bindings) ->
    let items = ref [] in
    let rec handlePattern attributes pat =
      match pat.pat_desc with
      | Tpat_var (ident, name)
      | Tpat_alias (_, ident, name) (* let x : t = ... *) ->
        let item = pat.pat_type in
        let declared =
          addItem ~name ~stamp:(Ident.binding_time ident) ~env
            ~extent:pat.pat_loc ~item attributes
            (Exported.add exported Exported.Value)
            Stamps.addValue
        in
        items :=
          {Module.kind = Module.Value declared.item; name = declared.name.txt}
          :: !items
      | Tpat_tuple pats | Tpat_array pats | Tpat_construct (_, _, pats) ->
        pats |> List.iter (fun p -> handlePattern [] p)
      | Tpat_or (p, _, _) -> handlePattern [] p
      | Tpat_record (items, _) ->
        items |> List.iter (fun (_, _, p) -> handlePattern [] p)
      | Tpat_lazy p -> handlePattern [] p
      | Tpat_variant (_, Some p, _) -> handlePattern [] p
      | Tpat_variant (_, None, _) | Tpat_any | Tpat_constant _ -> ()
    in
    List.iter
      (fun {vb_pat; vb_attributes} -> handlePattern vb_attributes vb_pat)
      bindings;
    !items
  | Tstr_module
      {mb_id; mb_attributes; mb_loc; mb_name = name; mb_expr = {mod_desc}}
    when not (String.length name.txt >= 6 && String.sub name.txt 0 6 = "local_")
         (* %%private generates a dummy module called local_... *) ->
    let item = forModule env mod_desc name.txt in
    let declared =
      addItem ~item ~name ~extent:mb_loc ~stamp:(Ident.binding_time mb_id) ~env
        mb_attributes
        (Exported.add exported Exported.Module)
        Stamps.addModule
    in
    [{Module.kind = Module declared.item; name = declared.name.txt}]
  | Tstr_recmodule modDecls ->
    modDecls
    |> List.map (fun modDecl ->
           forStructureItem ~env ~exported
             {item with str_desc = Tstr_module modDecl})
    |> List.flatten
  | Tstr_modtype
      {
        mtd_name = name;
        mtd_id;
        mtd_attributes;
        mtd_type = Some {mty_type = modType};
        mtd_loc;
      } ->
    let env =
      {env with modulePath = ExportedModule (name.txt, env.modulePath)}
    in
    let modTypeItem = forTypeModule env modType in
    let declared =
      addItem ~item:modTypeItem ~name ~extent:mtd_loc
        ~stamp:(Ident.binding_time mtd_id)
        ~env mtd_attributes
        (Exported.add exported Exported.Module)
        Stamps.addModule
    in
    [{Module.kind = Module modTypeItem; name = declared.name.txt}]
  | Tstr_include {incl_mod; incl_type} ->
    let env =
      match getModulePath incl_mod.mod_desc with
      | None -> env
      | Some path ->
        {env with modulePath = IncludedModule (path, env.modulePath)}
    in
    let topLevel =
      List.fold_right
        (fun item items -> forTypeSignatureItem ~env ~exported item @ items)
        incl_type []
    in
    topLevel
  | Tstr_primitive
      {val_id; val_name = name; val_loc; val_attributes; val_val = {val_type}}
    ->
    let declared =
      addItem ~extent:val_loc ~item:val_type ~name
        ~stamp:(Ident.binding_time val_id)
        ~env val_attributes
        (Exported.add exported Exported.Value)
        Stamps.addValue
    in
    [{Module.kind = Value declared.item; name = declared.name.txt}]
  | Tstr_type (recFlag, decls) ->
    decls
    |> List.mapi (fun i decl ->
           let recStatus =
             match recFlag with
             | Recursive when i = 0 -> Types.Trec_first
             | Nonrecursive when i = 0 -> Types.Trec_not
             | _ -> Types.Trec_next
           in
           decl |> forTypeDeclaration ~env ~exported ~recStatus)
  | _ -> []

and forModule env mod_desc moduleName =
  match mod_desc with
  | Tmod_ident (path, _lident) -> Ident path
  | Tmod_structure structure ->
    let env =
      {env with modulePath = ExportedModule (moduleName, env.modulePath)}
    in
    let contents = forStructure ~env structure.str_items in
    Structure contents
  | Tmod_functor (ident, argName, maybeType, resultExpr) ->
    (match maybeType with
    | None -> ()
    | Some t -> (
      match forTreeModuleType ~env t with
      | None -> ()
      | Some kind ->
        let stamp = Ident.binding_time ident in
        let declared =
          ProcessAttributes.newDeclared ~item:kind ~name:argName
            ~extent:t.Typedtree.mty_loc ~stamp ~modulePath:NotVisible false []
        in
        Stamps.addModule env.stamps stamp declared));
    forModule env resultExpr.mod_desc moduleName
  | Tmod_apply (functor_, _arg, _coercion) ->
    forModule env functor_.mod_desc moduleName
  | Tmod_unpack (_expr, moduleType) ->
    let env =
      {env with modulePath = ExportedModule (moduleName, env.modulePath)}
    in
    forTypeModule env moduleType
  | Tmod_constraint (expr, typ, _constraint, _coercion) ->
    (* TODO do this better I think *)
    let modKind = forModule env expr.mod_desc moduleName in
    let env =
      {env with modulePath = ExportedModule (moduleName, env.modulePath)}
    in
    let modTypeKind = forTypeModule env typ in
    Constraint (modKind, modTypeKind)

and forStructure ~env strItems =
  let exported = Exported.init () in
  let items =
    List.fold_right
      (fun item results -> forStructureItem ~env ~exported item @ results)
      strItems []
  in
  let attributes =
    match strItems with
    | {str_desc = Tstr_attribute attribute} :: _ -> [attribute]
    | _ -> []
  in
  let docstring =
    match ProcessAttributes.findDocAttribute attributes with
    | None -> []
    | Some d -> [d]
  in
  {docstring; exported; items}

let forCmt ~moduleName ~uri ({cmt_modname; cmt_annots} : Cmt_format.cmt_infos) =
  match cmt_annots with
  | Partial_implementation parts ->
    let items =
      parts |> Array.to_list
      |> Utils.filterMap (fun p ->
             match (p : Cmt_format.binary_part) with
             | Partial_structure str -> Some str.str_items
             | Partial_structure_item str -> Some [str]
             | _ -> None)
      |> List.concat
    in
    let env =
      {Env.stamps = Stamps.init (); modulePath = File (uri, moduleName)}
    in
    let structure = forStructure ~env items in
    {File.uri; moduleName = cmt_modname; stamps = env.stamps; structure}
  | Partial_interface parts ->
    let items =
      parts |> Array.to_list
      |> Utils.filterMap (fun (p : Cmt_format.binary_part) ->
             match p with
             | Partial_signature str -> Some str.sig_items
             | Partial_signature_item str -> Some [str]
             | _ -> None)
      |> List.concat
    in
    let env =
      {Env.stamps = Stamps.init (); modulePath = File (uri, moduleName)}
    in
    let structure = forSignature ~env items in
    {uri; moduleName = cmt_modname; stamps = env.stamps; structure}
  | Implementation structure ->
    let env =
      {Env.stamps = Stamps.init (); modulePath = File (uri, moduleName)}
    in
    let structure = forStructure ~env structure.str_items in
    {uri; moduleName = cmt_modname; stamps = env.stamps; structure}
  | Interface signature ->
    let env =
      {Env.stamps = Stamps.init (); modulePath = File (uri, moduleName)}
    in
    let structure = forSignature ~env signature.sig_items in
    {uri; moduleName = cmt_modname; stamps = env.stamps; structure}
  | _ -> File.create moduleName uri

let fileForCmt ~moduleName ~uri cmt =
  match Shared.tryReadCmt cmt with
  | None -> None
  | Some infos -> Some (forCmt ~moduleName ~uri infos)

let addLocItem extra loc locType =
  if not loc.Warnings.loc_ghost then
    extra.locItems <- {loc; locType} :: extra.locItems

let addReference ~extra stamp loc =
  Hashtbl.replace extra.internalReferences stamp
    (loc
    ::
    (if Hashtbl.mem extra.internalReferences stamp then
     Hashtbl.find extra.internalReferences stamp
    else []))

let extraForFile ~(file : File.t) =
  let extra = initExtra () in
  file.stamps
  |> Stamps.iterModules (fun stamp (d : Module.t Declared.t) ->
         addLocItem extra d.name.loc (LModule (Definition (stamp, Module)));
         addReference ~extra stamp d.name.loc);
  file.stamps
  |> Stamps.iterValues (fun stamp (d : Types.type_expr Declared.t) ->
         addLocItem extra d.name.loc
           (Typed (d.name.txt, d.item, Definition (stamp, Value)));
         addReference ~extra stamp d.name.loc);
  file.stamps
  |> Stamps.iterTypes (fun stamp (d : Type.t Declared.t) ->
         addLocItem extra d.name.loc
           (TypeDefinition (d.name.txt, d.item.Type.decl, stamp));
         addReference ~extra stamp d.name.loc;
         match d.item.Type.kind with
         | Record labels ->
           labels
           |> List.iter (fun {stamp; fname; typ} ->
                  addReference ~extra stamp fname.loc;
                  addLocItem extra fname.loc
                    (Typed
                       (d.name.txt, typ, Definition (d.stamp, Field fname.txt))))
         | Variant constructors ->
           constructors
           |> List.iter (fun {Constructor.stamp; cname} ->
                  addReference ~extra stamp cname.loc;
                  let t =
                    {
                      Types.id = 0;
                      level = 0;
                      desc =
                        Tconstr
                          ( Path.Pident
                              {Ident.stamp; name = d.name.txt; flags = 0},
                            [],
                            ref Types.Mnil );
                    }
                  in
                  addLocItem extra cname.loc
                    (Typed
                       ( d.name.txt,
                         t,
                         Definition (d.stamp, Constructor cname.txt) )))
         | _ -> ());
  extra

let rec joinPaths modulePath path =
  match modulePath with
  | Path.Pident ident -> (ident.stamp, ident.name, path)
  | Papply (fnPath, _argPath) -> joinPaths fnPath path
  | Pdot (inner, name, _) -> joinPaths inner (name :: path)

let rec makePath modulePath =
  match modulePath with
  | Path.Pident ident when ident.stamp == 0 -> `GlobalMod ident.name
  | Pident ident -> `Stamp ident.stamp
  | Papply (fnPath, _argPath) -> makePath fnPath
  | Pdot (inner, name, _) -> `Path (joinPaths inner [name])

let rec resolvePathInner ~(env : QueryEnv.t) ~path =
  match path with
  | [] -> None
  | [name] -> Some (`Local (env, name))
  | subName :: subPath -> (
    match Exported.find env.exported Exported.Module subName with
    | None -> None
    | Some stamp -> (
      match Stamps.findModule env.file.stamps stamp with
      | None -> None
      | Some {item} -> findInModule ~env item subPath))

and findInModule ~env module_ path =
  match module_ with
  | Structure {exported} -> resolvePathInner ~env:{env with exported} ~path
  | Constraint (_, module1) -> findInModule ~env module1 path
  | Ident modulePath -> (
    let stamp, moduleName, fullPath = joinPaths modulePath path in
    if stamp = 0 then Some (`Global (moduleName, fullPath))
    else
      match Stamps.findModule env.file.stamps stamp with
      | None -> None
      | Some {item} -> findInModule ~env item fullPath)

let fromCompilerPath ~(env : QueryEnv.t) path =
  match makePath path with
  | `Stamp stamp -> `Stamp stamp
  | `Path (0, moduleName, path) -> `Global (moduleName, path)
  | `GlobalMod name -> `GlobalMod name
  | `Path (stamp, _moduleName, path) -> (
    let res =
      match Stamps.findModule env.file.stamps stamp with
      | None -> None
      | Some {item = kind} -> findInModule ~env kind path
    in
    match res with
    | None -> `Not_found
    | Some (`Local (env, name)) -> `Exported (env, name)
    | Some (`Global (moduleName, fullPath)) -> `Global (moduleName, fullPath))

let addExternalReference ~extra moduleName path tip loc =
  (* TODO need to follow the path, and be able to load the files to follow module references... *)
  Hashtbl.replace extra.externalReferences moduleName
    ((path, tip, loc)
    ::
    (if Hashtbl.mem extra.externalReferences moduleName then
     Hashtbl.find extra.externalReferences moduleName
    else []))

let addFileReference ~extra moduleName loc =
  let newLocs =
    match Hashtbl.find_opt extra.fileReferences moduleName with
    | Some oldLocs -> LocationSet.add loc oldLocs
    | None -> LocationSet.singleton loc
  in
  Hashtbl.replace extra.fileReferences moduleName newLocs

let addForPath ~env ~extra path lident loc typ tip =
  let identName = Longident.last lident in
  let identLoc = Utils.endOfLocation loc (String.length identName) in
  let locType =
    match fromCompilerPath ~env path with
    | `Stamp stamp ->
      addReference ~extra stamp identLoc;
      LocalReference (stamp, tip)
    | `Not_found -> NotFound
    | `Global (moduleName, path) ->
      addExternalReference ~extra moduleName path tip identLoc;
      GlobalReference (moduleName, path, tip)
    | `Exported (env, name) -> (
      match
        match tip with
        | Type -> Exported.find env.exported Exported.Type name
        | _ -> Exported.find env.exported Exported.Value name
      with
      | Some stamp ->
        addReference ~extra stamp identLoc;
        LocalReference (stamp, tip)
      | None -> NotFound)
    | `GlobalMod _ -> NotFound
  in
  addLocItem extra loc (Typed (identName, typ, locType))

let addForPathParent ~env ~extra path loc =
  let locType =
    match fromCompilerPath ~env path with
    | `GlobalMod moduleName ->
      addFileReference ~extra moduleName loc;
      TopLevelModule moduleName
    | `Stamp stamp ->
      addReference ~extra stamp loc;
      LModule (LocalReference (stamp, Module))
    | `Not_found -> LModule NotFound
    | `Global (moduleName, path) ->
      addExternalReference ~extra moduleName path Module loc;
      LModule (GlobalReference (moduleName, path, Module))
    | `Exported (env, name) -> (
      match Exported.find env.exported Exported.Module name with
      | Some stamp ->
        addReference ~extra stamp loc;
        LModule (LocalReference (stamp, Module))
      | None -> LModule NotFound)
  in
  addLocItem extra loc locType

let getTypeAtPath ~env path =
  match fromCompilerPath ~env path with
  | `GlobalMod _ -> `Not_found
  | `Global (moduleName, path) -> `Global (moduleName, path)
  | `Not_found -> `Not_found
  | `Exported (env, name) -> (
    match Exported.find env.exported Exported.Type name with
    | None -> `Not_found
    | Some stamp -> (
      let declaredType = Stamps.findType env.file.stamps stamp in
      match declaredType with
      | Some declaredType -> `Local declaredType
      | None -> `Not_found))
  | `Stamp stamp -> (
    let declaredType = Stamps.findType env.file.stamps stamp in
    match declaredType with
    | Some declaredType -> `Local declaredType
    | None -> `Not_found)

let handleConstructor txt =
  match txt with
  | Longident.Lident name -> name
  | Ldot (_left, name) -> name
  | Lapply (_, _) -> assert false

let addForField ~env ~extra recordType fieldType {Asttypes.txt; loc} =
  match (Shared.dig recordType).desc with
  | Tconstr (path, _args, _memo) ->
    let t = getTypeAtPath ~env path in
    let name = handleConstructor txt in
    let nameLoc = Utils.endOfLocation loc (String.length name) in
    let locType =
      match t with
      | `Local {stamp; item = {kind = Record fields}} -> (
        match fields |> List.find_opt (fun f -> f.fname.txt = name) with
        | Some {stamp = astamp} ->
          addReference ~extra astamp nameLoc;
          LocalReference (stamp, Field name)
        | None -> NotFound)
      | `Global (moduleName, path) ->
        addExternalReference ~extra moduleName path (Field name) nameLoc;
        GlobalReference (moduleName, path, Field name)
      | _ -> NotFound
    in
    addLocItem extra nameLoc (Typed (name, fieldType, locType))
  | _ -> ()

let addForRecord ~env ~extra recordType items =
  match (Shared.dig recordType).desc with
  | Tconstr (path, _args, _memo) ->
    let t = getTypeAtPath ~env path in
    items
    |> List.iter (fun ({Asttypes.txt; loc}, {Types.lbl_res}, _) ->
           (* let name = Longident.last(txt); *)
           let name = handleConstructor txt in
           let nameLoc = Utils.endOfLocation loc (String.length name) in
           let locType =
             match t with
             | `Local {stamp; item = {kind = Record fields}} -> (
               match fields |> List.find_opt (fun f -> f.fname.txt = name) with
               | Some {stamp = astamp} ->
                 addReference ~extra astamp nameLoc;
                 LocalReference (stamp, Field name)
               | None -> NotFound)
             | `Global (moduleName, path) ->
               addExternalReference ~extra moduleName path (Field name) nameLoc;
               GlobalReference (moduleName, path, Field name)
             | _ -> NotFound
           in
           addLocItem extra nameLoc (Typed (name, lbl_res, locType)))
  | _ -> ()

let addForConstructor ~env ~extra constructorType {Asttypes.txt; loc}
    {Types.cstr_name} =
  match (Shared.dig constructorType).desc with
  | Tconstr (path, _args, _memo) ->
    let name = handleConstructor txt in
    let nameLoc = Utils.endOfLocation loc (String.length name) in
    let t = getTypeAtPath ~env path in
    let locType =
      match t with
      | `Local {stamp; item = {kind = Variant constructors}} -> (
        match
          constructors
          |> List.find_opt (fun c -> c.Constructor.cname.txt = cstr_name)
        with
        | Some {stamp = cstamp} ->
          addReference ~extra cstamp nameLoc;
          LocalReference (stamp, Constructor name)
        | None -> NotFound)
      | `Global (moduleName, path) ->
        addExternalReference ~extra moduleName path (Constructor name) nameLoc;
        GlobalReference (moduleName, path, Constructor name)
      | _ -> NotFound
    in
    addLocItem extra nameLoc (Typed (name, constructorType, locType))
  | _ -> ()

let rec lidIsComplex (lid : Longident.t) =
  match lid with
  | Lapply _ -> true
  | Ldot (lid, _) -> lidIsComplex lid
  | _ -> false

let rec addForLongident ~env ~extra top (path : Path.t) (txt : Longident.t) loc
    =
  if (not loc.Location.loc_ghost) && not (lidIsComplex txt) then (
    let idLength = String.length (String.concat "." (Longident.flatten txt)) in
    let reportedLength = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
    let isPpx = idLength <> reportedLength in
    if isPpx then
      match top with
      | Some (t, tip) -> addForPath ~env ~extra path txt loc t tip
      | None -> addForPathParent ~env ~extra path loc
    else
      let l = Utils.endOfLocation loc (String.length (Longident.last txt)) in
      (match top with
      | Some (t, tip) -> addForPath ~env ~extra path txt l t tip
      | None -> addForPathParent ~env ~extra path l);
      match (path, txt) with
      | Pdot (pinner, _pname, _), Ldot (inner, name) ->
        addForLongident ~env ~extra None pinner inner
          (Utils.chopLocationEnd loc (String.length name + 1))
      | Pident _, Lident _ -> ()
      | _ -> ())

let rec handle_module_expr ~env ~extra expr =
  match expr with
  | Tmod_constraint (expr, _, _, _) ->
    handle_module_expr ~env ~extra expr.mod_desc
  | Tmod_ident (path, {txt; loc}) ->
    if not (lidIsComplex txt) then
      Log.log ("Ident!! " ^ String.concat "." (Longident.flatten txt));
    addForLongident ~env ~extra None path txt loc
  | Tmod_functor (_ident, _argName, _maybeType, resultExpr) ->
    handle_module_expr ~env ~extra resultExpr.mod_desc
  | Tmod_apply (obj, arg, _) ->
    handle_module_expr ~env ~extra obj.mod_desc;
    handle_module_expr ~env ~extra arg.mod_desc
  | _ -> ()

let structure_item ~env ~extra (iter : Tast_iterator.iterator) item =
  (match item.str_desc with
  | Tstr_include {incl_mod = expr} ->
    handle_module_expr ~env ~extra expr.mod_desc
  | Tstr_module {mb_expr} -> handle_module_expr ~env ~extra mb_expr.mod_desc
  | Tstr_open {open_path; open_txt = {txt; loc}} ->
    (* Log.log("Have an open here"); *)
    addForLongident ~env ~extra None open_path txt loc;
    Hashtbl.replace extra.opens loc ()
  | _ -> ());
  Tast_iterator.default_iterator.structure_item iter item

let signature_item ~(file : File.t) ~extra (iter : Tast_iterator.iterator) item
    =
  (match item.sig_desc with
  | Tsig_value {val_id; val_loc; val_name = name; val_desc; val_attributes} ->
    let stamp = Ident.binding_time val_id in
    if Stamps.findValue file.stamps stamp = None then (
      let declared =
        ProcessAttributes.newDeclared ~name ~stamp ~extent:val_loc
          ~modulePath:NotVisible ~item:val_desc.ctyp_type false val_attributes
      in
      Stamps.addValue file.stamps stamp declared;
      addReference ~extra stamp name.loc;
      addLocItem extra name.loc
        (Typed (name.txt, val_desc.ctyp_type, Definition (stamp, Value))))
  | _ -> ());
  Tast_iterator.default_iterator.signature_item iter item

let typ ~env ~extra (iter : Tast_iterator.iterator) (item : Typedtree.core_type)
    =
  (match item.ctyp_desc with
  | Ttyp_constr (path, {txt; loc}, _args) ->
    addForLongident ~env ~extra (Some (item.ctyp_type, Type)) path txt loc
  | _ -> ());
  Tast_iterator.default_iterator.typ iter item

let pat ~(file : File.t) ~env ~extra (iter : Tast_iterator.iterator)
    (pattern : Typedtree.pattern) =
  let addForPattern stamp name =
    if Stamps.findValue file.stamps stamp = None then (
      let declared =
        ProcessAttributes.newDeclared ~name ~stamp ~modulePath:NotVisible
          ~extent:pattern.pat_loc ~item:pattern.pat_type false
          pattern.pat_attributes
      in
      Stamps.addValue file.stamps stamp declared;
      addReference ~extra stamp name.loc;
      addLocItem extra name.loc
        (Typed (name.txt, pattern.pat_type, Definition (stamp, Value))))
  in
  (* Log.log("Entering pattern " ++ Utils.showLocation(pat_loc)); *)
  (match pattern.pat_desc with
  | Tpat_record (items, _) -> addForRecord ~env ~extra pattern.pat_type items
  | Tpat_construct (lident, constructor, _) ->
    addForConstructor ~env ~extra pattern.pat_type lident constructor
  | Tpat_alias (_inner, ident, name) ->
    let stamp = Ident.binding_time ident in
    addForPattern stamp name
  | Tpat_var (ident, name) ->
    (* Log.log("Pattern " ++ name.txt); *)
    let stamp = Ident.binding_time ident in
    addForPattern stamp name
  | _ -> ());
  Tast_iterator.default_iterator.pat iter pattern

let expr ~env ~(extra : extra) (iter : Tast_iterator.iterator)
    (expression : Typedtree.expression) =
  (expression.exp_extra
   |> List.iter (fun (e, eloc, _) ->
          match e with
          | Texp_open (_, _path, _ident, _) -> Hashtbl.add extra.opens eloc ()
          | _ -> ());
   match expression.exp_desc with
   | Texp_ident (path, {txt; loc}, _) ->
     addForLongident ~env ~extra
       (Some (expression.exp_type, Value))
       path txt loc
   | Texp_record {fields} ->
     addForRecord ~env ~extra expression.exp_type
       (fields |> Array.to_list
       |> Utils.filterMap (fun (desc, item) ->
              match item with
              | Overridden (loc, _) -> Some (loc, desc, ())
              | _ -> None))
   | Texp_constant constant ->
     addLocItem extra expression.exp_loc (Constant constant)
   (* Skip unit and list literals *)
   | Texp_construct ({txt = Lident ("()" | "::"); loc}, _, _args)
     when loc.loc_end.pos_cnum - loc.loc_start.pos_cnum <> 2 ->
     ()
   | Texp_construct (lident, constructor, _args) ->
     addForConstructor ~env ~extra expression.exp_type lident constructor
   | Texp_field (inner, lident, _label_description) ->
     addForField ~env ~extra inner.exp_type expression.exp_type lident
   | _ -> ());
  Tast_iterator.default_iterator.expr iter expression

let getIterator ~env ~extra ~file =
  {
    Tast_iterator.default_iterator with
    expr = expr ~env ~extra;
    pat = pat ~env ~extra ~file;
    signature_item = signature_item ~file ~extra;
    structure_item = structure_item ~env ~extra;
    typ = typ ~env ~extra;
  }

let extraForStructureItems ~(iterator : Tast_iterator.iterator)
    (items : Typedtree.structure_item list) =
  items |> List.iter (iterator.structure_item iterator)

let extraForSignatureItems ~(iterator : Tast_iterator.iterator)
    (items : Typedtree.signature_item list) =
  items |> List.iter (iterator.signature_item iterator)

let extraForCmt ~(iterator : Tast_iterator.iterator)
    ({cmt_annots} : Cmt_format.cmt_infos) =
  let extraForParts parts =
    parts
    |> Array.iter (fun part ->
           match part with
           | Cmt_format.Partial_signature str -> iterator.signature iterator str
           | Partial_signature_item str -> iterator.signature_item iterator str
           | Partial_expression expression -> iterator.expr iterator expression
           | Partial_pattern pattern -> iterator.pat iterator pattern
           | Partial_class_expr () -> ()
           | Partial_module_type module_type ->
             iterator.module_type iterator module_type
           | Partial_structure _ | Partial_structure_item _ -> ())
  in
  match cmt_annots with
  | Implementation structure ->
    extraForStructureItems ~iterator structure.str_items
  | Partial_implementation parts ->
    let items =
      parts |> Array.to_list
      |> Utils.filterMap (fun (p : Cmt_format.binary_part) ->
             match p with
             | Partial_structure str -> Some str.str_items
             | Partial_structure_item str -> Some [str]
             (* | Partial_expression(exp) => Some([ str]) *)
             | _ -> None)
      |> List.concat
    in
    extraForStructureItems ~iterator items;
    extraForParts parts
  | Interface signature -> extraForSignatureItems ~iterator signature.sig_items
  | Partial_interface parts ->
    let items =
      parts |> Array.to_list
      |> Utils.filterMap (fun (p : Cmt_format.binary_part) ->
             match p with
             | Partial_signature s -> Some s.sig_items
             | Partial_signature_item str -> Some [str]
             | _ -> None)
      |> List.concat
    in
    extraForSignatureItems ~iterator items;
    extraForParts parts
  | _ -> extraForStructureItems ~iterator []

let fullForCmt ~moduleName ~package ~uri cmt =
  match Shared.tryReadCmt cmt with
  | None -> None
  | Some infos ->
    let file = forCmt ~moduleName ~uri infos in
    let extra = extraForFile ~file in
    let env = QueryEnv.fromFile file in
    let iterator = getIterator ~env ~extra ~file in
    extraForCmt ~iterator infos;
    Some {file; extra; package}

open SharedTypes

let newFileForCmt ~moduleName cmtCache changed ~cmt ~uri =
  match fileForCmt ~moduleName ~uri cmt with
  | None -> None
  | Some file ->
    Hashtbl.replace cmtCache cmt (changed, file);
    Some file

let fileForCmt ~moduleName ~cmt ~uri state =
  if Hashtbl.mem state.cmtCache cmt then
    let mtime, docs = Hashtbl.find state.cmtCache cmt in
    (* TODO: I should really throttle this mtime checking to like every 50 ms or so *)
    match Files.getMtime cmt with
    | None ->
      Log.log
        ("\226\154\160\239\184\143 cannot get docs for nonexistant cmt " ^ cmt);
      None
    | Some changed ->
      if changed > mtime then
        newFileForCmt ~moduleName state.cmtCache changed ~cmt ~uri
      else Some docs
  else
    match Files.getMtime cmt with
    | None ->
      Log.log
        ("\226\154\160\239\184\143 cannot get docs for nonexistant cmt " ^ cmt);
      None
    | Some changed -> newFileForCmt ~moduleName state.cmtCache changed ~cmt ~uri

let fileForModule modname ~package =
  if Hashtbl.mem package.pathsForModule modname then (
    let paths = Hashtbl.find package.pathsForModule modname in
    (* TODO: do better *)
    let uri = getUri paths in
    let cmt = getCmtPath ~uri paths in
    Log.log ("fileForModule " ^ showPaths paths);
    match fileForCmt ~moduleName:modname ~cmt ~uri state with
    | None -> None
    | Some docs -> Some docs)
  else (
    Log.log ("No path for module " ^ modname);
    None)

let rec resolvePath ~env ~path ~package =
  Log.log ("resolvePath path:" ^ pathToString path);
  match resolvePathInner ~env ~path with
  | None -> None
  | Some result -> (
    match result with
    | `Local (env, name) -> Some (env, name)
    | `Global (moduleName, fullPath) -> (
      Log.log
        ("resolvePath Global path:" ^ pathToString fullPath ^ " module:"
       ^ moduleName);
      match fileForModule ~package moduleName with
      | None -> None
      | Some file ->
        resolvePath ~env:(QueryEnv.fromFile file) ~path:fullPath ~package))

let resolveModuleFromCompilerPath ~env ~package path =
  match fromCompilerPath ~env path with
  | `Global (moduleName, path) -> (
    match fileForModule ~package moduleName with
    | None -> None
    | Some file -> (
      let env = QueryEnv.fromFile file in
      match resolvePath ~env ~package ~path with
      | None -> None
      | Some (env, name) -> (
        match Exported.find env.exported Exported.Module name with
        | None -> None
        | Some stamp -> (
          match Stamps.findModule env.file.stamps stamp with
          | None -> None
          | Some declared -> Some (env, Some declared)))))
  | `Stamp stamp -> (
    match Stamps.findModule env.file.stamps stamp with
    | None -> None
    | Some declared -> Some (env, Some declared))
  | `GlobalMod moduleName -> (
    match fileForModule ~package moduleName with
    | None -> None
    | Some file ->
      let env = QueryEnv.fromFile file in
      Some (env, None))
  | `Not_found -> None
  | `Exported (env, name) -> (
    match Exported.find env.exported Exported.Module name with
    | None -> None
    | Some stamp -> (
      match Stamps.findModule env.file.stamps stamp with
      | None -> None
      | Some declared -> Some (env, Some declared)))

let resolveFromCompilerPath ~env ~package path =
  match fromCompilerPath ~env path with
  | `Global (moduleName, path) -> (
    let res =
      match fileForModule ~package moduleName with
      | None -> None
      | Some file ->
        let env = QueryEnv.fromFile file in
        resolvePath ~env ~package ~path
    in
    match res with
    | None -> `Not_found
    | Some (env, name) -> `Exported (env, name))
  | `Stamp stamp -> `Stamp stamp
  | `GlobalMod _ -> `Not_found
  | `Not_found -> `Not_found
  | `Exported (env, name) -> `Exported (env, name)

let rec getSourceUri ~(env : QueryEnv.t) ~package path =
  match path with
  | File (uri, _moduleName) -> uri
  | NotVisible -> env.file.uri
  | IncludedModule (path, inner) -> (
    Log.log "INCLUDED MODULE";
    match resolveModuleFromCompilerPath ~env ~package path with
    | None ->
      Log.log "NOT FOUND";
      getSourceUri ~env ~package inner
    | Some (env, _declared) -> env.file.uri)
  | ExportedModule (_, inner) -> getSourceUri ~env ~package inner

let exportedForTip ~(env : QueryEnv.t) name (tip : Tip.t) =
  match tip with
  | Value -> Exported.find env.exported Exported.Value name
  | Field _ | Constructor _ | Type ->
    Exported.find env.exported Exported.Type name
  | Module -> Exported.find env.exported Exported.Module name

module ZZZ = Tast_iterator
