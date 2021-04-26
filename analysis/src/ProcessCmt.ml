open Typedtree
open SharedTypes
open Infix

type queryEnv = {qFile : file; qExported : exported}

let fileEnv file = {qFile = file; qExported = file.contents.exported}

let itemsExtent items =
  let open Typedtree in
  match items with
  | [] -> Location.none
  | first :: _ ->
    let last = List.nth items (List.length items - 1) in
    let first, last =
      if first.str_loc.loc_start.pos_cnum < last.str_loc.loc_start.pos_cnum then
        (first, last)
      else (last, first)
    in
    {
      loc_ghost = true;
      loc_start = first.str_loc.loc_start;
      loc_end = last.str_loc.loc_end;
    }

let sigItemsExtent items =
  let open Typedtree in
  match items with
  | [] -> Location.none
  | first :: _ ->
    let last = List.nth items (List.length items - 1) in
    {
      Location.loc_ghost = true;
      loc_start = first.sig_loc.loc_start;
      loc_end = last.sig_loc.loc_end;
    }

type env = {stamps : stamps; modulePath : visibilityPath; scope : Location.t}

let addItem ~name ~extent ~stamp ~env ~item attributes exported stamps =
  let declared =
    ProcessAttributes.newDeclared ~item
      ~scope:
        {
          Location.loc_start = extent.Location.loc_end;
          loc_end = env.scope.loc_end;
          loc_ghost = false;
        }
      ~extent ~name ~stamp ~modulePath:env.modulePath
      (not (Hashtbl.mem exported name.txt))
      attributes
  in
  if not (Hashtbl.mem exported name.txt) then
    Hashtbl.add exported name.txt stamp;
  Hashtbl.add stamps stamp declared;
  declared

let rec forSignatureTypeItem env (exported : SharedTypes.exported) item =
  let open Types in
  match item with
  | Sig_value (ident, {val_type; val_attributes; val_loc = loc}) ->
    let item = val_type in
    let declared =
      addItem
        ~name:(Location.mknoloc (Ident.name ident))
        ~extent:loc ~stamp:(Ident.binding_time ident) ~env ~item val_attributes
        exported.values env.stamps.values
    in
    [{declared with item = MValue declared.item}]
  | Sig_type
      ( ident,
        ({type_loc; type_kind; type_manifest; type_attributes} as decl),
        recStatus ) ->
    let declared =
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
                       (fun {cd_loc; cd_id; cd_args; cd_res; cd_attributes} ->
                         let name = Ident.name cd_id in
                         let stamp = Ident.binding_time cd_id in
                         let item =
                           {
                             stamp;
                             cname = Location.mknoloc name;
                             args =
                               (match cd_args with
                               | Cstr_tuple args ->
                                 args |> List.map (fun t -> (t, Location.none))
                               (* TODO(406): constructor record args support *)
                               | Cstr_record _ -> []);
                             res = cd_res;
                           }
                         in
                         let declared =
                           ProcessAttributes.newDeclared ~item ~extent:cd_loc
                             ~scope:
                               {
                                 Location.loc_start = type_loc.Location.loc_end;
                                 loc_end = env.scope.loc_end;
                                 loc_ghost = false;
                               }
                             ~name:(Location.mknoloc name)
                             ~stamp (* TODO maybe this needs another child *)
                             ~modulePath:env.modulePath true cd_attributes
                         in
                         Hashtbl.add env.stamps.constructors stamp declared;
                         item))
              | Type_record (fields, _) ->
                Record
                  (fields
                  |> List.map (fun {ld_id; ld_type} ->
                         let astamp = Ident.binding_time ld_id in
                         let name = Ident.name ld_id in
                         {
                           stamp = astamp;
                           fname = Location.mknoloc name;
                           typ = ld_type;
                         })));
          }
        ~name:(Location.mknoloc (Ident.name ident))
        ~stamp:(Ident.binding_time ident) ~env type_attributes exported.types
        env.stamps.types
    in
    [{declared with item = MType (declared.item, recStatus)}]
  (* | Sig_module({stamp, name}, {md_type: Mty_ident(path) | Mty_alias(path), md_attributes, md_loc}, _) =>
     let declared = addItem(~contents=Module.Ident(path), ~name=Location.mknoloc(name), ~stamp, ~env, md_attributes, exported.modules, env.stamps.modules);
     [{...declared, contents: Module.Module(declared.contents)}, ...items] *)
  | Sig_module (ident, {md_type; md_attributes; md_loc}, _) ->
    let declared =
      addItem ~extent:md_loc
        ~item:(forModuleType env md_type)
        ~name:(Location.mknoloc (Ident.name ident))
        ~stamp:(Ident.binding_time ident) ~env md_attributes exported.modules
        env.stamps.modules
    in
    [{declared with item = Module declared.item}]
  | _ -> []

and forSignatureType env signature =
  let exported = initExported () in
  let topLevel =
    List.fold_right
      (fun item items -> forSignatureTypeItem env exported item @ items)
      signature []
  in
  {docstring = []; exported; topLevel}

and forModuleType env moduleType =
  match moduleType with
  | Types.Mty_ident path -> Ident path
  | Mty_alias (_ (* 402 *), path) -> Ident path
  | Mty_signature signature -> Structure (forSignatureType env signature)
  | Mty_functor (_argIdent, _argType, resultType) ->
    forModuleType env resultType

let getModuleTypePath mod_desc =
  match mod_desc with
  | Tmty_ident (path, _) | Tmty_alias (path, _) -> Some path
  | Tmty_signature _ | Tmty_functor _ | Tmty_with _ | Tmty_typeof _ -> None

let forTypeDeclaration ~env ~(exported : exported)
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
                |> List.map (fun {cd_id; cd_name = cname; cd_args; cd_res} ->
                       let stamp = Ident.binding_time cd_id in
                       {
                         stamp;
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
                       }))
            | Ttype_record fields ->
              Record
                (fields
                |> List.map
                     (fun {ld_id; ld_name = fname; ld_type = {ctyp_type}} ->
                       let fstamp = Ident.binding_time ld_id in
                       {stamp = fstamp; fname; typ = ctyp_type})));
        }
      ~name ~stamp ~env typ_attributes exported.types env.stamps.types
  in
  {declared with item = MType (declared.item, recStatus)}

let forSignatureItem ~env ~(exported : exported) item =
  match item.sig_desc with
  | Tsig_value {val_id; val_loc; val_name = name; val_desc; val_attributes} ->
    let declared =
      addItem ~name
        ~stamp:(Ident.binding_time val_id)
        ~extent:val_loc ~item:val_desc.ctyp_type ~env val_attributes
        exported.values env.stamps.values
    in
    [{declared with item = MValue declared.item}]
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
    let item = forModuleType env mty_type in
    let declared =
      addItem ~item ~name ~extent:md_loc ~stamp:(Ident.binding_time md_id) ~env
        md_attributes exported.modules env.stamps.modules
    in
    [{declared with item = Module declared.item}]
  | Tsig_include {incl_mod; incl_type} ->
    let env =
      match getModuleTypePath incl_mod.mty_desc with
      | None -> env
      | Some path ->
        {env with modulePath = IncludedModule (path, env.modulePath)}
    in
    let topLevel =
      List.fold_right
        (fun item items -> forSignatureTypeItem env exported item @ items)
        incl_type []
    in
    topLevel
  (* TODO: process other things here *)
  | _ -> []

let forSignature ~env items =
  let exported = initExported () in
  let topLevel =
    items |> List.map (forSignatureItem ~env ~exported) |> List.flatten
  in
  let attributes =
    match items with
    | {sig_desc = Tsig_attribute attribute} :: _ -> [attribute]
    | _ -> []
  in
  let docstring =
    match ProcessAttributes.findDocAttribute attributes with
    | None -> []
    | Some d -> [d]
  in
  {docstring; exported; topLevel}

let forTreeModuleType ~env {mty_desc} =
  match mty_desc with
  | Tmty_ident _ -> None
  | Tmty_signature {sig_items} ->
    let contents = forSignature ~env sig_items in
    Some (Structure contents)
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

let rec forItem ~env ~(exported : exported) item =
  match item.str_desc with
  | Tstr_value (_isRec, bindings) ->
    optMap
      (fun {vb_loc; vb_pat = {pat_desc; pat_type}; vb_attributes} ->
        (* TODO get all the things out of the var. *)
        match pat_desc with
        | Tpat_var (ident, name)
        | Tpat_alias ({pat_desc = Tpat_any}, ident, name) (* let x : t = ... *)
          ->
          let item = pat_type in
          let declared =
            addItem ~name ~stamp:(Ident.binding_time ident) ~env ~extent:vb_loc
              ~item vb_attributes exported.values env.stamps.values
          in
          Some {declared with item = MValue declared.item}
        | _ -> None)
      bindings
  | Tstr_module
      {mb_id; mb_attributes; mb_loc; mb_name = name; mb_expr = {mod_desc}} ->
    let item = forModule env mod_desc name.txt in
    let declared =
      addItem ~item ~name ~extent:mb_loc ~stamp:(Ident.binding_time mb_id) ~env
        mb_attributes exported.modules env.stamps.modules
    in
    [{declared with item = Module declared.item}]
  | Tstr_include {incl_mod; incl_type} ->
    let env =
      match getModulePath incl_mod.mod_desc with
      | None -> env
      | Some path ->
        {env with modulePath = IncludedModule (path, env.modulePath)}
    in
    let topLevel =
      List.fold_right
        (fun item items -> forSignatureTypeItem env exported item @ items)
        incl_type []
    in
    topLevel
  | Tstr_primitive
      {val_id; val_name = name; val_loc; val_attributes; val_val = {val_type}}
    ->
    let declared =
      addItem ~extent:val_loc ~item:val_type ~name
        ~stamp:(Ident.binding_time val_id)
        ~env val_attributes exported.values env.stamps.values
    in
    [{declared with item = MValue declared.item}]
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
      {
        env with
        scope = itemsExtent structure.str_items;
        modulePath = ExportedModule (moduleName, env.modulePath);
      }
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
            ~scope:
              {
                Location.loc_start = t.mty_loc.loc_end;
                loc_end = env.scope.loc_end;
                loc_ghost = false;
              }
            ~extent:t.Typedtree.mty_loc ~stamp ~modulePath:NotVisible false []
        in
        Hashtbl.add env.stamps.modules stamp declared));
    forModule env resultExpr.mod_desc moduleName
  | Tmod_apply (functor_, _arg, _coercion) ->
    forModule env functor_.mod_desc moduleName
  | Tmod_unpack (_expr, moduleType) ->
    let env =
      {env with modulePath = ExportedModule (moduleName, env.modulePath)}
    in
    forModuleType env moduleType
  | Tmod_constraint (expr, _typ, Tmodtype_implicit, Tcoerce_structure _) ->
    (* implicit contraint synthesized during typechecking *)
    (* e.g. when the same id is defined twice (e.g. make with @react.component) *)
    (* skip the constraint and use the original module definition *)
    forModule env expr.mod_desc moduleName
  | Tmod_constraint (_expr, typ, _constraint, _coercion) ->
    (* TODO do this better I think *)
    let env =
      {env with modulePath = ExportedModule (moduleName, env.modulePath)}
    in
    forModuleType env typ

and forStructure ~env items =
  let exported = initExported () in
  let topLevel =
    List.fold_right
      (fun item results -> forItem ~env ~exported item @ results)
      items []
  in
  let attributes =
    match items with
    | {str_desc = Tstr_attribute attribute} :: _ -> [attribute]
    | _ -> []
  in
  let docstring =
    match ProcessAttributes.findDocAttribute attributes with
    | None -> []
    | Some d -> [d]
  in
  {docstring; exported; topLevel}

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
    let extent = itemsExtent items in
    let extent =
      {
        extent with
        loc_end =
          {
            extent.loc_end with
            pos_lnum = extent.loc_end.pos_lnum + 1000000;
            pos_cnum = extent.loc_end.pos_cnum + 100000000;
          };
      }
    in
    let env =
      {
        scope = extent;
        stamps = initStamps ();
        modulePath = File (uri, moduleName);
      }
    in
    let contents = forStructure ~env items in
    {uri; moduleName = cmt_modname; stamps = env.stamps; contents}
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
      {
        scope = sigItemsExtent items;
        stamps = initStamps ();
        modulePath = File (uri, moduleName);
      }
    in
    let contents = forSignature ~env items in
    {uri; moduleName = cmt_modname; stamps = env.stamps; contents}
  | Implementation structure ->
    let env =
      {
        scope = itemsExtent structure.str_items;
        stamps = initStamps ();
        modulePath = File (uri, moduleName);
      }
    in
    let contents = forStructure ~env structure.str_items in
    {uri; moduleName = cmt_modname; stamps = env.stamps; contents}
  | Interface signature ->
    let env =
      {
        scope = sigItemsExtent signature.sig_items;
        stamps = initStamps ();
        modulePath = File (uri, moduleName);
      }
    in
    let contents = forSignature ~env signature.sig_items in
    {uri; moduleName = cmt_modname; stamps = env.stamps; contents}
  | _ -> SharedTypes.emptyFile moduleName uri

let fileForCmt ~moduleName ~uri cmt =
  match Shared.tryReadCmt cmt with
  | Error e -> Error e
  | Ok infos -> Ok (forCmt ~moduleName ~uri infos)

let extraForFile ~(file : SharedTypes.file) =
  let extra = initExtra () in
  let addLocation loc ident =
    extra.locations <- (loc, ident) :: extra.locations
  in
  let addReference stamp loc =
    Hashtbl.replace extra.internalReferences stamp
      (loc
      ::
      (if Hashtbl.mem extra.internalReferences stamp then
      Hashtbl.find extra.internalReferences stamp
    else []))
  in
  file.stamps.modules
  |> Hashtbl.iter (fun stamp d ->
         addLocation d.name.loc (LModule (Definition (stamp, Module)));
         addReference stamp d.name.loc);
  file.stamps.values
  |> Hashtbl.iter (fun stamp d ->
         addLocation d.name.loc (Typed (d.item, Definition (stamp, Value)));
         addReference stamp d.name.loc);
  file.stamps.types
  |> Hashtbl.iter (fun stamp d ->
         addLocation d.name.loc
           (TypeDefinition (d.name.txt, d.item.Type.decl, stamp));
         addReference stamp d.name.loc;
         match d.item.Type.kind with
         | Record labels ->
           labels
           |> List.iter (fun {stamp; fname; typ} ->
                  addReference stamp fname.loc;
                  addLocation fname.loc
                    (Typed (typ, Definition (d.stamp, Field fname.txt))))
         | Variant constructos ->
           constructos
           |> List.iter (fun {stamp; cname} ->
                  addReference stamp cname.loc;
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
                  addLocation cname.loc
                    (Typed (t, Definition (d.stamp, Constructor cname.txt))))
         | _ -> ());
  extra

let rec relative ident path =
  match (ident, path) with
  | Longident.Lident name, Path.Pdot (path, pname, _) when pname = name ->
    Some path
  | Longident.Ldot (ident, name), Path.Pdot (path, pname, _) when pname = name
    ->
    relative ident path
  (* | (Ldot(Lident("*predef*" | "exn"), _), Pident(_)) => None *)
  | _ -> None

let findClosestMatchingOpen opens path ident loc =
  match relative ident path with
  | None -> None
  | Some openNeedle -> (
    let matching =
      Hashtbl.fold
        (fun _ op res ->
          if Utils.locWithinLoc loc op.extent && Path.same op.path openNeedle
          then op :: res
          else res)
        opens []
      |> List.sort (fun (a : SharedTypes.openTracker) b ->
             b.loc.loc_start.pos_cnum - a.loc.loc_start.pos_cnum)
    in
    match matching with [] -> None | first :: _ -> Some first)

let rec joinPaths modulePath path =
  match modulePath with
  | Path.Pident ident -> (ident.stamp, ident.name, path)
  | Papply (fnPath, _argPath) -> joinPaths fnPath path
  | Pdot (inner, name, _) -> joinPaths inner (Nested (name, path))

let rec makePath modulePath =
  match modulePath with
  | Path.Pident ident when ident.stamp == 0 -> `GlobalMod ident.name
  | Pident ident -> `Stamp ident.stamp
  | Papply (fnPath, _argPath) -> makePath fnPath
  | Pdot (inner, name, _) -> `Path (joinPaths inner (Tip name))

let rec resolvePathInner ~env ~path =
  match path with
  | Tip name -> Some (`Local (env, name))
  | Nested (subName, subPath) -> (
    match Hashtbl.find_opt env.qExported.modules subName with
    | None -> None
    | Some stamp -> (
      match Hashtbl.find_opt env.qFile.stamps.modules stamp with
      | None -> None
      | Some {item = kind} -> findInModule ~env kind subPath))

and findInModule ~env kind path =
  match kind with
  | Structure {exported} ->
    resolvePathInner ~env:{env with qExported = exported} ~path
  | Ident modulePath -> (
    let stamp, moduleName, fullPath = joinPaths modulePath path in
    if stamp = 0 then Some (`Global (moduleName, fullPath))
    else
      match Hashtbl.find_opt env.qFile.stamps.modules stamp with
      | None -> None
      | Some {item = kind} -> findInModule ~env kind fullPath)

let fromCompilerPath ~env path =
  match makePath path with
  | `Stamp stamp -> `Stamp stamp
  | `Path (0, moduleName, path) -> `Global (moduleName, path)
  | `GlobalMod name -> `GlobalMod name
  | `Path (stamp, _moduleName, path) -> (
    let res =
      match Hashtbl.find_opt env.qFile.stamps.modules stamp with
      | None -> None
      | Some {item = kind} -> findInModule ~env kind path
    in
    match res with
    | None -> `Not_found
    | Some (`Local (env, name)) -> `Exported (env, name)
    | Some (`Global (moduleName, fullPath)) -> `Global (moduleName, fullPath))

module F (Collector : sig
  val extra : extra

  val file : file

  val scopeExtent : Location.t list ref
end) =
struct
  let extra = Collector.extra

  let makeRelativePath basePath otherPath =
    let rec loop base other tip =
      if Path.same base other then Some tip
      else
        match other with
        | Pdot (inner, name, _) -> loop basePath inner (Nested (name, tip))
        | _ -> None
    in
    match otherPath with
    | Path.Pdot (inner, name, _) -> loop basePath inner (Tip name)
    | _ -> None

  let maybeAddUse path ident loc tip =
    match findClosestMatchingOpen extra.opens path ident loc with
    | None -> ()
    | Some tracker -> (
      match makeRelativePath tracker.path path with
      | None -> ()
      | Some relpath -> tracker.used <- (relpath, tip, loc) :: tracker.used)

  let addLocation loc ident = extra.locations <- (loc, ident) :: extra.locations

  let addReference stamp loc =
    Hashtbl.replace extra.internalReferences stamp
      (loc
      ::
      (if Hashtbl.mem extra.internalReferences stamp then
       Hashtbl.find extra.internalReferences stamp
      else []))

  let addExternalReference moduleName path tip loc =
    (* TODO need to follow the path, and be able to load the files to follow module references... *)
    Hashtbl.replace extra.externalReferences moduleName
      ((path, tip, loc)
      ::
      (if Hashtbl.mem extra.externalReferences moduleName then
       Hashtbl.find extra.externalReferences moduleName
      else []))

  let env = fileEnv Collector.file

  let addForPath path lident loc typ tip =
    maybeAddUse path lident loc tip;
    let identName = Longident.last lident in
    let identLoc = Utils.endOfLocation loc (String.length identName) in
    let locType =
      match fromCompilerPath ~env path with
      | `Stamp stamp ->
        addReference stamp identLoc;
        LocalReference (stamp, tip)
      | `Not_found -> NotFound
      | `Global (moduleName, path) ->
        addExternalReference moduleName path tip identLoc;
        GlobalReference (moduleName, path, tip)
      | `Exported (env, name) -> (
        match
          Hashtbl.find_opt
            (match tip with
            | Type -> env.qExported.types
            | _ -> env.qExported.values)
            name
        with
        | Some stamp ->
          addReference stamp identLoc;
          LocalReference (stamp, tip)
        | None -> NotFound)
      | `GlobalMod _ -> NotFound
    in
    addLocation loc (Typed (typ, locType))

  let addForPathParent path loc =
    let locType =
      match fromCompilerPath ~env path with
      | `GlobalMod name ->
        (* TODO track external references to filenames to handle renames well *)
        TopLevelModule name
      | `Stamp stamp ->
        addReference stamp loc;
        LModule (LocalReference (stamp, Module))
      | `Not_found -> LModule NotFound
      | `Global (moduleName, path) ->
        addExternalReference moduleName path Module loc;
        LModule (GlobalReference (moduleName, path, Module))
      | `Exported (env, name) -> (
        match Hashtbl.find_opt env.qExported.modules name with
        | Some stamp ->
          addReference stamp loc;
          LModule (LocalReference (stamp, Module))
        | None -> LModule NotFound)
    in
    addLocation loc locType

  let getTypeAtPath ~env path =
    match fromCompilerPath ~env path with
    | `GlobalMod _ -> `Not_found
    | `Global (moduleName, path) -> `Global (moduleName, path)
    | `Not_found -> `Not_found
    | `Exported (env, name) -> (
      match Hashtbl.find_opt env.qExported.types name with
      | None -> `Not_found
      | Some stamp -> (
        let declaredType = Hashtbl.find_opt env.qFile.stamps.types stamp in
        match declaredType with
        | Some declaredType -> `Local declaredType
        | None -> `Not_found))
    | `Stamp stamp -> (
      let declaredType = Hashtbl.find_opt env.qFile.stamps.types stamp in
      match declaredType with
      | Some declaredType -> `Local declaredType
      | None -> `Not_found)

  let handleConstructor path txt =
    let typeName =
      match path with
      | Path.Pdot (_path, typename, _) -> typename
      | Pident ident -> Ident.name ident
      | _ -> assert false
    in
    let open Longident in
    match txt with
    | Longident.Lident name -> (name, Lident typeName)
    | Ldot (left, name) -> (name, Ldot (left, typeName))
    | Lapply (_, _) -> assert false

  let addForField recordType item {Asttypes.txt; loc} =
    match (Shared.dig recordType).desc with
    | Tconstr (path, _args, _memo) ->
      let t = getTypeAtPath ~env path in
      let {Types.lbl_res} = item in
      let name, typeLident = handleConstructor path txt in
      maybeAddUse path typeLident loc (Field name);
      let nameLoc = Utils.endOfLocation loc (String.length name) in
      let locType =
        match t with
        | `Local {stamp; item = {kind = Record fields}} -> (
          match fields |> List.find_opt (fun f -> f.fname.txt = name) with
          | Some {stamp = astamp} ->
            addReference astamp nameLoc;
            LocalReference (stamp, Field name)
          | None -> NotFound)
        | `Global (moduleName, path) ->
          addExternalReference moduleName path (Field name) nameLoc;
          GlobalReference (moduleName, path, Field name)
        | _ -> NotFound
      in
      addLocation nameLoc (Typed (lbl_res, locType))
    | _ -> ()

  let addForRecord recordType items =
    match (Shared.dig recordType).desc with
    | Tconstr (path, _args, _memo) ->
      let t = getTypeAtPath ~env path in
      items
      |> List.iter (fun ({Asttypes.txt; loc}, {Types.lbl_res}, _) ->
             (* let name = Longident.last(txt); *)
             let name, typeLident = handleConstructor path txt in
             maybeAddUse path typeLident loc (Field name);
             let nameLoc = Utils.endOfLocation loc (String.length name) in
             let locType =
               match t with
               | `Local {stamp; item = {kind = Record fields}} -> (
                 match
                   fields |> List.find_opt (fun f -> f.fname.txt = name)
                 with
                 | Some {stamp = astamp} ->
                   addReference astamp nameLoc;
                   LocalReference (stamp, Field name)
                 | None -> NotFound)
               | `Global (moduleName, path) ->
                 addExternalReference moduleName path (Field name) nameLoc;
                 GlobalReference (moduleName, path, Field name)
               | _ -> NotFound
             in
             addLocation nameLoc (Typed (lbl_res, locType)))
    | _ -> ()

  let addForConstructor constructorType {Asttypes.txt; loc} {Types.cstr_name} =
    match (Shared.dig constructorType).desc with
    | Tconstr (path, _args, _memo) ->
      (* let name = Longident.last(txt); *)
      let name, typeLident = handleConstructor path txt in
      maybeAddUse path typeLident loc (Constructor name);
      let nameLoc = Utils.endOfLocation loc (String.length name) in
      let t = getTypeAtPath ~env path in
      let locType =
        match t with
        | `Local {stamp; item = {kind = Variant constructors}} -> (
          match
            constructors |> List.find_opt (fun c -> c.cname.txt = cstr_name)
          with
          | Some {stamp = cstamp} ->
            addReference cstamp nameLoc;
            LocalReference (stamp, Constructor name)
          | None -> NotFound)
        | `Global (moduleName, path) ->
          addExternalReference moduleName path (Constructor name) nameLoc;
          GlobalReference (moduleName, path, Constructor name)
        | _ -> NotFound
      in
      addLocation nameLoc (Typed (constructorType, locType))
    | _ -> ()

  let currentScopeExtent () =
    if !Collector.scopeExtent = [] then Location.none
    else List.hd !Collector.scopeExtent

  let addScopeExtent loc =
    Collector.scopeExtent := loc :: !Collector.scopeExtent

  let popScopeExtent () =
    if List.length !Collector.scopeExtent > 1 then
      Collector.scopeExtent := List.tl !Collector.scopeExtent

  let rec addForLongident top (path : Path.t) (txt : Longident.t) loc =
    if not loc.Location.loc_ghost then (
      let idLength =
        String.length (String.concat "." (Longident.flatten txt))
      in
      let reportedLength = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
      let isPpx = idLength <> reportedLength in
      if isPpx then
        match top with
        | Some (t, tip) -> addForPath path txt loc t tip
        | None -> addForPathParent path loc
      else
        let l = Utils.endOfLocation loc (String.length (Longident.last txt)) in
        (match top with
        | Some (t, tip) -> addForPath path txt l t tip
        | None -> addForPathParent path l);
        match (path, txt) with
        | Pdot (pinner, _pname, _), Ldot (inner, name) ->
          addForLongident None pinner inner
            (Utils.chopLocationEnd loc (String.length name + 1))
        | Pident _, Lident _ -> ()
        | _ -> ())

  let rec handle_module_expr expr =
    match expr with
    | Tmod_constraint (expr, _, _, _) -> handle_module_expr expr.mod_desc
    | Tmod_ident (path, {txt; loc}) ->
      Log.log ("Ident!! " ^ String.concat "." (Longident.flatten txt));
      maybeAddUse path txt loc Module;
      addForLongident None path txt loc
    | Tmod_functor (_ident, _argName, _maybeType, resultExpr) ->
      handle_module_expr resultExpr.mod_desc
    | Tmod_apply (obj, arg, _) ->
      handle_module_expr obj.mod_desc;
      handle_module_expr arg.mod_desc
    | _ -> ()

  open Typedtree
  include TypedtreeIter.DefaultIteratorArgument

  let enter_structure_item item =
    match item.str_desc with
    | Tstr_attribute
        ( {Asttypes.txt = "ocaml.explanation"; loc},
          PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ({pexp_desc = Pexp_constant (Pconst_string (doc, _))}, _);
              };
            ] ) ->
      addLocation loc (Explanation doc)
    | Tstr_include {incl_mod = expr} -> handle_module_expr expr.mod_desc
    | Tstr_module {mb_expr} -> handle_module_expr mb_expr.mod_desc
    | Tstr_open {open_path; open_txt = {txt; loc}} ->
      (* Log.log("Have an open here"); *)
      maybeAddUse open_path txt loc Module;
      let tracker =
        {
          path = open_path;
          loc;
          used = [];
          extent =
            {
              loc_ghost = true;
              loc_start = loc.loc_end;
              loc_end = (currentScopeExtent ()).loc_end;
            };
        }
      in
      addForLongident None open_path txt loc;
      Hashtbl.replace Collector.extra.opens loc tracker
    | _ -> ()

  let enter_structure {str_items} =
    if str_items <> [] then
      let first = List.hd str_items in
      let last = List.nth str_items (List.length str_items - 1) in
      let extent =
        {
          Location.loc_ghost = true;
          loc_start = first.str_loc.loc_start;
          loc_end = last.str_loc.loc_end;
        }
      in
      addScopeExtent extent

  let leave_structure str = if str.str_items <> [] then popScopeExtent ()

  let enter_signature_item item =
    match item.sig_desc with
    | Tsig_value {val_id; val_loc; val_name = name; val_desc; val_attributes} ->
      let stamp = Ident.binding_time val_id in
      if not (Hashtbl.mem Collector.file.stamps.values stamp) then (
        let declared =
          ProcessAttributes.newDeclared ~name ~stamp ~extent:val_loc
            ~scope:
              {
                loc_ghost = true;
                loc_start = val_loc.loc_end;
                loc_end = (currentScopeExtent ()).loc_end;
              }
            ~modulePath:NotVisible ~item:val_desc.ctyp_type false val_attributes
        in
        Hashtbl.add Collector.file.stamps.values stamp declared;
        addReference stamp name.loc;
        addLocation name.loc
          (Typed (val_desc.ctyp_type, Definition (stamp, Value))))
    | _ -> ()

  let enter_core_type {ctyp_type; ctyp_desc} =
    match ctyp_desc with
    | Ttyp_constr (path, {txt; loc}, _args) ->
      (* addForPath(path, txt, loc, Shared.makeFlexible(ctyp_type), Type) *)
      addForLongident (Some (ctyp_type, Type)) path txt loc
    | _ -> ()

  let enter_pattern {pat_desc; pat_loc; pat_type; pat_attributes} =
    let addForPattern stamp name =
      if not (Hashtbl.mem Collector.file.stamps.values stamp) then (
        let declared =
          ProcessAttributes.newDeclared ~name ~stamp
            ~scope:
              {
                loc_ghost = true;
                loc_start = pat_loc.loc_end;
                loc_end = (currentScopeExtent ()).loc_end;
              }
            ~modulePath:NotVisible ~extent:pat_loc ~item:pat_type false
            pat_attributes
        in
        Hashtbl.add Collector.file.stamps.values stamp declared;
        addReference stamp name.loc;
        addLocation name.loc (Typed (pat_type, Definition (stamp, Value))))
    in
    (* Log.log("Entering pattern " ++ Utils.showLocation(pat_loc)); *)
    match pat_desc with
    | Tpat_record (items, _) -> addForRecord pat_type items
    | Tpat_construct (lident, constructor, _) ->
      addForConstructor pat_type lident constructor
    | Tpat_alias (_inner, ident, name) ->
      let stamp = Ident.binding_time ident in
      addForPattern stamp name
    | Tpat_var (ident, name) ->
      (* Log.log("Pattern " ++ name.txt); *)
      let stamp = Ident.binding_time ident in
      addForPattern stamp name
    | _ -> ()

  let enter_expression expression =
    expression.exp_extra
    |> List.iter (fun (e, eloc, _) ->
           match e with
           | Texp_open (_, path, _ident, _) ->
             Hashtbl.add extra.opens eloc
               {path; loc = eloc; extent = expression.exp_loc; used = []}
           | _ -> ());
    match expression.exp_desc with
    | Texp_ident (path, {txt; loc}, {val_type}) ->
      addForLongident (Some (val_type, Value)) path txt loc
    | Texp_record {fields} ->
      addForRecord expression.exp_type
        (fields |> Array.to_list
        |> Utils.filterMap (fun (desc, item) ->
               match item with
               | Overridden (loc, _) -> Some (loc, desc, ())
               | _ -> None))
    | Texp_constant constant ->
      addLocation expression.exp_loc (Constant constant)
    (* Skip unit and list literals *)
    | Texp_construct ({txt = Lident ("()" | "::"); loc}, _, _args)
      when loc.loc_end.pos_cnum - loc.loc_start.pos_cnum <> 2 ->
      ()
    | Texp_construct (lident, constructor, _args) ->
      addForConstructor expression.exp_type lident constructor
    | Texp_field (inner, lident, label_description) ->
      addForField inner.exp_type label_description lident
    | Texp_let (_, _, _) ->
      (* TODO this scope tracking won't work for recursive *)
      addScopeExtent expression.exp_loc
    | Texp_function {cases} -> (
      match cases with [{c_rhs}] -> addScopeExtent c_rhs.exp_loc | _ -> ())
    | _ -> ()

  let leave_expression expression =
    match expression.exp_desc with
    | Texp_let (_isrec, _bindings, _expr) -> popScopeExtent ()
    | Texp_function {cases} -> (
      match cases with [_] -> popScopeExtent () | _ -> ())
    | _ -> ()
end

let forItems ~(file : SharedTypes.file) items parts =
  let extra = extraForFile ~file in
  let extent = itemsExtent items in
  let extent =
    {
      extent with
      loc_end =
        {
          extent.loc_end with
          pos_lnum = extent.loc_end.pos_lnum + 1000000;
          pos_cnum = extent.loc_end.pos_cnum + 100000000;
        };
    }
  in
  (* TODO look through parts and extend the extent *)
  let module Iter = TypedtreeIter.MakeIterator (F (struct
    let scopeExtent = ref [extent]

    let extra = extra

    let file = file
  end)) in
  List.iter Iter.iter_structure_item items;
  (* Log.log("Parts " ++ string_of_int(Array.length(parts))); *)
  parts
  |> Array.iter (fun part ->
         match part with
         | Cmt_format.Partial_signature str -> Iter.iter_signature str
         | Partial_signature_item str -> Iter.iter_signature_item str
         | Partial_expression expression -> Iter.iter_expression expression
         | Partial_pattern pattern -> Iter.iter_pattern pattern
         | Partial_class_expr class_expr -> Iter.iter_class_expr class_expr
         | Partial_module_type module_type -> Iter.iter_module_type module_type
         | Partial_structure _ | Partial_structure_item _ -> ());
  extra

let extraForCmt ~file ({cmt_annots} : Cmt_format.cmt_infos) =
  match cmt_annots with
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
    forItems ~file items parts
  | Implementation structure -> forItems ~file structure.str_items [||]
  | Partial_interface _ | Interface _ -> forItems ~file [] [||]
  | _ -> forItems ~file [] [||]

let fullForCmt ~moduleName ~uri cmt =
  match Shared.tryReadCmt cmt with
  | Error e -> Error e
  | Ok infos ->
    let file = forCmt ~moduleName ~uri infos in
    let extra = extraForCmt ~file infos in
    Ok {SharedTypes.file; extra}

open SharedTypes

let newDocsForCmt ~moduleName cmtCache changed cmt src =
  let open Infix in
  let uri = Uri2.fromPath (src |? cmt) in
  match fileForCmt ~moduleName ~uri cmt with
  | Error e ->
    Log.log e;
    None
  | Ok file ->
    Hashtbl.replace cmtCache cmt (changed, file);
    Some file

let getFullFromCmt ~uri =
  let path = Uri2.toPath uri in
  match Packages.getPackage uri with
  | Error e -> Error e
  | Ok package -> (
    let moduleName =
      BuildSystem.namespacedName package.namespace (FindFiles.getName path)
    in
    match Hashtbl.find_opt package.pathsForModule moduleName with
    | Some paths -> (
      let cmt = SharedTypes.getCmt ~interface:(Utils.endsWith path "i") paths in
      match fullForCmt ~moduleName ~uri cmt with
      | Error e -> Error e
      | Ok full ->
        Hashtbl.replace package.interModuleDependencies moduleName
          (SharedTypes.hashList full.extra.externalReferences |> List.map fst);
        Ok (package, full))
    | None -> Error ("can't find module " ^ moduleName))

let fileForUri uri =
  match getFullFromCmt ~uri with
  | Error e -> Error e
  | Ok (_package, {extra; file}) -> Ok (file, extra)

let extraForModule ~package modname =
  if Hashtbl.mem package.TopTypes.pathsForModule modname then
    let paths = Hashtbl.find package.pathsForModule modname in
    match SharedTypes.getSrc paths with
    | None -> None
    | Some src -> (
      match getFullFromCmt ~uri:(Uri2.fromPath src) with
      | Ok (_package, {extra}) -> Some extra
      | Error _ -> None)
  else None

let docsForCmt ~moduleName cmt src state =
  if Hashtbl.mem state.TopTypes.cmtCache cmt then
    let mtime, docs = Hashtbl.find state.cmtCache cmt in
    (* TODO: I should really throttle this mtime checking to like every 50 ms or so *)
    match Files.getMtime cmt with
    | None ->
      Log.log
        ("\226\154\160\239\184\143 cannot get docs for nonexistant cmt " ^ cmt);
      None
    | Some changed ->
      if changed > mtime then
        newDocsForCmt ~moduleName state.cmtCache changed cmt src
      else Some docs
  else
    match Files.getMtime cmt with
    | None ->
      Log.log
        ("\226\154\160\239\184\143 cannot get docs for nonexistant cmt " ^ cmt);
      None
    | Some changed -> newDocsForCmt ~moduleName state.cmtCache changed cmt src

let docsForModule modname ~package =
  if Hashtbl.mem package.TopTypes.pathsForModule modname then (
    let paths = Hashtbl.find package.pathsForModule modname in
    (* TODO: do better *)
    let cmt = SharedTypes.getCmt paths in
    let src = SharedTypes.getSrc paths in
    Log.log ("FINDING docs for module " ^ SharedTypes.showPaths paths);
    let open Infix in
    Log.log ("FINDING " ^ cmt ^ " src " ^ (src |? ""));
    match docsForCmt ~moduleName:modname cmt src TopTypes.state with
    | None -> None
    | Some docs -> Some (docs, src))
  else (
    Log.log ("No path for module " ^ modname);
    None)

let fileForModule ~package modname =
  match docsForModule modname ~package with
  | None -> None
  | Some (file, _) -> Some file

let rec resolvePath ~env ~path ~package =
  match resolvePathInner ~env ~path with
  | None -> None
  | Some result -> (
    match result with
    | `Local (env, name) -> Some (env, name)
    | `Global (moduleName, fullPath) -> (
      match fileForModule ~package moduleName with
      | None -> None
      | Some file -> resolvePath ~env:(fileEnv file) ~path:fullPath ~package))

let tupleOfLexing {Lexing.pos_lnum; pos_cnum; pos_bol} =
  (pos_lnum - 1, pos_cnum - pos_bol)

let locationIsBefore {Location.loc_start} pos = tupleOfLexing loc_start <= pos

let findInScope pos name stamps =
  (* Log.log("Find " ++ name ++ " with " ++ string_of_int(Hashtbl.length(stamps)) ++ " stamps"); *)
  Hashtbl.fold
    (fun _stamp declared result ->
      if declared.name.txt = name then
        (* Log.log("a stamp " ++ Utils.showLocation(declared.scopeLoc) ++ " " ++ string_of_int(l) ++ "," ++ string_of_int(c)); *)
        if locationIsBefore declared.scopeLoc pos then
          match result with
          | None -> Some declared
          | Some current ->
            if
              current.name.loc.loc_start.pos_cnum
              < declared.name.loc.loc_start.pos_cnum
            then Some declared
            else result
        else result
      else (* Log.log("wrong name " ++ declared.name.txt); *)
        result)
    stamps None

let resolveFromStamps ~(env : queryEnv) ~path ~package ~pos =
  match path with
  | Tip name -> Some (env, name)
  | Nested (name, inner) -> (
    (* Log.log("Finding from stamps " ++ name); *)
    match findInScope pos name env.qFile.stamps.modules with
    | None -> None
    | Some declared -> (
      (* Log.log("found it"); *)
      match findInModule ~env declared.item inner with
      | None -> None
      | Some res -> (
        match res with
        | `Local (env, name) -> Some (env, name)
        | `Global (moduleName, fullPath) -> (
          match fileForModule ~package moduleName with
          | None -> None
          | Some file -> resolvePath ~env:(fileEnv file) ~path:fullPath ~package
          ))))

let resolveModuleFromCompilerPath ~env ~package path =
  match fromCompilerPath ~env path with
  | `Global (moduleName, path) -> (
    match fileForModule ~package moduleName with
    | None -> None
    | Some file -> (
      let env = fileEnv file in
      match resolvePath ~env ~package ~path with
      | None -> None
      | Some (env, name) -> (
        match Hashtbl.find_opt env.qExported.modules name with
        | None -> None
        | Some stamp -> (
          match Hashtbl.find_opt env.qFile.stamps.modules stamp with
          | None -> None
          | Some declared -> Some (env, Some declared)))))
  | `Stamp stamp -> (
    match Hashtbl.find_opt env.qFile.stamps.modules stamp with
    | None -> None
    | Some declared -> Some (env, Some declared))
  | `GlobalMod moduleName -> (
    match fileForModule ~package moduleName with
    | None -> None
    | Some file ->
      let env = fileEnv file in
      Some (env, None))
  | `Not_found -> None
  | `Exported (env, name) -> (
    match Hashtbl.find_opt env.qExported.modules name with
    | None -> None
    | Some stamp -> (
      match Hashtbl.find_opt env.qFile.stamps.modules stamp with
      | None -> None
      | Some declared -> Some (env, Some declared)))

let resolveFromCompilerPath ~env ~package path =
  match fromCompilerPath ~env path with
  | `Global (moduleName, path) -> (
    let res =
      match fileForModule ~package moduleName with
      | None -> None
      | Some file ->
        let env = fileEnv file in
        resolvePath ~env ~package ~path
    in
    match res with
    | None -> `Not_found
    | Some (env, name) -> `Exported (env, name))
  | `Stamp stamp -> `Stamp stamp
  | `GlobalMod _ -> `Not_found
  | `Not_found -> `Not_found
  | `Exported (env, name) -> `Exported (env, name)

let rec getSourceUri ~(env : queryEnv) ~package path =
  match path with
  | File (uri, _moduleName) -> uri
  | NotVisible -> env.qFile.uri
  | IncludedModule (path, inner) -> (
    Log.log "INCLUDED MODULE";
    match resolveModuleFromCompilerPath ~env ~package path with
    | None ->
      Log.log "NOT FOUND";
      getSourceUri ~env ~package inner
    | Some (env, _declared) -> env.qFile.uri)
  | ExportedModule (_, inner) -> getSourceUri ~env ~package inner

let exportedForTip ~(env : queryEnv) name tip =
  match tip with
  | Value -> Hashtbl.find_opt env.qExported.values name
  | Field _ | Constructor _ | Type -> Hashtbl.find_opt env.qExported.types name
  | Module -> Hashtbl.find_opt env.qExported.modules name
