open Typedtree
open SharedTypes
open Infix

let itemsExtent items =
  let open Typedtree in
  match items = [] with
  | true -> Location.none
  | false ->
    let first = List.hd items in
    let last = List.nth items (List.length items - 1) in
    let first, last =
      match
        first.str_loc.loc_start.pos_cnum < last.str_loc.loc_start.pos_cnum
      with
      | true -> (first, last)
      | false -> (last, first)
    in
    {
      loc_ghost = true;
      loc_start = first.str_loc.loc_start;
      loc_end = last.str_loc.loc_end;
    }

let sigItemsExtent items =
  let open Typedtree in
  match items = [] with
  | true -> Location.none
  | false ->
    let first = List.hd items in
    let last = List.nth items (List.length items - 1) in
    {
      Location.loc_ghost = true;
      loc_start = first.sig_loc.loc_start;
      loc_end = last.sig_loc.loc_end;
    }

type env = {
  stamps : stamps;
  processDoc : string -> string list;
  modulePath : visibilityPath;
  scope : Location.t;
}

let addItem ~name ~extent ~stamp ~env ~item attributes exported stamps =
  let declared =
    ProcessAttributes.newDeclared ~item
      ~scope:
        {
          Location.loc_start = extent.Location.loc_end;
          loc_end = env.scope.loc_end;
          loc_ghost = false;
        }
      ~extent ~name ~stamp ~modulePath:env.modulePath ~processDoc:env.processDoc
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
              ( match type_kind with
              | Type_abstract -> (
                match type_manifest with
                | Some {desc = Tconstr (path, args, _)} ->
                  Abstract (Some (path, args))
                | Some {desc = Ttuple items} -> Tuple items
                (* TODO dig *)
                | _ -> Abstract None )
              | Type_open -> Open
              | Type_variant constructors ->
                Variant
                  ( constructors
                  |> List.map
                       (fun {cd_loc; cd_id; cd_args; cd_res; cd_attributes} ->
                         let name = Ident.name cd_id in
                         let stamp = Ident.binding_time cd_id in
                         let item =
                           {
                             stamp;
                             cname = Location.mknoloc name;
                             args =
                               ( match cd_args with
                               | Cstr_tuple args -> args
                               (* TODO(406): constructor record args support *)
                               | Cstr_record _ -> [] )
                               |> List.map (fun t -> (t, Location.none));
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
                             ~stamp
                             (* TODO maybe this needs another child *)
                             ~modulePath:env.modulePath
                             ~processDoc:env.processDoc true cd_attributes
                         in
                         Hashtbl.add env.stamps.constructors stamp declared;
                         item) )
              | Type_record (fields, _) ->
                Record
                  ( fields
                  |> List.map (fun {ld_id; ld_type} ->
                         let astamp = Ident.binding_time ld_id in
                         let name = Ident.name ld_id in
                         {
                           stamp = astamp;
                           fname = Location.mknoloc name;
                           typ = ld_type;
                         }) ) );
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
            ( match typ_kind with
            | Ttype_abstract -> (
              match typ_manifest with
              | Some {ctyp_desc = Ttyp_constr (path, _lident, args)} ->
                Abstract (Some (path, args |> List.map (fun t -> t.ctyp_type)))
              | Some {ctyp_desc = Ttyp_tuple items} ->
                Tuple (items |> List.map (fun t -> t.ctyp_type))
              (* TODO dig *)
              | _ -> Abstract None )
            | Ttype_open -> Open
            | Ttype_variant constructors ->
              Variant
                ( constructors
                |> List.map (fun {cd_id; cd_name = cname; cd_args; cd_res} ->
                       let stamp = Ident.binding_time cd_id in
                       {
                         stamp;
                         cname;
                         args =
                           ( match cd_args with
                           | Cstr_tuple args -> args
                           (* TODO(406) *)
                           | Cstr_record _ -> [] )
                           |> List.map (fun t -> (t.ctyp_type, t.ctyp_loc));
                         res = (cd_res |?>> fun t -> t.ctyp_type);
                       }) )
            | Ttype_record fields ->
              Record
                ( fields
                |> List.map
                     (fun {ld_id; ld_name = fname; ld_type = {ctyp_type}} ->
                       let fstamp = Ident.binding_time ld_id in
                       {stamp = fstamp; fname; typ = ctyp_type}) ) );
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
    | Some d -> env.processDoc d
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
        | Tpat_alias ({pat_desc = Tpat_any}, ident, name) (* let x : t = ... *) ->
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
    ( match maybeType with
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
            ~extent:t.Typedtree.mty_loc ~stamp ~modulePath:NotVisible
            ~processDoc:env.processDoc false []
        in
        Hashtbl.add env.stamps.modules stamp declared ) );
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
    | Some d -> env.processDoc d
  in
  {docstring; exported; topLevel}

let forCmt ~moduleName ~uri processDoc
    ({cmt_modname; cmt_annots} : Cmt_format.cmt_infos) =
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
        processDoc;
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
        processDoc;
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
        processDoc;
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
        processDoc;
        modulePath = File (uri, moduleName);
      }
    in
    let contents = forSignature ~env signature.sig_items in
    {uri; moduleName = cmt_modname; stamps = env.stamps; contents}
  | _ -> SharedTypes.emptyFile moduleName uri
