open Typedtree
open SharedTypes

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
    match matching with [] -> None | first :: _ -> Some first )

let getTypeAtPath ~env path =
  match Query.fromCompilerPath ~env path with
  | `GlobalMod _ -> `Not_found
  | `Global (moduleName, path) -> `Global (moduleName, path)
  | `Not_found -> `Not_found
  | `Exported (env, name) -> (
    match Hashtbl.find_opt env.exported.types name with
    | None -> `Not_found
    | Some stamp -> (
      let declaredType = Hashtbl.find_opt env.file.stamps.types stamp in
      match declaredType with
      | Some declaredType -> `Local declaredType
      | None -> `Not_found ) )
  | `Stamp stamp -> (
    let declaredType = Hashtbl.find_opt env.file.stamps.types stamp in
    match declaredType with
    | Some declaredType -> `Local declaredType
    | None -> `Not_found )

module F (Collector : sig
  val extra : extra

  val file : file

  val scopeExtent : Location.t list ref
end) =
struct
  let extra = Collector.extra

  let maybeAddUse path ident loc tip =
    match findClosestMatchingOpen extra.opens path ident loc with
    | None -> ()
    | Some tracker -> (
      match Query.makeRelativePath tracker.path path with
      | None -> ()
      | Some relpath -> tracker.used <- (relpath, tip, loc) :: tracker.used )

  let addLocation loc ident = extra.locations <- (loc, ident) :: extra.locations

  let addReference stamp loc =
    Hashtbl.replace extra.internalReferences stamp
      ( loc
      ::
      ( match Hashtbl.mem extra.internalReferences stamp with
      | true -> Hashtbl.find extra.internalReferences stamp
      | false -> [] ) )

  let addExternalReference moduleName path tip loc =
    (* TODO need to follow the path, and be able to load the files to follow module references... *)
    Hashtbl.replace extra.externalReferences moduleName
      ( (path, tip, loc)
      ::
      ( match Hashtbl.mem extra.externalReferences moduleName with
      | true -> Hashtbl.find extra.externalReferences moduleName
      | false -> [] ) )

  let env = Query.fileEnv Collector.file

  let getTypeAtPath = getTypeAtPath ~env

  let addForPath path lident loc typ tip =
    maybeAddUse path lident loc tip;
    let identName = Longident.last lident in
    let identLoc = Utils.endOfLocation loc (String.length identName) in
    let locType =
      match Query.fromCompilerPath ~env path with
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
            ( match tip = Type with
            | true -> env.exported.types
            | false -> env.exported.values )
            name
        with
        | Some stamp ->
          addReference stamp identLoc;
          LocalReference (stamp, tip)
        | None -> NotFound )
      | `GlobalMod _ -> NotFound
    in
    addLocation loc (Typed (typ, locType))

  let addForPathParent path loc =
    let locType =
      match Query.fromCompilerPath ~env path with
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
        match Hashtbl.find_opt env.exported.modules name with
        | Some stamp ->
          addReference stamp loc;
          LModule (LocalReference (stamp, Module))
        | None -> LModule NotFound )
    in
    addLocation loc locType

  let addForField recordType item {Asttypes.txt; loc} =
    match (Shared.dig recordType).desc with
    | Tconstr (path, _args, _memo) ->
      let t = getTypeAtPath path in
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
          | None -> NotFound )
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
      let t = getTypeAtPath path in
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
              | None -> NotFound )
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
      let t = getTypeAtPath path in
      let locType =
        match t with
        | `Local {stamp; item = {kind = Variant constructors}} -> (
          match
            constructors |> List.find_opt (fun c -> c.cname.txt = cstr_name)
          with
          | Some {stamp = cstamp} ->
            addReference cstamp nameLoc;
            LocalReference (stamp, Constructor name)
          | None -> NotFound )
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
        ( match top with
        | Some (t, tip) -> addForPath path txt l t tip
        | None -> addForPathParent path l );
        match (path, txt) with
        | Pdot (pinner, _pname, _), Ldot (inner, name) ->
          addForLongident None pinner inner
            (Utils.chopLocationEnd loc (String.length name + 1))
        | Pident _, Lident _ -> ()
        | _ -> () )

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
            ~modulePath:NotVisible
            ~processDoc:(fun x -> [x])
            ~item:val_desc.ctyp_type false val_attributes
        in
        Hashtbl.add Collector.file.stamps.values stamp declared;
        addReference stamp name.loc;
        addLocation name.loc
          (Typed (val_desc.ctyp_type, Definition (stamp, Value))) )
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
            ~modulePath:NotVisible ~extent:pat_loc
            ~processDoc:(fun x -> [x])
            ~item:pat_type false pat_attributes
        in
        Hashtbl.add Collector.file.stamps.values stamp declared;
        addReference stamp name.loc;
        addLocation name.loc (Typed (pat_type, Definition (stamp, Value))) )
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
        ( fields |> Array.to_list
        |> Utils.filterMap (fun (desc, item) ->
               match item with
               | Overridden (loc, _) -> Some (loc, desc, ())
               | _ -> None) )
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
      match cases with [{c_rhs}] -> addScopeExtent c_rhs.exp_loc | _ -> () )
    | _ -> ()

  let leave_expression expression =
    match expression.exp_desc with
    | Texp_let (_isrec, _bindings, _expr) -> popScopeExtent ()
    | Texp_function {cases} -> (
      match cases with [_] -> popScopeExtent () | _ -> () )
    | _ -> ()
end

let forFile ~file =
  let extra = initExtra () in
  let addLocation loc ident =
    extra.locations <- (loc, ident) :: extra.locations
  in
  let addReference stamp loc =
    Hashtbl.replace extra.internalReferences stamp
      ( loc
      ::
      ( match Hashtbl.mem extra.internalReferences stamp with
      | true -> Hashtbl.find extra.internalReferences stamp
      | false -> [] ) )
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
      addLocation d.name.loc (TypeDefinition (d.name.txt, d.item.Type.decl, stamp));
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

let forItems ~file items parts =
  let extra = forFile ~file in
  let extent = ProcessCmt.itemsExtent items in
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

let forCmt ~file ({cmt_annots} : Cmt_format.cmt_infos) =
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
  | Partial_interface _ | Interface _ ->
    (** TODO actually process signature items *)
    forItems ~file [] [||]
  | _ -> forItems ~file [] [||]
