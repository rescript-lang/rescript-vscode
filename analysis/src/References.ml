open SharedTypes

let debugReferences = ref true

let maybeLog m = if !debugReferences then Log.log ("[ref] " ^ m)

let checkPos (line, char)
    {Location.loc_start = {pos_lnum; pos_bol; pos_cnum}; loc_end} =
  if line < pos_lnum || (line = pos_lnum && char < pos_cnum - pos_bol) then
    false
  else if
    line > loc_end.pos_lnum
    || (line = loc_end.pos_lnum && char > loc_end.pos_cnum - loc_end.pos_bol)
  then false
  else true

let locItemsForPos ~extra pos =
  extra.locItems |> List.filter (fun {loc; locType = _} -> checkPos pos loc)

let locItemForPos ~extra pos =
  let locItems = locItemsForPos ~extra pos in
  match locItems with
  | [({locType = Typed (_, LocalReference _)} as li1); li3]
    when li1.loc = li3.loc ->
    (* JSX and compiler combined:
       ~x becomes Props#x
       heuristic for: [Props, x], give loc of `x` *)
    Some li3
  | [
   ({locType = Typed (_, LocalReference _)} as li1);
   ({locType = Typed (_, GlobalReference ("Js_OO", Tip "unsafe_downgrade", _))}
   as li2);
   li3;
  ]
  (* For older compiler 9.0 or earlier *)
    when li1.loc = li2.loc && li2.loc = li3.loc ->
    (* JSX and compiler combined:
       ~x becomes Js_OO.unsafe_downgrade(Props)#x
       heuristic for: [Props, unsafe_downgrade, x], give loc of `x` *)
    Some li3
  | [
   {locType = Typed (_, LocalReference (_, Value))};
   ({locType = Typed (_, Definition (_, Value))} as li2);
  ] ->
    (* JSX on type-annotated labeled (~arg:t):
       (~arg:t) becomes Props#arg
       Props has the location range of arg:t
       arg has the location range of arg
       heuristic for: [Props, arg], give loc of `arg` *)
    (* Printf.eprintf "l1 %s\nl2 %s\n"
       (SharedTypes.locationToString _l1)
       (SharedTypes.locationToString l2); *)
    Some li2
  | [li1; li2; li3] when li1.loc = li2.loc && li2.loc = li3.loc ->
    (* JSX with at most one child
       heuristic for: [makeProps, make, createElement], give the loc of `make` *)
    Some li2
  | [li1; li2; li3; li4]
    when li1.loc = li2.loc && li2.loc = li3.loc && li3.loc = li4.loc ->
    (* JSX variadic, e.g. <C> {x} {y} </C>
       heuristic for: [makeProps  , React.null, make, createElementVariadic], give the loc of `make` *)
    Some li3
  | li :: _ -> Some li
  | _ -> None

let declaredForTip ~stamps stamp tip =
  let open Infix in
  match tip with
  | Value ->
    Hashtbl.find_opt stamps.values stamp |?>> fun x -> {x with item = ()}
  | Field _ | Constructor _ | Type ->
    Hashtbl.find_opt stamps.types stamp |?>> fun x -> {x with item = ()}
  | Module ->
    Hashtbl.find_opt stamps.modules stamp |?>> fun x -> {x with item = ()}

let getField file stamp name =
  match Hashtbl.find_opt file.stamps.types stamp with
  | None -> None
  | Some {item = {kind}} -> (
    match kind with
    | Record fields -> fields |> List.find_opt (fun f -> f.fname.txt = name)
    | _ -> None)

let getConstructor file stamp name =
  match Hashtbl.find_opt file.stamps.types stamp with
  | None -> None
  | Some {item = {kind}} -> (
    match kind with
    | Variant constructors -> (
      match
        constructors |> List.find_opt (fun const -> const.cname.txt = name)
      with
      | None -> None
      | Some const -> Some const)
    | _ -> None)

let definedForLoc ~file ~package locKind =
  let inner ~file stamp tip =
    match tip with
    | Constructor name -> (
      match getConstructor file stamp name with
      | None -> None
      | Some constructor -> Some ([], `Constructor constructor))
    | Field name -> (
      match getField file stamp name with
      | None -> None
      | Some field -> Some ([], `Field field))
    | _ -> (
      maybeLog
        ("Trying for declared " ^ tipToString tip ^ " " ^ string_of_int stamp
       ^ " in file " ^ Uri2.toString file.uri);
      match declaredForTip ~stamps:file.stamps stamp tip with
      | None -> None
      | Some declared -> Some (declared.docstring, `Declared))
  in
  match locKind with
  | NotFound -> None
  | LocalReference (stamp, tip) | Definition (stamp, tip) ->
    inner ~file stamp tip
  | GlobalReference (moduleName, path, tip) -> (
    maybeLog ("Getting global " ^ moduleName);
    match ProcessCmt.fileForModule ~package moduleName with
    | None ->
      Log.log ("Cannot get module " ^ moduleName);
      None
    | Some file -> (
      let env = ProcessCmt.fileEnv file in
      match ProcessCmt.resolvePath ~env ~path ~package with
      | None ->
        Log.log ("Cannot resolve path " ^ pathToString path);
        None
      | Some (env, name) -> (
        match ProcessCmt.exportedForTip ~env name tip with
        | None ->
          Log.log
            ("Exported not found for tip " ^ name ^ " > " ^ tipToString tip);
          None
        | Some stamp -> (
          maybeLog ("Getting for " ^ string_of_int stamp ^ " in " ^ name);
          match inner ~file:env.qFile stamp tip with
          | None ->
            Log.log "could not get defined";
            None
          | Some res ->
            maybeLog "Yes!! got it";
            Some res))))

let declaredForExportedTip ~(stamps : stamps) ~(exported : exported) name tip =
  let open Infix in
  match tip with
  | Value ->
    Hashtbl.find_opt exported.values name |?> fun stamp ->
    Hashtbl.find_opt stamps.values stamp |?>> fun x -> {x with item = ()}
  | Field _ | Constructor _ | Type ->
    Hashtbl.find_opt exported.types name |?> fun stamp ->
    Hashtbl.find_opt stamps.types stamp |?>> fun x -> {x with item = ()}
  | Module ->
    Hashtbl.find_opt exported.modules name |?> fun stamp ->
    Hashtbl.find_opt stamps.modules stamp |?>> fun x -> {x with item = ()}

let alternateDeclared ~file ~package declared tip =
  match Hashtbl.find_opt package.TopTypes.pathsForModule file.moduleName with
  | None -> None
  | Some paths -> (
    maybeLog ("paths for " ^ file.moduleName);
    match paths with
    | IntfAndImpl (_, intf, _, impl) -> (
      maybeLog "Have both!!";
      let intfUri = Uri2.fromPath intf in
      let implUri = Uri2.fromPath impl in
      if intfUri = file.uri then
        match ProcessCmt.fileForUri implUri with
        | Error e ->
          Log.log e;
          None
        | Ok (file, extra) -> (
          match
            declaredForExportedTip ~stamps:file.stamps
              ~exported:file.contents.exported declared.name.txt tip
          with
          | None -> None
          | Some declared -> Some (file, extra, declared))
      else
        match ProcessCmt.fileForUri intfUri with
        | Error e ->
          Log.log e;
          None
        | Ok (file, extra) -> (
          match
            declaredForExportedTip ~stamps:file.stamps
              ~exported:file.contents.exported declared.name.txt tip
          with
          | None -> None
          | Some declared -> Some (file, extra, declared)))
    | _ -> None)

let rec resolveModuleReference ~file ~package (declared : moduleKind declared) =
  match declared.item with
  | Structure _ -> Some (file, Some declared)
  | Constraint (moduleItem, _moduleTypeItem) ->
    resolveModuleReference ~file ~package {declared with item = moduleItem}
  | Ident path -> (
    let env = ProcessCmt.fileEnv file in
    match ProcessCmt.fromCompilerPath ~env path with
    | `Not_found -> None
    | `Exported (env, name) -> (
      match Hashtbl.find_opt env.qExported.modules name with
      | None -> None
      | Some stamp -> (
        match Hashtbl.find_opt env.qFile.stamps.modules stamp with
        | None -> None
        | Some md -> Some (env.qFile, Some md)))
    | `Global (moduleName, path) -> (
      match ProcessCmt.fileForModule ~package moduleName with
      | None -> None
      | Some file -> (
        let env = ProcessCmt.fileEnv file in
        match ProcessCmt.resolvePath ~env ~package ~path with
        | None -> None
        | Some (env, name) -> (
          match Hashtbl.find_opt env.qExported.modules name with
          | None -> None
          | Some stamp -> (
            match Hashtbl.find_opt env.qFile.stamps.modules stamp with
            | None -> None
            | Some md -> Some (env.qFile, Some md)))))
    | `Stamp stamp -> (
      match Hashtbl.find_opt file.stamps.modules stamp with
      | None -> None
      | Some md -> Some (file, Some md))
    | `GlobalMod name -> (
      match ProcessCmt.fileForModule ~package name with
      | None -> None
      | Some file -> Some (file, None))
    | _ -> None)

let validateLoc (loc : Location.t) (backup : Location.t) =
  if loc.loc_start.pos_cnum = -1 then
    if backup.loc_start.pos_cnum = -1 then
      {
        Location.loc_ghost = true;
        loc_start = {pos_cnum = 0; pos_lnum = 1; pos_bol = 0; pos_fname = ""};
        loc_end = {pos_cnum = 0; pos_lnum = 1; pos_bol = 0; pos_fname = ""};
      }
    else backup
  else loc

let resolveModuleDefinition ~file ~package stamp =
  match Hashtbl.find_opt file.stamps.modules stamp with
  | None -> None
  | Some md -> (
    match resolveModuleReference ~file ~package md with
    | None -> None
    | Some (file, declared) ->
      let loc =
        match declared with
        | None -> Utils.topLoc (Uri2.toPath file.uri)
        | Some declared -> validateLoc declared.name.loc declared.extentLoc
      in
      Some (file.uri, loc))

let definition ~file ~package stamp tip =
  match tip with
  | Constructor name -> (
    match getConstructor file stamp name with
    | None -> None
    | Some constructor -> Some (file.uri, constructor.cname.loc))
  | Field name -> (
    match getField file stamp name with
    | None -> None
    | Some field -> Some (file.uri, field.fname.loc))
  | Module -> resolveModuleDefinition ~file ~package stamp
  | _ -> (
    match declaredForTip ~stamps:file.stamps stamp tip with
    | None -> None
    | Some declared ->
      let loc = validateLoc declared.name.loc declared.extentLoc in
      let env = ProcessCmt.fileEnv file in
      let uri = ProcessCmt.getSourceUri ~env ~package declared.modulePath in
      maybeLog ("Inner uri " ^ Uri2.toString uri);
      Some (uri, loc))

let orLog message v =
  match v with
  | None ->
    maybeLog message;
    None
  | _ -> v

let definitionForLocItem ~package ~file locItem =
  match locItem.locType with
  | Typed (_, Definition (stamp, tip)) -> (
    maybeLog "Trying to find a defintion for a definition";
    match declaredForTip ~stamps:file.stamps stamp tip with
    | None -> None
    | Some declared ->
      maybeLog "Declared";
      if declared.exported then (
        maybeLog ("exported, looking for alternate " ^ file.moduleName);
        match alternateDeclared ~package ~file declared tip with
        | None -> None
        | Some (file, _extra, declared) ->
          let loc = validateLoc declared.name.loc declared.extentLoc in
          Some (file.uri, loc))
      else None)
  | Typed (_, NotFound)
  | LModule (NotFound | Definition (_, _))
  | TypeDefinition (_, _, _)
  | Constant _ ->
    None
  | TopLevelModule name -> (
    maybeLog ("Toplevel " ^ name);
    let open Infix in
    match
      Hashtbl.find_opt package.pathsForModule name
      |> orLog "No paths found" |?> getSrc |> orLog "No src found"
    with
    | None -> None
    | Some src -> Some (Uri2.fromPath src, Utils.topLoc src))
  | LModule (LocalReference (stamp, tip))
  | Typed (_, LocalReference (stamp, tip)) ->
    maybeLog ("Local defn " ^ tipToString tip);
    definition ~file ~package stamp tip
  | LModule (GlobalReference (moduleName, path, tip))
  | Typed (_, GlobalReference (moduleName, path, tip)) -> (
    maybeLog
      ("Global defn " ^ moduleName ^ " " ^ pathToString path ^ " : "
     ^ tipToString tip);
    match ProcessCmt.fileForModule ~package moduleName with
    | None -> None
    | Some file -> (
      let env = ProcessCmt.fileEnv file in
      match ProcessCmt.resolvePath ~env ~path ~package with
      | None -> None
      | Some (env, name) -> (
        match ProcessCmt.exportedForTip ~env name tip with
        | None -> None
        | Some stamp ->
          (* oooh wht do I do if the stamp is inside a pseudo-file? *)
          maybeLog ("Got stamp " ^ string_of_int stamp);
          definition ~file:env.qFile ~package stamp tip)))

let isVisible (declared : _ SharedTypes.declared) =
  declared.exported
  &&
  let rec loop v =
    match v with
    | File _ -> true
    | NotVisible -> false
    | IncludedModule (_, inner) -> loop inner
    | ExportedModule (_, inner) -> loop inner
  in
  loop declared.modulePath

let rec pathFromVisibility visibilityPath current =
  match visibilityPath with
  | File _ -> Some current
  | IncludedModule (_, inner) -> pathFromVisibility inner current
  | ExportedModule (name, inner) ->
    pathFromVisibility inner (Nested (name, current))
  | NotVisible -> None

let pathFromVisibility visibilityPath tipName =
  pathFromVisibility visibilityPath (Tip tipName)

let forLocalStamp ~package ~file ~extra stamp tip =
  let env = ProcessCmt.fileEnv file in
  let open Infix in
  match
    match tip with
    | Constructor name -> getConstructor file stamp name |?>> fun x -> x.stamp
    | Field name -> getField file stamp name |?>> fun x -> x.stamp
    | _ -> Some stamp
  with
  | None -> []
  | Some localStamp -> (
    match Hashtbl.find_opt extra.internalReferences localStamp with
    | None -> []
    | Some local ->
      maybeLog ("Checking externals: " ^ string_of_int stamp);
      let externals =
        match declaredForTip ~stamps:env.qFile.stamps stamp tip with
        | None -> []
        | Some declared ->
          if isVisible declared then (
            let alternativeReferences =
              match alternateDeclared ~package ~file declared tip with
              | None -> []
              | Some (file, extra, {stamp}) -> (
                match
                  match tip with
                  | Constructor name ->
                    getConstructor file stamp name |?>> fun x -> x.stamp
                  | Field name -> getField file stamp name |?>> fun x -> x.stamp
                  | _ -> Some stamp
                with
                | None -> []
                | Some localStamp -> (
                  match
                    Hashtbl.find_opt extra.internalReferences localStamp
                  with
                  | None -> []
                  | Some local -> [(file.uri, local)]))
              (* if this file has a corresponding interface or implementation file
                 also find the references in that file *)
            in
            match pathFromVisibility declared.modulePath declared.name.txt with
            | None -> []
            | Some path ->
              maybeLog ("Now checking path " ^ pathToString path);
              let thisModuleName = file.moduleName in
              let externals =
                package.localModules
                |> List.filter (fun name -> name <> file.moduleName)
                |> Utils.filterMap (fun name ->
                       match ProcessCmt.fileForModule ~package name with
                       | None -> None
                       | Some file -> (
                         match ProcessCmt.extraForModule ~package name with
                         | None -> None
                         | Some extra -> (
                           match
                             Hashtbl.find_opt extra.externalReferences
                               thisModuleName
                           with
                           | None -> None
                           | Some refs ->
                             let refs =
                               refs
                               |> Utils.filterMap (fun (p, t, l) ->
                                      if p = path && t = tip then Some l
                                      else None)
                             in
                             Some (file.uri, refs))))
              in
              alternativeReferences @ externals)
          else (
            maybeLog "Not visible";
            [])
      in
      (file.uri, local) :: externals)

let allReferencesForLocItem ~package ~file ~extra locItem =
  match locItem.locType with
  | Typed (_, NotFound) | LModule NotFound | TopLevelModule _ | Constant _ -> []
  | TypeDefinition (_, _, stamp) ->
    forLocalStamp ~package ~file ~extra stamp Type
  | Typed (_, (LocalReference (stamp, tip) | Definition (stamp, tip)))
  | LModule (LocalReference (stamp, tip) | Definition (stamp, tip)) ->
    maybeLog
      ("Finding references for " ^ Uri2.toString file.uri ^ " and stamp "
     ^ string_of_int stamp ^ " and tip " ^ tipToString tip);
    forLocalStamp ~package ~file ~extra stamp tip
  | LModule (GlobalReference (moduleName, path, tip))
  | Typed (_, GlobalReference (moduleName, path, tip)) -> (
    match ProcessCmt.fileForModule ~package moduleName with
    | None -> []
    | Some file -> (
      let env = ProcessCmt.fileEnv file in
      match ProcessCmt.resolvePath ~env ~path ~package with
      | None -> []
      | Some (env, name) -> (
        match ProcessCmt.exportedForTip ~env name tip with
        | None -> []
        | Some stamp -> (
          match ProcessCmt.fileForUri env.qFile.uri with
          | Error _ -> []
          | Ok (file, extra) ->
            maybeLog
              ("Finding references for (global) "
              ^ Uri2.toString env.qFile.uri
              ^ " and stamp " ^ string_of_int stamp ^ " and tip "
              ^ tipToString tip);
            forLocalStamp ~package ~file ~extra stamp tip))))
