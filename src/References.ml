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

let locsForPos ~extra pos =
  extra.locations |> List.filter (fun (loc, _l) -> checkPos pos loc)

let locForPos ~extra pos =
  match locsForPos ~extra pos with
  | [
   (loc1, Typed (_, LocalReference _));
   (loc2, Typed (_, GlobalReference ("Js_OO", Tip "unsafe_downgrade", _)));
   ((loc3, _) as l3);
  ]
    when loc1 = loc2 && loc2 = loc3 ->
    (* JSX and compiler combined: *)
    (* ~x becomes Js_OO.unsafe_downgrade(Props)#x *)
    (* heuristic for: [Props, unsafe_downgrade, x], give loc of `x` *)
    Some l3
  | [(loc1, _); ((loc2, _) as l); (loc3, _)] when loc1 = loc2 && loc2 = loc3 ->
    (* JSX with at most one child *)
    (* heuristic for: [makeProps, make, createElement], give the loc of `make` *)
    Some l
  | [(loc1, _); (loc2, _); ((loc3, _) as l); (loc4, _)]
    when loc1 = loc2 && loc2 = loc3 && loc3 = loc4 ->
    (* JSX variadic, e.g. <C> {x} {y} </C> *)
    (* heuristic for: [makeProps, React.null, make, createElementVariadic], give the loc of `make` *)
    Some l
  | l :: _ -> Some l
  | _ -> None

let definedForLoc ~file ~getModule locKind =
  let inner ~file stamp tip =
    match tip with
    | Constructor name -> (
      match Query.getConstructor file stamp name with
      | None -> None
      | Some constructor -> Some ([], `Constructor constructor))
    | Field name -> (
      match Query.getField file stamp name with
      | None -> None
      | Some field -> Some ([], `Field field))
    | _ -> (
      maybeLog
        ("Trying for declared " ^ tipToString tip ^ " " ^ string_of_int stamp
       ^ " in file " ^ Uri2.toString file.uri);
      match Query.declaredForTip ~stamps:file.stamps stamp tip with
      | None -> None
      | Some declared -> Some (declared.docstring, `Declared))
  in
  match locKind with
  | NotFound -> None
  | LocalReference (stamp, tip) | Definition (stamp, tip) ->
    inner ~file stamp tip
  | GlobalReference (moduleName, path, tip) -> (
    maybeLog ("Getting global " ^ moduleName);
    match getModule moduleName with
    | None ->
      Log.log ("Cannot get module " ^ moduleName);
      None
    | Some file -> (
      let env = Query.fileEnv file in
      match Query.resolvePath ~env ~path ~getModule with
      | None ->
        Log.log ("Cannot resolve path " ^ pathToString path);
        None
      | Some (env, name) -> (
        match Query.exportedForTip ~env name tip with
        | None ->
          Log.log
            ("Exported not found for tip " ^ name ^ " > " ^ tipToString tip);
          None
        | Some stamp -> (
          maybeLog ("Getting for " ^ string_of_int stamp ^ " in " ^ name);
          match inner ~file:env.file stamp tip with
          | None ->
            Log.log "could not get defined";
            None
          | Some res ->
            maybeLog "Yes!! got it";
            Some res))))

let alternateDeclared ~file ~pathsForModule ~getUri declared tip =
  match Hashtbl.find_opt pathsForModule file.moduleName with
  | None -> None
  | Some paths -> (
    maybeLog ("paths for " ^ file.moduleName);
    match paths with
    | IntfAndImpl (_, intf, _, impl) -> (
      maybeLog "Have both!!";
      let intfUri = Uri2.fromPath intf in
      let implUri = Uri2.fromPath impl in
      if intfUri = file.uri then
        match getUri implUri with
        | Error e ->
          Log.log e;
          None
        | Ok (file, extra) -> (
          match
            Query.declaredForExportedTip ~stamps:file.stamps
              ~exported:file.contents.exported declared.name.txt tip
          with
          | None -> None
          | Some declared -> Some (file, extra, declared))
      else
        match getUri intfUri with
        | Error e ->
          Log.log e;
          None
        | Ok (file, extra) -> (
          match
            Query.declaredForExportedTip ~stamps:file.stamps
              ~exported:file.contents.exported declared.name.txt tip
          with
          | None -> None
          | Some declared -> Some (file, extra, declared)))
    | _ -> None)

let resolveModuleReference ~file ~getModule (declared : moduleKind declared) =
  match declared.item with
  | Structure _ -> Some (file, Some declared)
  | Ident path -> (
    let env = Query.fileEnv file in
    match Query.fromCompilerPath ~env path with
    | `Not_found -> None
    | `Exported (env, name) -> (
      match Hashtbl.find_opt env.exported.modules name with
      | None -> None
      | Some stamp -> (
        match Hashtbl.find_opt env.file.stamps.modules stamp with
        | None -> None
        | Some md ->
          Some (env.file, Some md)
          (* Some((env.file.uri, validateLoc(md.name.loc, md.extentLoc))) *)))
    | `Global (moduleName, path) -> (
      match getModule moduleName with
      | None -> None
      | Some file -> (
        let env = Query.fileEnv file in
        match Query.resolvePath ~env ~getModule ~path with
        | None -> None
        | Some (env, name) -> (
          match Hashtbl.find_opt env.exported.modules name with
          | None -> None
          | Some stamp -> (
            match Hashtbl.find_opt env.file.stamps.modules stamp with
            | None -> None
            | Some md ->
              Some (env.file, Some md)
              (* Some((env.file.uri, validateLoc(md.name.loc, md.extentLoc))) *)
            ))))
    | `Stamp stamp -> (
      match Hashtbl.find_opt file.stamps.modules stamp with
      | None -> None
      | Some md ->
        Some (file, Some md)
        (* Some((file.uri, validateLoc(md.name.loc, md.extentLoc))) *))
    | `GlobalMod name -> (
      match getModule name with
      | None -> None
      | Some file ->
        (* maybeLog("Congrats, found a global mod"); *)
        Some (file, None))
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

let resolveModuleDefinition ~file ~getModule stamp =
  match Hashtbl.find_opt file.stamps.modules stamp with
  | None -> None
  | Some md -> (
    match resolveModuleReference ~file ~getModule md with
    | None -> None
    | Some (file, declared) ->
      let loc =
        match declared with
        | None -> Utils.topLoc (Uri2.toPath file.uri)
        | Some declared -> validateLoc declared.name.loc declared.extentLoc
      in
      Some (file.uri, loc))

let definition ~file ~getModule stamp tip =
  match tip with
  | Constructor name -> (
    match Query.getConstructor file stamp name with
    | None -> None
    | Some constructor -> Some (file.uri, constructor.cname.loc))
  | Field name -> (
    match Query.getField file stamp name with
    | None -> None
    | Some field -> Some (file.uri, field.fname.loc))
  | Module -> resolveModuleDefinition ~file ~getModule stamp
  | _ -> (
    match Query.declaredForTip ~stamps:file.stamps stamp tip with
    | None -> None
    | Some declared ->
      let loc = validateLoc declared.name.loc declared.extentLoc in
      let env = Query.fileEnv file in
      let uri = Query.getSourceUri ~env ~getModule declared.modulePath in
      maybeLog ("Inner uri " ^ Uri2.toString uri);
      Some (uri, loc))

let orLog message v =
  match v with
  | None ->
    maybeLog message;
    None
  | _ -> v

let definitionForLoc ~pathsForModule ~file ~getUri ~getModule loc =
  match loc with
  | Typed (_, Definition (stamp, tip)) -> (
    maybeLog "Trying to find a defintion for a definition";
    match Query.declaredForTip ~stamps:file.stamps stamp tip with
    | None -> None
    | Some declared ->
      maybeLog "Declared";
      if declared.exported then (
        maybeLog ("exported, looking for alternate " ^ file.moduleName);
        match alternateDeclared ~pathsForModule ~file ~getUri declared tip with
        | None -> None
        | Some (file, _extra, declared) ->
          let loc = validateLoc declared.name.loc declared.extentLoc in
          Some (file.uri, loc))
      else None)
  | Explanation _
  | Typed (_, NotFound)
  | LModule (NotFound | Definition (_, _))
  | TypeDefinition (_, _, _)
  | Constant _ ->
    None
  | TopLevelModule name -> (
    maybeLog ("Toplevel " ^ name);
    let open Infix in
    match
      Hashtbl.find_opt pathsForModule name
      |> orLog "No paths found" |?> getSrc |> orLog "No src found"
    with
    | None -> None
    | Some src -> Some (Uri2.fromPath src, Utils.topLoc src))
  | LModule (LocalReference (stamp, tip))
  | Typed (_, LocalReference (stamp, tip)) ->
    maybeLog ("Local defn " ^ tipToString tip);
    definition ~file ~getModule stamp tip
  | LModule (GlobalReference (moduleName, path, tip))
  | Typed (_, GlobalReference (moduleName, path, tip)) -> (
    maybeLog
      ("Global defn " ^ moduleName ^ " " ^ pathToString path ^ " : "
     ^ tipToString tip);
    match getModule moduleName with
    | None -> None
    | Some file -> (
      let env = Query.fileEnv file in
      match Query.resolvePath ~env ~path ~getModule with
      | None -> None
      | Some (env, name) -> (
        match Query.exportedForTip ~env name tip with
        | None -> None
        | Some stamp ->
          (* oooh wht do I do if the stamp is inside a pseudo-file? *)
          maybeLog ("Got stamp " ^ string_of_int stamp);
          definition ~file:env.file ~getModule stamp tip)))
