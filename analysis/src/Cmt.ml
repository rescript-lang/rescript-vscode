open SharedTypes

let fromUri ~uri =
  let path = Uri2.toPath uri in
  match Packages.getPackage ~uri with
  | None -> None
  | Some package -> (
    let moduleName =
      BuildSystem.namespacedName package.namespace (FindFiles.getName path)
    in
    match Hashtbl.find_opt package.pathsForModule moduleName with
    | Some paths ->
      let cmt = getCmtPath ~uri paths in
      ProcessCmt.fullForCmt ~moduleName ~package ~uri cmt
    | None ->
      prerr_endline ("can't find module " ^ moduleName);
      None)

let fromModule ~package modname =
  if Hashtbl.mem package.pathsForModule modname then
    let paths = Hashtbl.find package.pathsForModule modname in
    let uri = getUri paths in
    fromUri ~uri
  else None

let fromPath ~path =
  let uri = Uri2.fromPath path in
  fromUri ~uri

let resolveModuleFromCompilerPath ~env ~package path =
  match ProcessCmt.fromCompilerPath ~env path with
  | `Global (moduleName, path) -> (
    match ProcessCmt.fileForModule ~package moduleName with
    | None -> None
    | Some file -> (
      let env = QueryEnv.fromFile file in
      match ProcessCmt.resolvePath ~env ~package ~path with
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
    match ProcessCmt.fileForModule ~package moduleName with
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
  match ProcessCmt.fromCompilerPath ~env path with
  | `Global (moduleName, path) -> (
    let res =
      match ProcessCmt.fileForModule ~package moduleName with
      | None -> None
      | Some file ->
        let env = QueryEnv.fromFile file in
        ProcessCmt.resolvePath ~env ~package ~path
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
