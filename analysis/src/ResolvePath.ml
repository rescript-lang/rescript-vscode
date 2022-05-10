open SharedTypes

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
      match ProcessCmt.fileForModule ~package moduleName with
      | None -> None
      | Some file ->
        resolvePath ~env:(QueryEnv.fromFile file) ~path:fullPath ~package))

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

let resolveModuleFromCompilerPath ~env ~package path =
  match fromCompilerPath ~env path with
  | `Global (moduleName, path) -> (
    match ProcessCmt.fileForModule ~package moduleName with
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
  match fromCompilerPath ~env path with
  | `Global (moduleName, path) -> (
    let res =
      match ProcessCmt.fileForModule ~package moduleName with
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
