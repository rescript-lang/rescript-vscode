open SharedTypes

type queryEnv = {file : file; exported : exported}

let fileEnv file = {file; exported = file.contents.exported}

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
      else
        (* Log.log("wrong name " ++ declared.name.txt); *)
        result
    )
    stamps None

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

let rec resolvePathInner ~env ~path =
  match path with
  | Tip name -> Some (`Local (env, name))
  | Nested (subName, subPath) -> (
    match Hashtbl.find_opt env.exported.modules subName with
    | None -> None
    | Some stamp -> (
      match Hashtbl.find_opt env.file.stamps.modules stamp with
      | None -> None
      | Some {item = kind} -> findInModule ~env kind subPath ) )

and findInModule ~env kind path =
  match kind with
  | Structure {exported} -> resolvePathInner ~env:{env with exported} ~path
  | Ident modulePath -> (
    let stamp, moduleName, fullPath = joinPaths modulePath path in
    if stamp = 0 then Some (`Global (moduleName, fullPath))
    else
      match Hashtbl.find_opt env.file.stamps.modules stamp with
      | None -> None
      | Some {item = kind} -> findInModule ~env kind fullPath )

(* let rec findSubModule = (~env, ~getModule) *)

let rec resolvePath ~env ~path ~getModule =
  match resolvePathInner ~env ~path with
  | None -> None
  | Some result -> (
    match result with
    | `Local (env, name) -> Some (env, name)
    | `Global (moduleName, fullPath) -> (
      match getModule moduleName with
      | None -> None
      | Some file -> resolvePath ~env:(fileEnv file) ~path:fullPath ~getModule )
    )

let resolveFromStamps ~env ~path ~getModule ~pos =
  match path with
  | Tip name -> Some (env, name)
  | Nested (name, inner) -> (
    (* Log.log("Finding from stamps " ++ name); *)
    match findInScope pos name env.file.stamps.modules with
    | None -> None
    | Some declared -> (
      (* Log.log("found it"); *)
      match findInModule ~env declared.item inner with
      | None -> None
      | Some res -> (
        match res with
        | `Local (env, name) -> Some (env, name)
        | `Global (moduleName, fullPath) -> (
          match getModule moduleName with
          | None -> None
          | Some file ->
            resolvePath ~env:(fileEnv file) ~path:fullPath ~getModule ) ) ) )

open Infix

let fromCompilerPath ~env path =
  match makePath path with
  | `Stamp stamp -> `Stamp stamp
  | `Path (0, moduleName, path) -> `Global (moduleName, path)
  | `GlobalMod name -> `GlobalMod name
  | `Path (stamp, _moduleName, path) -> (
    let res =
      match Hashtbl.find_opt env.file.stamps.modules stamp with
      | None -> None
      | Some {item = kind} -> findInModule ~env kind path
    in
    match res with
    | None -> `Not_found
    | Some (`Local (env, name)) -> `Exported (env, name)
    | Some (`Global (moduleName, fullPath)) -> `Global (moduleName, fullPath) )

let resolveModuleFromCompilerPath ~env ~getModule path =
  match fromCompilerPath ~env path with
  | `Global (moduleName, path) -> (
    match getModule moduleName with
    | None -> None
    | Some file -> (
      let env = fileEnv file in
      match resolvePath ~env ~getModule ~path with
      | None -> None
      | Some (env, name) -> (
        match Hashtbl.find_opt env.exported.modules name with
        | None -> None
        | Some stamp -> (
          match Hashtbl.find_opt env.file.stamps.modules stamp with
          | None -> None
          | Some declared -> Some (env, Some declared) ) ) ) )
  | `Stamp stamp -> (
    match Hashtbl.find_opt env.file.stamps.modules stamp with
    | None -> None
    | Some declared -> Some (env, Some declared) )
  | `GlobalMod moduleName -> (
    match getModule moduleName with
    | None -> None
    | Some file ->
      let env = fileEnv file in
      Some (env, None) )
  | `Not_found -> None
  | `Exported (env, name) -> (
    match Hashtbl.find_opt env.exported.modules name with
    | None -> None
    | Some stamp -> (
      match Hashtbl.find_opt env.file.stamps.modules stamp with
      | None -> None
      | Some declared -> Some (env, Some declared) ) )

let resolveFromCompilerPath ~env ~getModule path =
  match fromCompilerPath ~env path with
  | `Global (moduleName, path) -> (
    let res =
      match getModule moduleName with
      | None -> None
      | Some file ->
        let env = fileEnv file in
        resolvePath ~env ~getModule ~path
    in
    match res with
    | None -> `Not_found
    | Some (env, name) -> `Exported (env, name) )
  | `Stamp stamp -> `Stamp stamp
  | `GlobalMod _ -> `Not_found
  | `Not_found -> `Not_found
  | `Exported (env, name) -> `Exported (env, name)

let declaredForExportedTip ~(stamps : stamps) ~(exported : exported) name tip =
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

let declaredForTip ~stamps stamp tip =
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
    | _ -> None )

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
      | Some const -> Some const )
    | _ -> None )

let exportedForTip ~env name tip =
  match tip with
  | Value -> Hashtbl.find_opt env.exported.values name
  | Field _ | Constructor _ | Type -> Hashtbl.find_opt env.exported.types name
  | Module -> Hashtbl.find_opt env.exported.modules name

let rec getSourceUri ~env ~getModule path =
  match path with
  | File (uri, _moduleName) -> uri
  | NotVisible -> env.file.uri
  | IncludedModule (path, inner) -> (
    Log.log "INCLUDED MODULE";
    match resolveModuleFromCompilerPath ~env ~getModule path with
    | None ->
      Log.log "NOT FOUND";
      getSourceUri ~env ~getModule inner
    | Some (env, _declared) -> env.file.uri )
  | ExportedModule (_, inner) -> getSourceUri ~env ~getModule inner
