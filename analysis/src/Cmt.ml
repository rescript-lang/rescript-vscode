open SharedTypes

let fullForCmt ~moduleName ~package ~uri cmt =
  match Shared.tryReadCmt cmt with
  | None -> None
  | Some infos ->
    let file = ProcessCmt.fileForCmtInfos ~moduleName ~uri infos in
    let extra = ProcessExtra.getExtra ~file ~infos in
    Some {file; extra; package}

let fullFromUri ~uri =
  let path = Uri.toPath uri in
  match Packages.getPackage ~uri with
  | None -> None
  | Some package -> (
    let moduleName =
      BuildSystem.namespacedName package.namespace (FindFiles.getName path)
    in
    match Hashtbl.find_opt package.pathsForModule moduleName with
    | Some paths ->
      let cmt = getCmtPath ~uri paths in
      fullForCmt ~moduleName ~package ~uri cmt
    | None ->
      prerr_endline ("can't find module " ^ moduleName);
      None)

let fullsFromModule ~package ~moduleName =
  if Hashtbl.mem package.pathsForModule moduleName then
    let paths = Hashtbl.find package.pathsForModule moduleName in
    let uris = getUris paths in
    uris |> List.filter_map (fun uri -> fullFromUri ~uri)
  else []

let loadFullCmtFromPath ~path =
  let uri = Uri.fromPath path in
  let full = fullFromUri ~uri in
  match full with
  | None -> None
  | Some full ->
    (* Turn on uncurried for the outcome printer *)
    if full.package.uncurried then Config.uncurried := Uncurried;
    Some full
