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
      let cmt = SharedTypes.getCmtPath ~uri paths in
      ProcessCmt.fullForCmt ~moduleName ~package ~uri cmt
    | None ->
      prerr_endline ("can't find module " ^ moduleName);
      None)

let fromModule ~package modname =
  if Hashtbl.mem package.pathsForModule modname then
    let paths = Hashtbl.find package.pathsForModule modname in
    let uri = SharedTypes.getUri paths in
    fromUri ~uri
  else None

let fromPath ~path =
  let uri = Uri2.fromPath path in
  fromUri ~uri
