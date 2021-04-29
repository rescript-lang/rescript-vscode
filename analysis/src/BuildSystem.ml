let namespacedName namespace name =
  match namespace with None -> name | Some namespace -> name ^ "-" ^ namespace

let getBsPlatformDir rootPath =
  let result =
    ModuleResolution.resolveNodeModulePath ~startPath:rootPath "rescript"
  in
  let result =
    if result = None then
      ModuleResolution.resolveNodeModulePath ~startPath:rootPath "bs-platform"
    else result
  in
  match result with
  | Some path -> Ok path
  | None ->
    let message = "bs-platform could not be found" in
    Log.log message;
    Error message

let getCompiledBase root =
  Files.ifExists (Filename.concat (Filename.concat root "lib") "bs")

let getStdlib base =
  match getBsPlatformDir base with
  | Error e -> Error e
  | Ok bsPlatformDir ->
    Ok (Filename.concat (Filename.concat bsPlatformDir "lib") "ocaml")
