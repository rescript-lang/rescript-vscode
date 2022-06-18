let namespacedName namespace name =
  match namespace with
  | None -> name
  | Some namespace -> name ^ "-" ^ namespace

let ( /+ ) = Filename.concat

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
  | Some path -> Some path
  | None ->
    let message = "bs-platform could not be found" in
    Log.log message;
    None

let getLibBs root = Files.ifExists (root /+ "lib" /+ "bs")

let getStdlib base =
  match getBsPlatformDir base with
  | None -> None
  | Some bsPlatformDir -> Some (bsPlatformDir /+ "lib" /+ "ocaml")
