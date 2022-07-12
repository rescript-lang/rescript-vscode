let namespacedName namespace name =
  match namespace with
  | None -> name
  | Some namespace -> name ^ "-" ^ namespace

let ( /+ ) = Filename.concat

let getBsPlatformDir rootPath =
  let result =
    ModuleResolution.resolveNodeModulePath ~startPath:rootPath "rescript"
  in
  match result with
  | Some path -> Some path
  | None ->
    let message = "rescript could not be found" in
    Log.log message;
    None

let getLibBs root = Files.ifExists (root /+ "lib" /+ "bs")

let getStdlib base =
  match getBsPlatformDir base with
  | None -> None
  | Some bsPlatformDir -> Some (bsPlatformDir /+ "lib" /+ "ocaml")
