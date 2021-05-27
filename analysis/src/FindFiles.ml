let ifDebug debug name fn v =
  if debug then Log.log (name ^ ": " ^ fn v);
  v

let ( /+ ) = Filename.concat

(* Returns a list of paths, relative to the provided `base` *)
let getSourceDirectories ~includeDev ~baseDir config =
  let open Infix in
  let rec handleItem current item =
    match item with
    | Json.Array contents ->
      List.map (handleItem current) contents |> List.concat
    | Json.String text -> [current /+ text]
    | Json.Object _ -> (
      let dir =
        Json.get "dir" item |?> Json.string |? "Must specify directory"
      in
      let typ =
        if includeDev then "lib"
        else item |> Json.get "type" |?> Json.string |? "lib"
      in
      if typ = "dev" then []
      else
        match item |> Json.get "subdirs" with
        | None | Some Json.False -> [current /+ dir]
        | Some Json.True ->
          Files.collectDirs (baseDir /+ current /+ dir)
          |> List.filter (fun name -> name <> Filename.current_dir_name)
          |> List.map (Files.relpath baseDir)
        | Some item -> (current /+ dir) :: handleItem (current /+ dir) item)
    | _ -> failwith "Invalid subdirs entry"
  in
  match config |> Json.get "sources" with
  | None -> []
  | Some item -> handleItem "" item

let isCompiledFile name =
  Filename.check_suffix name ".cmt" || Filename.check_suffix name ".cmti"

let isImplementation name =
  Filename.check_suffix name ".re"
  || Filename.check_suffix name ".res"
  || Filename.check_suffix name ".ml"

let isInterface name =
  Filename.check_suffix name ".rei"
  || Filename.check_suffix name ".resi"
  || Filename.check_suffix name ".mli"

let isSourceFile name = isImplementation name || isInterface name

let compiledNameSpace name =
  String.split_on_char '-' name
  |> List.map String.capitalize_ascii
  |> String.concat ""
  (* Remove underscores??? Whyyy bucklescript, whyyyy *)
  |> String.split_on_char '_'
  |> String.concat ""

let compiledBaseName ~namespace name =
  Filename.chop_extension name
  ^ match namespace with None -> "" | Some n -> "-" ^ compiledNameSpace n

let getName x =
  Filename.basename x |> Filename.chop_extension |> String.capitalize_ascii

let filterDuplicates cmts =
  (* Remove .cmt's that have .cmti's *)
  let intfs = Hashtbl.create 100 in
  cmts
  |> List.iter (fun path ->
         if
           Filename.check_suffix path ".rei"
           || Filename.check_suffix path ".mli"
           || Filename.check_suffix path ".cmti"
         then Hashtbl.add intfs (getName path) true);
  cmts
  |> List.filter (fun path ->
         not
           ((Filename.check_suffix path ".re"
            || Filename.check_suffix path ".ml"
            || Filename.check_suffix path ".cmt")
           && Hashtbl.mem intfs (getName path)))

let nameSpaceToName n =
  n
  |> Str.split (Str.regexp "[-/@]")
  |> List.map String.capitalize_ascii
  |> String.concat ""

let getNamespace config =
  let ns = Json.get "namespace" config in
  let open Infix in
  let isNamespaced =
    ns |?> Json.bool |? (ns |?> Json.string |?> (fun _ -> Some true) |? false)
  in
  if isNamespaced then
    ns |?> Json.string
    |?? (Json.get "name" config |?> Json.string)
    |! "name is required if namespace is true" |> nameSpaceToName
    |> fun s -> Some s
  else None

let collectFiles directory =
  let allFiles = Files.readDirectory directory in
  let compileds = allFiles |> List.filter isCompiledFile |> filterDuplicates in
  let sources = allFiles |> List.filter isSourceFile |> filterDuplicates in
  compileds
  |> Utils.filterMap (fun path ->
         let modName = getName path in
         let cmt = directory /+ path in
         let resOpt =
           Utils.find
             (fun name ->
               if getName name = modName then Some (directory /+ name) else None)
             sources
         in
         match resOpt with
         | None -> None
         | Some res -> Some (modName, SharedTypes.Impl {cmt; res}))

(* returns a list of (absolute path to cmt(i), relative path from base to source file) *)
let findProjectFiles ~namespace ~path ~sourceDirectories ~libBs =
  let files =
    sourceDirectories
    |> List.map (Filename.concat path)
    |> ifDebug true "Source directories" (String.concat " - ")
    |> List.map (fun name -> Files.collect name isSourceFile)
    |> List.concat |> Utils.dedup
    |> ifDebug true "Source files found" (String.concat " : ")
  in

  let interfaces = Hashtbl.create 100 in
  files
  |> List.iter (fun path ->
         if isInterface path then (
           Log.log ("Adding intf " ^ path);
           Hashtbl.replace interfaces (getName path) path));

  let normals =
    files
    |> Utils.filterMap (fun file ->
           if isImplementation file then (
             let moduleName = getName file in
             let resi = Hashtbl.find_opt interfaces moduleName in
             Hashtbl.remove interfaces moduleName;
             let base = compiledBaseName ~namespace (Files.relpath path file) in
             match resi with
             | Some resi ->
               let cmti = (libBs /+ base) ^ ".cmti" in
               let cmt = (libBs /+ base) ^ ".cmt" in
               if Files.exists cmti then
                 if Files.exists cmt then
                   (* Log.log("Intf and impl " ++ cmti ++ " " ++ cmt) *)
                   Some
                     ( moduleName,
                       SharedTypes.IntfAndImpl {cmti; resi; cmt; res = file} )
                 else None
               else (
                 (* Log.log("Just intf " ++ cmti) *)
                 Log.log ("Bad source file (no cmt/cmti/cmi) " ^ (libBs /+ base));
                 None)
             | None ->
               let cmt = (libBs /+ base) ^ ".cmt" in
               if Files.exists cmt then Some (moduleName, Impl {cmt; res = file})
               else (
                 Log.log ("Bad source file (no cmt/cmi) " ^ (libBs /+ base));
                 None))
           else None)
  in
  let result =
    normals
    |> List.map (fun (name, paths) ->
           match namespace with
           | None -> (name, paths)
           | Some namespace -> (name ^ "-" ^ namespace, paths))
  in
  match namespace with
  | None -> result
  | Some namespace ->
    let moduleName = nameSpaceToName namespace in
    let cmt = (libBs /+ namespace) ^ ".cmt" in
    Log.log ("adding namespace " ^ namespace ^ " : " ^ moduleName ^ " : " ^ cmt);
    (moduleName, Namespace {cmt}) :: result

let findDependencyFiles base config =
  let open Infix in
  let deps =
    config |> Json.get "bs-dependencies" |?> Json.array |? []
    |> optMap Json.string
  in
  let devDeps =
    config
    |> Json.get "bs-dev-dependencies"
    |?> Json.array |? [] |> optMap Json.string
  in
  let deps = deps @ devDeps in
  Log.log ("Deps " ^ String.concat ", " deps);
  let depFiles =
    deps
    |> List.map (fun name ->
           let result =
             ModuleResolution.resolveNodeModulePath ~startPath:base name
             |?> fun path ->
             let innerPath = path /+ "bsconfig.json" in
             Log.log ("Dep loc " ^ innerPath);
             match Files.readFile innerPath with
             | Some text -> (
               let inner = Json.parse text in
               let namespace = getNamespace inner in
               let sourceDirectories =
                 getSourceDirectories ~includeDev:false ~baseDir:path inner
               in
               match BuildSystem.getLibBs path with
               | None -> None
               | Some libBs ->
                 Log.log ("Compiled base: " ^ libBs);
                 let compiledDirectories =
                   sourceDirectories |> List.map (Filename.concat libBs)
                 in
                 let compiledDirectories =
                   match namespace with
                   | None -> compiledDirectories
                   | Some _ -> libBs :: compiledDirectories
                 in
                 let projectFiles =
                   findProjectFiles ~namespace ~path ~sourceDirectories ~libBs
                 in
                 Some (compiledDirectories, projectFiles))
             | None -> None
           in
           match result with
           | Some (files, directories) -> (files, directories)
           | None ->
             Log.log ("Skipping nonexistent dependency: " ^ name);
             ([], []))
  in
  match BuildSystem.getStdlib base with
  | Error e -> Error e
  | Ok stdlibDirectory ->
    let compiledDirectories, projectFiles =
      let files, directories = List.split depFiles in
      (List.concat files, List.concat directories)
    in
    let allFiles = projectFiles @ collectFiles stdlibDirectory in
    Ok (compiledDirectories, allFiles)
