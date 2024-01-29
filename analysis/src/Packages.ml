open SharedTypes

(* Creates the `pathsForModule` hashtbl, which maps a `moduleName` to it's `paths` (the ml/re, mli/rei, cmt, and cmti files) *)
let makePathsForModule ~projectFilesAndPaths ~dependenciesFilesAndPaths =
  let pathsForModule = Hashtbl.create 30 in
  dependenciesFilesAndPaths
  |> List.iter (fun (modName, paths) ->
         Hashtbl.replace pathsForModule modName paths);
  projectFilesAndPaths
  |> List.iter (fun (modName, paths) ->
         Hashtbl.replace pathsForModule modName paths);
  pathsForModule

let getReScriptVersion () =
  (* TODO: Include patch stuff when needed *)
  let defaultVersion = (11, 0) in
  try
    let value = Sys.getenv "RESCRIPT_VERSION" in
    let version =
      match value |> String.split_on_char '.' with
      | major :: minor :: _rest -> (
        match (int_of_string_opt major, int_of_string_opt minor) with
        | Some major, Some minor -> (major, minor)
        | _ -> defaultVersion)
      | _ -> defaultVersion
    in
    version
  with Not_found -> defaultVersion

let newBsPackage ~rootPath =
  let rescriptJson = Filename.concat rootPath "rescript.json" in
  let bsconfigJson = Filename.concat rootPath "bsconfig.json" in

  let parseRaw raw =
    let libBs =
      match !Cfg.isDocGenFromCompiler with
      | true -> BuildSystem.getStdlib rootPath
      | false -> BuildSystem.getLibBs rootPath
    in
    match Json.parse raw with
    | Some config -> (
      match FindFiles.findDependencyFiles rootPath config with
      | None -> None
      | Some (dependencyDirectories, dependenciesFilesAndPaths) -> (
        match libBs with
        | None -> None
        | Some libBs ->
          Some
            (let namespace = FindFiles.getNamespace config in
             let rescriptVersion = getReScriptVersion () in
             let suffix =
               match config |> Json.get "suffix" with
               | Some (String suffix) -> suffix
               | _ -> ".js"
             in
             let uncurried =
               let ns = config |> Json.get "uncurried" in
               match (rescriptVersion, ns) with
               | (major, _), None when major >= 11 -> Some true
               | _, ns -> Option.bind ns Json.bool
             in
             let uncurried = uncurried = Some true in
             let sourceDirectories =
               FindFiles.getSourceDirectories ~includeDev:true ~baseDir:rootPath
                 config
             in
             let projectFilesAndPaths =
               FindFiles.findProjectFiles
                 ~public:(FindFiles.getPublic config)
                 ~namespace ~path:rootPath ~sourceDirectories ~libBs
             in
             projectFilesAndPaths
             |> List.iter (fun (_name, paths) -> Log.log (showPaths paths));
             let pathsForModule =
               makePathsForModule ~projectFilesAndPaths
                 ~dependenciesFilesAndPaths
             in
             let opens_from_namespace =
               match namespace with
               | None -> []
               | Some namespace ->
                 let cmt = Filename.concat libBs namespace ^ ".cmt" in
                 Log.log
                   ("############ Namespaced as " ^ namespace ^ " at " ^ cmt);
                 Hashtbl.add pathsForModule namespace (Namespace {cmt});
                 let path = [FindFiles.nameSpaceToName namespace] in
                 [path]
             in
             Log.log
               ("Dependency dirs: "
               ^ String.concat " "
                   (dependencyDirectories |> List.map Utils.dumpPath));
             let opens_from_bsc_flags =
               let bind f x = Option.bind x f in
               match Json.get "bsc-flags" config |> bind Json.array with
               | Some l ->
                 List.fold_left
                   (fun opens item ->
                     match item |> Json.string with
                     | None -> opens
                     | Some s -> (
                       let parts = String.split_on_char ' ' s in
                       match parts with
                       | "-open" :: name :: _ ->
                         let path = name |> String.split_on_char '.' in
                         path :: opens
                       | _ -> opens))
                   [] l
               | None -> []
             in
             let opens =
               [
                 (if uncurried then "PervasivesU" else "Pervasives");
                 "JsxModules";
               ]
               :: opens_from_namespace
               |> List.rev_append opens_from_bsc_flags
               |> List.map (fun path -> path @ ["place holder"])
             in
             Log.log
               ("Opens from ReScript config file: "
               ^ (opens |> List.map pathToString |> String.concat " "));
             {
               suffix;
               rescriptVersion;
               rootPath;
               projectFiles =
                 projectFilesAndPaths |> List.map fst |> FileSet.of_list;
               dependenciesFiles =
                 dependenciesFilesAndPaths |> List.map fst |> FileSet.of_list;
               pathsForModule;
               opens;
               namespace;
               builtInCompletionModules =
                 (if
                    opens_from_bsc_flags
                    |> List.find_opt (fun opn ->
                           match opn with
                           | ["RescriptCore"] -> true
                           | _ -> false)
                    |> Option.is_some
                  then
                    {
                      arrayModulePath = ["Array"];
                      optionModulePath = ["Option"];
                      stringModulePath = ["String"];
                      intModulePath = ["Int"];
                      floatModulePath = ["Float"];
                      promiseModulePath = ["Promise"];
                      listModulePath = ["List"];
                      resultModulePath = ["Result"];
                      exnModulePath = ["Exn"];
                      regexpModulePath = ["RegExp"];
                    }
                  else if
                    opens_from_bsc_flags
                    |> List.find_opt (fun opn ->
                           match opn with
                           | ["Belt"] -> true
                           | _ -> false)
                    |> Option.is_some
                  then
                    {
                      arrayModulePath = ["Array"];
                      optionModulePath = ["Option"];
                      stringModulePath = ["Js"; "String2"];
                      intModulePath = ["Int"];
                      floatModulePath = ["Float"];
                      promiseModulePath = ["Js"; "Promise"];
                      listModulePath = ["List"];
                      resultModulePath = ["Result"];
                      exnModulePath = ["Js"; "Exn"];
                      regexpModulePath = ["Js"; "Re"];
                    }
                  else
                    {
                      arrayModulePath = ["Js"; "Array2"];
                      optionModulePath = ["Belt"; "Option"];
                      stringModulePath = ["Js"; "String2"];
                      intModulePath = ["Belt"; "Int"];
                      floatModulePath = ["Belt"; "Float"];
                      promiseModulePath = ["Js"; "Promise"];
                      listModulePath = ["Belt"; "List"];
                      resultModulePath = ["Belt"; "Result"];
                      exnModulePath = ["Js"; "Exn"];
                      regexpModulePath = ["Js"; "Re"];
                    });
               uncurried;
             })))
    | None -> None
  in

  match Files.readFile rescriptJson with
  | Some raw -> parseRaw raw
  | None -> (
    Log.log ("Unable to read " ^ rescriptJson);
    match Files.readFile bsconfigJson with
    | Some raw -> parseRaw raw
    | None ->
      Log.log ("Unable to read " ^ bsconfigJson);
      None)

let findRoot ~uri packagesByRoot =
  let path = Uri.toPath uri in
  let rec loop path =
    if path = "/" then None
    else if Hashtbl.mem packagesByRoot path then Some (`Root path)
    else if
      Files.exists (Filename.concat path "rescript.json")
      || Files.exists (Filename.concat path "bsconfig.json")
    then Some (`Bs path)
    else
      let parent = Filename.dirname path in
      if parent = path then (* reached root *) None else loop parent
  in
  loop (Filename.dirname path)

let getPackage ~uri =
  let open SharedTypes in
  if Hashtbl.mem state.rootForUri uri then
    Some (Hashtbl.find state.packagesByRoot (Hashtbl.find state.rootForUri uri))
  else
    match findRoot ~uri state.packagesByRoot with
    | None ->
      Log.log "No root directory found";
      None
    | Some (`Root rootPath) ->
      Hashtbl.replace state.rootForUri uri rootPath;
      Some
        (Hashtbl.find state.packagesByRoot (Hashtbl.find state.rootForUri uri))
    | Some (`Bs rootPath) -> (
      match newBsPackage ~rootPath with
      | None -> None
      | Some package ->
        Hashtbl.replace state.rootForUri uri package.rootPath;
        Hashtbl.replace state.packagesByRoot package.rootPath package;
        Some package)
