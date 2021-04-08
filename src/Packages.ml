open Infix
open TopTypes

let escapePreprocessingFlags flag =
  (* ppx escaping not supported on windows yet *)
  if Sys.os_type = "Win32" then flag
  else
    let parts = Utils.split_on_char ' ' flag in
    match parts with
    | (("-ppx" | "-pp") as flag) :: rest ->
      flag ^ " " ^ Utils.maybeQuoteFilename (String.concat " " rest)
    | _ -> flag

(* Creates the `pathsForModule` hashtbl, which maps a `moduleName` to it's `paths` (the ml/re, mli/rei, cmt, and cmti files) *)
let makePathsForModule (localModules : (string * SharedTypes.paths) list)
    (dependencyModules : (string * SharedTypes.paths) list) =
  let pathsForModule = Hashtbl.create 30 in
  dependencyModules
  |> List.iter (fun (modName, paths) -> Hashtbl.replace pathsForModule modName paths);
  localModules
  |> List.iter (fun (modName, paths) -> Hashtbl.replace pathsForModule modName paths);
  pathsForModule

let newBsPackage rootPath =
  match Files.readFileResult (rootPath /+ "bsconfig.json") with
  | Error e -> Error e
  | Ok raw -> (
    let config = Json.parse raw in
    Log.log {|📣 📣 NEW BSB PACKAGE 📣 📣|};
    (* failwith("Wat"); *)
    Log.log ("- location: " ^ rootPath);
    let compiledBase = BuildSystem.getCompiledBase rootPath in
    match FindFiles.findDependencyFiles ~debug:true rootPath config with
    | Error e -> Error e
    | Ok (dependencyDirectories, dependencyModules) -> (
      match
        compiledBase
        |> RResult.orError
             "You need to run bsb first so that rescript-editor-support can \
              access the compiled artifacts.\n\
              Once you've run bsb, restart the language server."
      with
      | Error e -> Error e
      | Ok compiledBase ->
        Ok
          (let namespace = FindFiles.getNamespace config in
           let localSourceDirs =
             FindFiles.getSourceDirectories ~includeDev:true rootPath config
           in
           Log.log
             ("Got source directories " ^ String.concat " - " localSourceDirs);
           let localModules =
             FindFiles.findProjectFiles ~debug:true namespace rootPath
               localSourceDirs compiledBase
           (*
             |> List.map(((name, paths)) => (switch (namespace) {
               | None => name
               | Some(n) => name ++ "-" ++ n }, paths)); *)
           in
           Log.log
             ( "-- All local modules found: "
             ^ string_of_int (List.length localModules) );
           localModules
           |> List.iter (fun (name, paths) ->
                Log.log name;
                match paths with
                | SharedTypes.Impl (cmt, _) -> Log.log ("impl " ^ cmt)
                | Intf (cmi, _) -> Log.log ("intf " ^ cmi)
                | _ -> Log.log "Both");
           let pathsForModule =
             makePathsForModule localModules dependencyModules
           in
           let opens =
             match namespace with
             | None -> []
             | Some namespace ->
               let cmt = (compiledBase /+ namespace) ^ ".cmt" in
               Log.log ("############ Namespaced as " ^ namespace ^ " at " ^ cmt);
               Hashtbl.add pathsForModule namespace (Impl (cmt, None));
               [FindFiles.nameSpaceToName namespace]
           in
           Log.log ("Dependency dirs " ^ String.concat " " dependencyDirectories);
           let opens =
             let flags =
               MerlinFile.getFlags rootPath
               |> RResult.withDefault [""]
               |> List.map escapePreprocessingFlags
             in
             let opens =
               List.fold_left
                 (fun opens item ->
                   let parts = Utils.split_on_char ' ' item in
                   let rec loop items =
                     match items with
                     | "-open" :: name :: rest -> name :: loop rest
                     | _ :: rest -> loop rest
                     | [] -> []
                   in
                   opens @ loop parts)
                 opens flags
             in
             opens
           in
           let interModuleDependencies =
             Hashtbl.create (List.length localModules)
           in
           {
             rootPath;
             localModules = localModules |> List.map fst;
             dependencyModules = dependencyModules |> List.map fst;
             pathsForModule;
             opens;
             namespace;
             interModuleDependencies;
           }) ) )

let findRoot ~uri packagesByRoot =
  let path = Uri2.toPath uri in
  let rec loop path =
    if path = "/" then None
    else if Hashtbl.mem packagesByRoot path then Some (`Root path)
    else if Files.exists (path /+ "bsconfig.json") then Some (`Bs path)
    else loop (Filename.dirname path)
  in
  loop (Filename.dirname path)

let getPackage ~uri state =
  if Hashtbl.mem state.rootForUri uri then
    Ok (Hashtbl.find state.packagesByRoot (Hashtbl.find state.rootForUri uri))
  else
    match
      findRoot ~uri state.packagesByRoot
      |> RResult.orError "No root directory found"
    with
    | Error e -> Error e
    | Ok root -> (
      match
        match root with
        | `Root rootPath ->
          Hashtbl.replace state.rootForUri uri rootPath;
          Ok
            (Hashtbl.find state.packagesByRoot
               (Hashtbl.find state.rootForUri uri))
        | `Bs rootPath -> (
          match newBsPackage rootPath with
          | Error e -> Error e
          | Ok package ->
            Hashtbl.replace state.rootForUri uri package.rootPath;
            Hashtbl.replace state.packagesByRoot package.rootPath package;
            Ok package )
      with
      | Error e -> Error e
      | Ok package -> Ok package )
