open Infix

(* Creates the `pathsForModule` hashtbl, which maps a `moduleName` to it's `paths` (the ml/re, mli/rei, cmt, and cmti files) *)
let makePathsForModule (localModules : (string * SharedTypes.paths) list)
    (dependencyModules : (string * SharedTypes.paths) list) =
  let pathsForModule = Hashtbl.create 30 in
  dependencyModules
  |> List.iter (fun (modName, paths) ->
         Hashtbl.replace pathsForModule modName paths);
  localModules
  |> List.iter (fun (modName, paths) ->
         Hashtbl.replace pathsForModule modName paths);
  pathsForModule

let newBsPackage rootPath =
  let path = Filename.concat rootPath "bsconfig.json" in
  match Files.readFile path with
  | None -> Error ("Unable to read " ^ path)
  | Some raw -> (
    let config = Json.parse raw in
    Log.log {|📣 📣 NEW BSB PACKAGE 📣 📣|};
    (* failwith("Wat"); *)
    Log.log ("- location: " ^ rootPath);
    let compiledBase = BuildSystem.getCompiledBase rootPath in
    match FindFiles.findDependencyFiles rootPath config with
    | Error e -> Error e
    | Ok (dependencyDirectories, dependencyModules) -> (
      match compiledBase with
      | None ->
        Error
          "Please run the build first so that the editor can analyze the \
           project's artifacts."
      | Some compiledBase ->
        Ok
          (let namespace = FindFiles.getNamespace config in
           let localSourceDirs =
             FindFiles.getSourceDirectories ~includeDev:true rootPath config
           in
           Log.log
             ("Got source directories " ^ String.concat " - " localSourceDirs);
           let localModules =
             FindFiles.findProjectFiles namespace rootPath localSourceDirs
               compiledBase
           in
           Log.log
             ("-- All local modules found: "
             ^ string_of_int (List.length localModules));
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
           let opens_from_namespace =
             match namespace with
             | None -> []
             | Some namespace ->
               let cmt = Filename.concat compiledBase namespace ^ ".cmt" in
               Log.log ("############ Namespaced as " ^ namespace ^ " at " ^ cmt);
               Hashtbl.add pathsForModule namespace (Impl (cmt, None));
               [FindFiles.nameSpaceToName namespace]
           in
           Log.log ("Dependency dirs " ^ String.concat " " dependencyDirectories);
           let opens_from_bsc_flags =
             match Json.get "bsc-flags" config |?> Json.array with
             | Some l ->
               List.fold_left
                 (fun opens item ->
                   match item |> Json.string with
                   | None -> opens
                   | Some s -> (
                     let parts = String.split_on_char ' ' s in
                     match parts with
                     | "-open" :: name :: _ -> name :: opens
                     | _ -> opens))
                 [] l
             | None -> []
           in
           let opens =
             List.rev_append opens_from_bsc_flags opens_from_namespace
           in
           Log.log ("Opens from bsconfig: " ^ (opens |> String.concat " "));
           let interModuleDependencies =
             Hashtbl.create (List.length localModules)
           in
           {
             SharedTypes.rootPath;
             localModules = localModules |> List.map fst;
             dependencyModules = dependencyModules |> List.map fst;
             pathsForModule;
             opens;
             namespace;
             interModuleDependencies;
           })))

let findRoot ~uri packagesByRoot =
  let path = Uri2.toPath uri in
  let rec loop path =
    if path = "/" then None
    else if Hashtbl.mem packagesByRoot path then Some (`Root path)
    else if Files.exists (Filename.concat path "bsconfig.json") then
      Some (`Bs path)
    else
      let parent = Filename.dirname path in
      if parent = path then (* reached root *) None else loop parent
  in
  loop (Filename.dirname path)

let getPackage ~uri =
  let open SharedTypes in
  if Hashtbl.mem state.rootForUri uri then
    Ok (Hashtbl.find state.packagesByRoot (Hashtbl.find state.rootForUri uri))
  else
    match findRoot ~uri state.packagesByRoot with
    | None -> Error "No root directory found"
    | Some (`Root rootPath) ->
      Hashtbl.replace state.rootForUri uri rootPath;
      Ok (Hashtbl.find state.packagesByRoot (Hashtbl.find state.rootForUri uri))
    | Some (`Bs rootPath) -> (
      match newBsPackage rootPath with
      | Error e -> Error e
      | Ok package ->
        Hashtbl.replace state.rootForUri uri package.rootPath;
        Hashtbl.replace state.packagesByRoot package.rootPath package;
        Ok package)
