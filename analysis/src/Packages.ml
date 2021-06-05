open Infix

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

let newBsPackage rootPath =
  let path = Filename.concat rootPath "bsconfig.json" in
  match Files.readFile path with
  | None -> Error ("Unable to read " ^ path)
  | Some raw -> (
    let config = Json.parse raw in
    Log.log {|📣 📣 NEW BSB PACKAGE 📣 📣|};
    Log.log ("- location: " ^ rootPath);
    let libBs = BuildSystem.getLibBs rootPath in
    match FindFiles.findDependencyFiles rootPath config with
    | Error e -> Error e
    | Ok (dependencyDirectories, dependenciesFilesAndPaths) -> (
      match libBs with
      | None ->
        Error
          "Please run the build first so that the editor can analyze the \
           project's artifacts."
      | Some libBs ->
        Ok
          (let namespace = FindFiles.getNamespace config in
           let sourceDirectories =
             FindFiles.getSourceDirectories ~includeDev:true ~baseDir:rootPath
               config
           in
           Log.log
             ("Got source directories " ^ String.concat " - " sourceDirectories);
           let projectFilesAndPaths =
             FindFiles.findProjectFiles ~namespace ~path:rootPath
               ~sourceDirectories ~libBs
           in
           Log.log
             ("-- All project files found: "
             ^ string_of_int (List.length projectFilesAndPaths));
           projectFilesAndPaths
           |> List.iter (fun (name, paths) ->
                  Log.log name;
                  match paths with
                  | SharedTypes.Impl {cmt} -> Log.log ("impl " ^ cmt)
                  | _ -> Log.log "Both");
           let pathsForModule =
             makePathsForModule ~projectFilesAndPaths ~dependenciesFilesAndPaths
           in
           let opens_from_namespace =
             match namespace with
             | None -> []
             | Some namespace ->
               let cmt = Filename.concat libBs namespace ^ ".cmt" in
               Log.log ("############ Namespaced as " ^ namespace ^ " at " ^ cmt);
               Hashtbl.add pathsForModule namespace (Namespace {cmt});
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
           {
             SharedTypes.rootPath;
             projectFiles = projectFilesAndPaths |> List.map fst;
             dependenciesFiles = dependenciesFilesAndPaths |> List.map fst;
             pathsForModule;
             opens;
             namespace;
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
  prerr_endline ("getPackage " ^ Uri2.toString uri);
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
