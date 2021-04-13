open Infix
open TopTypes

let isMl path =
  Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli"

let odocToMd text = MarkdownOfOCamldoc.convert text

let compose fn1 fn2 arg = fn1 arg |> fn2

let converter src =
  let mlToOutput s = [compose odocToMd Omd.to_markdown s] in
  fold src mlToOutput (fun src ->
      match isMl src with true -> mlToOutput | false -> fun x -> [x])

let newDocsForCmt ~moduleName cmtCache changed cmt src =
  let uri = Uri2.fromPath (src |? cmt) in
  match Process_406.fileForCmt ~moduleName ~uri cmt (converter src) with
  | Error e ->
    Log.log e;
    None
  | Ok file ->
    Hashtbl.replace cmtCache cmt (changed, file);
    Some file

let docsForCmt ~moduleName cmt src state =
  if Hashtbl.mem state.cmtCache cmt then
    let mtime, docs = Hashtbl.find state.cmtCache cmt in
    (* TODO: I should really throttle this mtime checking to like every 50 ms or so *)
    match Files.getMtime cmt with
    | None ->
      Log.log
        ("\226\154\160\239\184\143 cannot get docs for nonexistant cmt " ^ cmt);
      None
    | Some changed ->
      if changed > mtime then
        newDocsForCmt ~moduleName state.cmtCache changed cmt src
      else Some docs
  else
    match Files.getMtime cmt with
    | None ->
      Log.log
        ("\226\154\160\239\184\143 cannot get docs for nonexistant cmt " ^ cmt);
      None
    | Some changed -> newDocsForCmt ~moduleName state.cmtCache changed cmt src

open Infix

let getFullFromCmt ~state ~uri =
  let path = Uri2.toPath uri in
  match Packages.getPackage uri state with
  | Error e -> Error e
  | Ok package -> (
    let moduleName =
      BuildSystem.namespacedName package.namespace (FindFiles.getName path)
    in
    match Hashtbl.find_opt package.pathsForModule moduleName with
    | Some paths -> (
      let cmt = SharedTypes.getCmt ~interface:(Utils.endsWith path "i") paths in
      match Process_406.fullForCmt ~moduleName ~uri cmt (fun x -> [x]) with
      | Error e -> Error e
      | Ok full ->
        Hashtbl.replace package.interModuleDependencies moduleName
          (SharedTypes.hashList full.extra.externalReferences |> List.map fst);
        Ok (package, full))
    | None -> Error ("can't find module " ^ moduleName))

let docsForModule modname state ~package =
  if Hashtbl.mem package.pathsForModule modname then (
    let paths = Hashtbl.find package.pathsForModule modname in
    (* TODO: do better *)
    let cmt = SharedTypes.getCmt paths in
    let src = SharedTypes.getSrc paths in
    Log.log ("FINDING docs for module " ^ SharedTypes.showPaths paths);
    Log.log ("FINDING " ^ cmt ^ " src " ^ (src |? ""));
    match docsForCmt ~moduleName:modname cmt src state with
    | None -> None
    | Some docs -> Some (docs, src))
  else (
    Log.log ("No path for module " ^ modname);
    None)

let fileForUri state uri =
  match getFullFromCmt ~state ~uri with
  | Error e -> Error e
  | Ok (_package, {extra; file}) -> Ok (file, extra)

let fileForModule state ~package modname =
  match docsForModule modname state ~package with
  | None -> None
  | Some (file, _) -> Some file
