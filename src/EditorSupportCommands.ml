module J = JsonShort

let dumpLocations state ~package ~file ~extra ~selectPos uri =
  let locations =
    extra.SharedTypes.locations
    |> List.filter (fun (l, _) -> not l.Location.loc_ghost)
  in
  let locations =
    match selectPos with
    | Some pos -> (
      let pos = Utils.cmtLocFromVscode pos in
      match References.locForPos ~extra:{extra with locations} pos with
      | None -> []
      | Some l -> [l] )
    | None -> locations
  in
  let dedupTable = Hashtbl.create 1 in
  let dedupHover hover i =
    let isCandidate = String.length hover > 10 in
    if isCandidate then (
      match Hashtbl.find_opt dedupTable hover with
      | Some n -> J.s ("#" ^ string_of_int n)
      | None ->
        Hashtbl.replace dedupTable hover i;
        J.s hover )
    else J.s hover
  in
  let locationsInfo =
    locations
    |> Utils.filterMapIndex (fun i ((location : Location.t), loc) ->
        let locIsModule =
          match loc with
          | SharedTypes.LModule _ | TopLevelModule _ -> true
          | TypeDefinition _ | Typed _ | Constant _ | Explanation _ -> false
        in
        let hoverText =
          Hover.newHover ~file
            ~getModule:(State.fileForModule state ~package)
            loc
        in
        let hover =
          match hoverText with
          | None -> []
          | Some s -> [("hover", dedupHover s i)]
        in
        let uriLocOpt =
          References.definitionForLoc ~pathsForModule:package.pathsForModule
            ~file ~getUri:(State.fileForUri state)
            ~getModule:(State.fileForModule state ~package)
            loc
        in
        let def, skipZero =
          match uriLocOpt with
          | None -> ([], false)
          | Some (uri2, loc) ->
            let uriIsCurrentFile = uri = uri2 in
            let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
              pos_lnum = 1 && pos_cnum - pos_bol = 0
            in
            (* Skip if range is all zero, unless it's a module *)
            let skipZero =
              (not locIsModule) && loc.loc_start |> posIsZero
              && loc.loc_end |> posIsZero
            in
            let range = ("range", Protocol.rangeOfLoc loc) in
            (
              [
                ("definition",
                  J.o
                    (match uriIsCurrentFile with
                    | true -> [range]
                    | false -> [("uri", Json.String (Uri2.toString uri2)); range])
                )
              ],
              skipZero
            )
        in
        let skip = skipZero || (hover = [] && def = []) in
        match skip with
        | true -> None
        | false -> Some (J.o ([("range", Protocol.rangeOfLoc location)] @ hover @ def)))
    |> J.l
  in
  Json.stringify locationsInfo

(* Split (line,char) from filepath:line:char *)
let splitLineChar pathWithPos =
  let mkPos line char = Some (line |> int_of_string, char |> int_of_string) in
  match pathWithPos |> String.split_on_char ':' with
  | [filePath; line; char] -> (filePath, mkPos line char)
  | [drive; rest; line; char] ->
    (* c:\... on Windows *)
    (drive ^ ":" ^ rest, mkPos line char)
  | _ -> (pathWithPos, None)

let dump files =
  Shared.cacheTypeToString := true;
  let state = TopTypes.empty () in
  files
  |> List.iter (fun pathWithPos ->
      let filePath, selectPos = pathWithPos |> splitLineChar in
      let filePath = Files.maybeConcat (Unix.getcwd ()) filePath in
      let uri = Uri2.fromPath filePath in
      let result =
        match State.getFullFromCmt ~state ~uri with
        | Error message ->
          prerr_endline message;
          "[]"
        | Ok (package, {file; extra}) ->
          dumpLocations state ~package ~file ~extra ~selectPos uri
      in
      print_endline result)

let autocomplete ~currentFile ~full ~package ~pos ~state =
  let maybeText = Files.readFile currentFile in
  let completions =
    NewCompletions.computeCompletions ~full ~maybeText ~package ~pos ~state
  in
  Json.stringify completions

let complete ~pathWithPos ~currentFile =
  let state = TopTypes.empty () in
  match pathWithPos |> splitLineChar with
  | filePath, Some pos ->
    let filePath = Files.maybeConcat (Unix.getcwd ()) filePath in
    let uri = Uri2.fromPath filePath in
    let result =
      match State.getFullFromCmt ~state ~uri with
      | Error message ->
        prerr_endline message;
        "[]"
      | Ok (package, full) ->
        autocomplete ~currentFile ~full ~package ~pos ~state
    in
    print_endline result
  | _ -> ()
