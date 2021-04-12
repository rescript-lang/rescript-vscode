let posOfLexing {Lexing.pos_lnum; pos_cnum; pos_bol} =
  Json.Object
    [
      ("line", Json.Number (float_of_int (pos_lnum - 1)));
      ("character", Json.Number (float_of_int (pos_cnum - pos_bol)));
    ]

let rangeOfLoc {Location.loc_start; loc_end} =
  Json.Object [("start", posOfLexing loc_start); ("end", posOfLexing loc_end)]

let dumpLocations state ~package ~file ~extra ~selectPos uri =
  let locations =
    extra.SharedTypes.locations
    |> List.filter (fun (l, _) -> not l.Location.loc_ghost)
  in
  let locations =
    match selectPos with
    | Some (line, col) -> (
      let pos = Utils.protocolLineColToCmtLoc ~line ~col in
      match References.locForPos ~extra:{extra with locations} pos with
      | None -> []
      | Some l -> [l])
    | None -> locations
  in
  let locationsInfo =
    locations
    |> Utils.filterMap (fun ((location : Location.t), loc) ->
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
             | Some s -> [("hover", Json.String s)]
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
                 (not locIsModule) && posIsZero loc.loc_start
                 && posIsZero loc.loc_end
               in
               let range = ("range", rangeOfLoc loc) in
               ( [
                   ( "definition",
                     Json.Object
                       (match uriIsCurrentFile with
                       | true -> [range]
                       | false ->
                         [("uri", Json.String (Uri2.toString uri2)); range]) );
                 ],
                 skipZero )
           in
           let skip = skipZero || (hover = [] && def = []) in
           match skip with
           | true -> None
           | false ->
             Some (Json.Object ([("range", rangeOfLoc location)] @ hover @ def)))
  in
  Json.stringify (Json.Array locationsInfo)

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

let complete ~path ~line ~col ~currentFile =
  let state = TopTypes.empty () in
  let filePath = Files.maybeConcat (Unix.getcwd ()) path in
  let uri = Uri2.fromPath filePath in
  let result =
    match State.getFullFromCmt ~state ~uri with
    | Error message ->
      prerr_endline message;
      "[]"
    | Ok (package, full) ->
      let maybeText = Files.readFile currentFile in
      NewCompletions.computeCompletions ~full ~maybeText ~package
        ~pos:(line, col) ~state
      |> List.map Protocol.stringifyCompletionItem
      |> Protocol.array
  in
  print_endline result

let hover state ~file ~line ~col ~extra ~package =
  let open TopTypes in
  let locations =
    extra.SharedTypes.locations
    |> List.filter (fun (l, _) -> not l.Location.loc_ghost)
  in
  let pos = Utils.protocolLineColToCmtLoc ~line ~col in
  match References.locForPos ~extra:{extra with locations} pos with
  | None -> Protocol.null
  | Some (_, loc) -> (
    let locIsModule =
      match loc with
      | SharedTypes.LModule _ | TopLevelModule _ -> true
      | TypeDefinition _ | Typed _ | Constant _ | Explanation _ -> false
    in
    let uriLocOpt =
      References.definitionForLoc ~pathsForModule:package.pathsForModule ~file
        ~getUri:(State.fileForUri state)
        ~getModule:(State.fileForModule state ~package)
        loc
    in
    let skipZero =
      match uriLocOpt with
      | None -> false
      | Some (_, loc) ->
        let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
          pos_lnum = 1 && pos_cnum - pos_bol = 0
        in
        (* Skip if range is all zero, unless it's a module *)
        (not locIsModule) && posIsZero loc.loc_start && posIsZero loc.loc_end
    in
    if skipZero then Protocol.null
    else
      let hoverText =
        Hover.newHover ~file ~getModule:(State.fileForModule state ~package) loc
      in
      match hoverText with
      | None -> Protocol.null
      | Some s -> Protocol.stringifyHover {contents = s})

let hover ~path ~line ~col =
  let state = TopTypes.empty () in
  let filePath = Files.maybeConcat (Unix.getcwd ()) path in
  let uri = Uri2.fromPath filePath in
  let result =
    match State.getFullFromCmt ~state ~uri with
    | Error message -> Protocol.stringifyHover {contents = message}
    | Ok (package, {file; extra}) ->
      hover state ~file ~line ~col ~extra ~package
  in
  print_endline result

let definition state ~file ~line ~col ~extra ~package =
  let open TopTypes in
  let locations =
    extra.SharedTypes.locations
    |> List.filter (fun (l, _) -> not l.Location.loc_ghost)
  in
  let pos = Utils.protocolLineColToCmtLoc ~line ~col in
  match References.locForPos ~extra:{extra with locations} pos with
  | None -> Protocol.null
  | Some (_, loc) -> (
    let locIsModule =
      match loc with
      | SharedTypes.LModule _ | TopLevelModule _ -> true
      | TypeDefinition _ | Typed _ | Constant _ | Explanation _ -> false
    in
    let uriLocOpt =
      References.definitionForLoc ~pathsForModule:package.pathsForModule ~file
        ~getUri:(State.fileForUri state)
        ~getModule:(State.fileForModule state ~package)
        loc
    in
    match uriLocOpt with
    | None -> Protocol.null
    | Some (uri2, loc) ->
      let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
        pos_lnum = 1 && pos_cnum - pos_bol = 0
      in
      (* Skip if range is all zero, unless it's a module *)
      let skipZero =
        (not locIsModule) && posIsZero loc.loc_start && posIsZero loc.loc_end
      in
      if skipZero then Protocol.null
      else
        Protocol.stringifyLocation
          {uri = Uri2.toString uri2; range = Utils.cmtLocToRange loc})

let definition ~path ~line ~col =
  let state = TopTypes.empty () in
  let filePath = Files.maybeConcat (Unix.getcwd ()) path in
  let uri = Uri2.fromPath filePath in
  let result =
    match State.getFullFromCmt ~state ~uri with
    | Error _message -> Protocol.null
    | Ok (package, {file; extra}) ->
      definition state ~file ~line ~col ~extra ~package
  in
  print_endline result
