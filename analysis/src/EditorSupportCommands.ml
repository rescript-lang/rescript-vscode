let dumpLocations state ~package ~file ~extra =
  let locations =
    extra.SharedTypes.locations
    |> List.filter (fun (l, _) -> not l.Location.loc_ghost)
  in
  locations
  |> List.map (fun ((location : Location.t), loc) ->
         let hoverText =
           Hover.newHover ~file
             ~getModule:(State.fileForModule state ~package)
             loc
         in
         let hover =
           match hoverText with None -> "" | Some s -> String.escaped s
         in
         let uriLocOpt =
           References.definitionForLoc ~pathsForModule:package.pathsForModule
             ~file ~getUri:(State.fileForUri state)
             ~getModule:(State.fileForModule state ~package)
             loc
         in
         let def =
           match uriLocOpt with
           | None -> Protocol.null
           | Some (uri2, loc) ->
             Protocol.stringifyLocation
               {uri = Uri2.toString uri2; range = Utils.cmtLocToRange loc}
         in
         Protocol.stringifyRange (Utils.cmtLocToRange location)
         ^ "\n  Hover: " ^ hover ^ "\n  Definition: " ^ def)
  |> String.concat "\n\n"

let dump files =
  Shared.cacheTypeToString := true;
  let state = TopTypes.empty () in
  files
  |> List.iter (fun path ->
         let filePath = Files.maybeConcat (Unix.getcwd ()) path in
         let uri = Uri2.fromPath filePath in
         let result =
           match State.getFullFromCmt ~state ~uri with
           | Error message ->
             prerr_endline message;
             "[]"
           | Ok (package, {file; extra}) ->
             dumpLocations state ~package ~file ~extra
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
      | Some s -> Protocol.stringifyHover {contents = s} )

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
          {uri = Uri2.toString uri2; range = Utils.cmtLocToRange loc} )

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

let test ~path =
  Uri2.stripPath := true;
  match Files.readFile path with
  | None -> assert false
  | Some text ->
    let lines = text |> String.split_on_char '\n' in
    let processLine i line =
      if Str.string_match (Str.regexp "^//[ ]*\\^") line 0 then
        let matched = Str.matched_string line in
        let len = line |> String.length in
        let mlen = String.length matched in
        let rest = String.sub line mlen (len - mlen) in
        let line = i - 1 in
        let col = mlen - 1 in
        if mlen >= 3 then (
          ( match String.sub rest 0 3 with
          | "def" ->
            print_endline
              ( "Definition " ^ path ^ " " ^ string_of_int line ^ ":"
              ^ string_of_int col );
            definition ~path ~line ~col
          | "hov" ->
            print_endline
              ( "Hover " ^ path ^ " " ^ string_of_int line ^ ":"
              ^ string_of_int col );

            hover ~path ~line ~col
          | "com" ->
            print_endline
              ( "Complete " ^ path ^ " " ^ string_of_int line ^ ":"
              ^ string_of_int col );
            let currentFile, cout = Filename.open_temp_file "def" "txt" in
            lines
            |> List.iteri (fun j l ->
                   let lineToOutput =
                     if j == i then String.sub rest 3 (len - mlen - 3) else l
                   in
                   Printf.fprintf cout "%s\n" lineToOutput);
            let line = line + 1 in
            let col = len - mlen - 3 in
            close_out cout;
            complete ~path ~line ~col ~currentFile;
            Sys.remove currentFile
          | _ -> () );
          print_newline () )
    in
    lines |> List.iteri processLine
