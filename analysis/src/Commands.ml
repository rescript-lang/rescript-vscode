let dumpLocations ~package ~file ~extra:{SharedTypes.locItems} =
  locItems
  |> List.map (fun locItem ->
         let hoverText = Hover.newHover ~package ~file locItem in
         let hover =
           match hoverText with None -> "" | Some s -> String.escaped s
         in
         let uriLocOpt =
           References.definitionForLocItem ~package ~file locItem
         in
         let def =
           match uriLocOpt with
           | None -> Protocol.null
           | Some (uri2, loc) ->
             Protocol.stringifyLocation
               {uri = Uri2.toString uri2; range = Utils.cmtLocToRange loc}
         in
         Protocol.stringifyRange (Utils.cmtLocToRange locItem.loc)
         ^ "\n  Hover: " ^ hover ^ "\n  Definition: " ^ def)
  |> String.concat "\n\n"

let dump files =
  Shared.cacheTypeToString := true;
  files
  |> List.iter (fun path ->
         let uri = Uri2.fromLocalPath path in
         let result =
           match ProcessCmt.getFullFromCmt ~uri with
           | None -> "[]"
           | Some {package; file; extra} -> dumpLocations ~package ~file ~extra
         in
         print_endline result)

let completion ~path ~line ~col ~currentFile =
  let uri = Uri2.fromLocalPath path in
  let result =
    match ProcessCmt.getFullFromCmt ~uri with
    | None -> "[]"
    | Some full ->
      let maybeText = Files.readFile currentFile in
      NewCompletions.computeCompletions ~full ~maybeText ~pos:(line, col)
      |> List.map Protocol.stringifyCompletionItem
      |> Protocol.array
  in
  print_endline result

let hover ~path ~line ~col =
  let uri = Uri2.fromLocalPath path in
  let result =
    match ProcessCmt.getFullFromCmt ~uri with
    | None -> Protocol.null
    | Some {package; file; extra} -> (
      let pos = Utils.protocolLineColToCmtLoc ~line ~col in
      match References.locItemForPos ~extra pos with
      | None -> Protocol.null
      | Some locItem -> (
        let isModule =
          match locItem.locType with
          | SharedTypes.LModule _ | TopLevelModule _ -> true
          | TypeDefinition _ | Typed _ | Constant _ -> false
        in
        let uriLocOpt =
          References.definitionForLocItem ~package ~file locItem
        in
        let skipZero =
          match uriLocOpt with
          | None -> false
          | Some (_, loc) ->
            let isInterface = file.uri |> Uri2.isInterface in
            let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
              (not isInterface) && pos_lnum = 1 && pos_cnum - pos_bol = 0
            in
            (* Skip if range is all zero, unless it's a module *)
            (not isModule) && posIsZero loc.loc_start && posIsZero loc.loc_end
        in
        if skipZero then Protocol.null
        else
          let hoverText = Hover.newHover ~file ~package locItem in
          match hoverText with
          | None -> Protocol.null
          | Some s -> Protocol.stringifyHover {contents = s}))
  in
  print_endline result

let definition ~path ~line ~col =
  let uri = Uri2.fromLocalPath path in
  let result =
    match ProcessCmt.getFullFromCmt ~uri with
    | None -> Protocol.null
    | Some {package; file; extra} -> (
      let pos = Utils.protocolLineColToCmtLoc ~line ~col in

      match References.locItemForPos ~extra pos with
      | None -> Protocol.null
      | Some locItem -> (
        let isModule =
          match locItem.locType with
          | SharedTypes.LModule _ | TopLevelModule _ -> true
          | TypeDefinition _ | Typed _ | Constant _ -> false
        in
        let uriLocOpt =
          References.definitionForLocItem ~package ~file locItem
        in
        match uriLocOpt with
        | None -> Protocol.null
        | Some (uri2, loc) ->
          let isInterface = file.uri |> Uri2.isInterface in
          let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
            (not isInterface) && pos_lnum = 1 && pos_cnum - pos_bol = 0
          in
          (* Skip if range is all zero, unless it's a module *)
          let skipZero =
            (not isModule) && posIsZero loc.loc_start && posIsZero loc.loc_end
          in
          if skipZero then Protocol.null
          else
            Protocol.stringifyLocation
              {uri = Uri2.toString uri2; range = Utils.cmtLocToRange loc}))
  in
  print_endline result

let references ~path ~line ~col =
  let uri = Uri2.fromLocalPath path in
  let result =
    match ProcessCmt.getFullFromCmt ~uri with
    | None -> Protocol.null
    | Some ({extra} as full) -> (
      let pos = Utils.protocolLineColToCmtLoc ~line ~col in
      match References.locItemForPos ~extra pos with
      | None -> Protocol.null
      | Some locItem ->
        let allReferences = References.allReferencesForLocItem ~full locItem in
        let allLocs =
          allReferences
          |> List.fold_left
               (fun acc (uri2, references) ->
                 (references
                 |> List.map (fun loc ->
                        Protocol.stringifyLocation
                          {
                            uri = Uri2.toString uri2;
                            range = Utils.cmtLocToRange loc;
                          }))
                 @ acc)
               []
        in
        "[\n" ^ (allLocs |> String.concat ",\n") ^ "\n]")
  in
  print_endline result

let documentSymbol ~path =
  let uri = Uri2.fromLocalPath path in
  match ProcessCmt.getFullFromCmt ~uri with
  | None -> print_endline Protocol.null
  | Some {file} ->
    let open SharedTypes in
    let rec getItems {topLevel} =
      let rec getItem = function
        | MValue v -> (v |> SharedTypes.variableKind, [])
        | MType (t, _) -> (t.decl |> SharedTypes.declarationKind, [])
        | Module (Structure contents) -> (Module, getItems contents)
        | Module (Constraint (_, modTypeItem)) -> getItem (Module modTypeItem)
        | Module (Ident _) -> (Module, [])
      in
      let fn {name = {txt}; extentLoc; item} =
        let item, siblings = getItem item in
        if extentLoc.loc_ghost then siblings
        else (txt, extentLoc, item) :: siblings
      in
      let x = topLevel |> List.map fn |> List.concat in
      x
    in
    let allSymbols =
      getItems file.contents
      |> List.map (fun (name, loc, kind) ->
             Protocol.stringifyDocumentSymbolItem
               {
                 name;
                 location =
                   {uri = Uri2.toString uri; range = Utils.cmtLocToRange loc};
                 kind = SharedTypes.symbolKind kind;
               })
    in
    print_endline ("[\n" ^ (allSymbols |> String.concat ",\n") ^ "\n]")

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
          (match String.sub rest 0 3 with
          | "def" ->
            print_endline
              ("Definition " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            definition ~path ~line ~col
          | "hov" ->
            print_endline
              ("Hover " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);

            hover ~path ~line ~col
          | "ref" ->
            print_endline
              ("References " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            references ~path ~line ~col
          | "doc" ->
            print_endline ("DocumentSymbol " ^ path);
            documentSymbol ~path
          | "com" ->
            print_endline
              ("Complete " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
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
            completion ~path ~line ~col ~currentFile;
            Sys.remove currentFile
          | _ -> ());
          print_newline ())
    in
    lines |> List.iteri processLine
