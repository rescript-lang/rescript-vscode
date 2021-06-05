let dumpLocations ~full =
  full.SharedTypes.extra.locItems
  |> List.map (fun locItem ->
         let locItemTxt = SharedTypes.locItemToString locItem in
         let hoverText = Hover.newHover ~full locItem in
         let hover =
           match hoverText with None -> "" | Some s -> String.escaped s
         in
         let uriLocOpt = References.definitionForLocItem ~full locItem in
         let def =
           match uriLocOpt with
           | None -> Protocol.null
           | Some (uri2, loc) ->
             Protocol.stringifyLocation
               {uri = Uri2.toString uri2; range = Utils.cmtLocToRange loc}
         in
         Protocol.stringifyRange (Utils.cmtLocToRange locItem.loc)
         ^ "\n  Hover: " ^ hover ^ "\n  Definition: " ^ def ^ "\n locItem: "
         ^ locItemTxt)
  |> String.concat "\n\n"

let dump files =
  Shared.cacheTypeToString := true;
  files
  |> List.iter (fun path ->
         let uri = Uri2.fromPath path in
         let result =
           match ProcessCmt.getFullFromCmt ~uri with
           | None -> "[]"
           | Some full -> dumpLocations ~full
         in
         print_endline result)

let completion ~path ~line ~col ~currentFile =
  let uri = Uri2.fromPath path in
  let result =
    let textOpt = Files.readFile currentFile in
    NewCompletions.computeCompletions ~uri ~textOpt ~pos:(line, col)
    |> List.map Protocol.stringifyCompletionItem
    |> Protocol.array
  in
  print_endline result

let hover ~path ~line ~col =
  let uri = Uri2.fromPath path in
  let result =
    match ProcessCmt.getFullFromCmt ~uri with
    | None -> Protocol.null
    | Some ({file} as full) -> (
      let pos = Utils.protocolLineColToCmtLoc ~line ~col in
      match References.locItemForPos ~full pos with
      | None -> Protocol.null
      | Some locItem -> (
        let isModule =
          match locItem.locType with
          | SharedTypes.LModule _ | TopLevelModule _ -> true
          | TypeDefinition _ | Typed _ | Constant _ -> false
        in
        let uriLocOpt = References.definitionForLocItem ~full locItem in
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
          let hoverText = Hover.newHover ~full locItem in
          match hoverText with
          | None -> Protocol.null
          | Some s -> Protocol.stringifyHover {contents = s}))
  in
  print_endline result

let definition ~path ~line ~col =
  let uri = Uri2.fromPath path in
  let result =
    match ProcessCmt.getFullFromCmt ~uri with
    | None -> Protocol.null
    | Some ({file} as full) -> (
      let pos = Utils.protocolLineColToCmtLoc ~line ~col in

      match References.locItemForPos ~full pos with
      | None -> Protocol.null
      | Some locItem -> (
        let isModule =
          match locItem.locType with
          | SharedTypes.LModule _ | TopLevelModule _ -> true
          | TypeDefinition _ | Typed _ | Constant _ -> false
        in
        let uriLocOpt = References.definitionForLocItem ~full locItem in
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
  let uri = Uri2.fromPath path in
  let result =
    match ProcessCmt.getFullFromCmt ~uri with
    | None -> Protocol.null
    | Some full -> (
      let pos = Utils.protocolLineColToCmtLoc ~line ~col in
      match References.locItemForPos ~full pos with
      | None -> Protocol.null
      | Some locItem ->
        let allReferences = References.allReferencesForLocItem ~full locItem in
        let allLocs =
          allReferences
          |> List.fold_left
               (fun acc {References.uri = uri2; loc} ->
                 Protocol.stringifyLocation
                   {uri = Uri2.toString uri2; range = Utils.cmtLocToRange loc}
                 :: acc)
               []
        in
        "[\n" ^ (allLocs |> String.concat ",\n") ^ "\n]")
  in
  print_endline result

let documentSymbol ~path =
  let uri = Uri2.fromPath path in
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

let rename ~path ~line ~col ~newName =
  let uri = Uri2.fromPath path in
  let result =
    match ProcessCmt.getFullFromCmt ~uri with
    | None -> Protocol.null
    | Some full -> (
      let pos = Utils.protocolLineColToCmtLoc ~line ~col in
      match References.locItemForPos ~full pos with
      | None -> Protocol.null
      | Some locItem ->
        let allReferences = References.allReferencesForLocItem ~full locItem in
        let referencesToToplevelModules, referencesToItems =
          allReferences
          |> List.fold_left
               (fun acc {References.uri = uri2; loc} -> (uri2, loc) :: acc)
               []
          |> List.partition (fun (_, loc) -> Utils.isTopLoc loc)
        in
        let fileRenames =
          referencesToToplevelModules
          |> List.map (fun (uri, _) ->
                 let path = Uri2.toPath uri in
                 let dir = Filename.dirname path in
                 let newPath =
                   Filename.concat dir (newName ^ Filename.extension path)
                 in
                 let newUri = Uri2.fromPath newPath in
                 Protocol.
                   {
                     oldUri = uri |> Uri2.toString;
                     newUri = newUri |> Uri2.toString;
                   })
        in
        let textDocumentEdits =
          let module StringMap = Misc.StringMap in
          let textEditsByUri =
            referencesToItems
            |> List.map (fun (uri, loc) -> (Uri2.toString uri, loc))
            |> List.fold_left
                 (fun acc (uri, loc) ->
                   let textEdit =
                     Protocol.
                       {range = Utils.cmtLocToRange loc; newText = newName}
                   in
                   match StringMap.find_opt uri acc with
                   | None -> StringMap.add uri [textEdit] acc
                   | Some prevEdits ->
                     StringMap.add uri (textEdit :: prevEdits) acc)
                 StringMap.empty
          in
          StringMap.fold
            (fun uri edits acc ->
              let textDocumentEdit =
                Protocol.{textDocument = {uri; version = None}; edits}
              in
              textDocumentEdit :: acc)
            textEditsByUri []
        in
        let fileRenamesString =
          fileRenames |> List.map Protocol.stringifyRenameFile
        in
        let textDocumentEditsString =
          textDocumentEdits |> List.map Protocol.stringifyTextDocumentEdit
        in
        "[\n"
        ^ (fileRenamesString @ textDocumentEditsString |> String.concat ",\n")
        ^ "\n]")
  in
  print_endline result

let test ~path =
  Uri2.stripPath := true;
  match Files.readFile path with
  | None -> assert false
  | Some text ->
    let lines = text |> String.split_on_char '\n' in
    let processLine i line =
      if Str.string_match (Str.regexp "^ *//[ ]*\\^") line 0 then
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
          | "ren" ->
            let newName = String.sub rest 4 (len - mlen - 4) in
            let () =
              print_endline
                ("Rename " ^ path ^ " " ^ string_of_int line ^ ":"
               ^ string_of_int col ^ " " ^ newName)
            in

            rename ~path ~line ~col ~newName
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
