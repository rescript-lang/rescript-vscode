let getCompletions ~debug ~path ~pos ~currentFile ~forHover =
  let textOpt = Files.readFile currentFile in
  match textOpt with
  | None | Some "" -> []
  | Some text -> (
    match
      CompletionFrontEnd.completionWithParser ~debug ~path ~posCursor:pos
        ~currentFile ~text
    with
    | None -> []
    | Some (completable, scope) -> (
      if debug then
        Printf.printf "Completable: %s\n"
          (SharedTypes.Completable.toString completable);
      (* Only perform expensive ast operations if there are completables *)
      match Cmt.fromPath ~path with
      | None -> []
      | Some full ->
        let env = SharedTypes.QueryEnv.fromFile full.file in
        let package = full.package in
        completable
        |> CompletionBackEnd.processCompletable ~debug ~package ~pos ~scope ~env
             ~forHover))

let completion ~debug ~path ~pos ~currentFile =
  print_endline
    (getCompletions ~debug ~path ~pos ~currentFile ~forHover:false
    |> List.map CompletionBackEnd.completionToItem
    |> List.map Protocol.stringifyCompletionItem
    |> Protocol.array)

let hover ~path ~line ~col ~currentFile ~debug =
  let result =
    match Cmt.fromPath ~path with
    | None -> Protocol.null
    | Some ({file} as full) -> (
      match References.getLocItem ~full ~line ~col with
      | None -> (
        let completions =
          getCompletions ~debug ~path ~pos:(line, col) ~currentFile
            ~forHover:true
        in
        match completions with
        | {kind = Label typString} :: _ ->
          Protocol.stringifyHover {contents = Hover.codeBlock typString}
        | _ -> Protocol.null)
      | Some locItem -> (
        let isModule =
          match locItem.locType with
          | LModule _ | TopLevelModule _ -> true
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

let codeAction ~path ~line ~col ~currentFile =
  Xform.extractCodeActions ~path ~pos:(line, col) ~currentFile
  |> CodeActions.stringifyCodeActions |> print_endline

let definition ~path ~line ~col =
  let locationOpt =
    match Cmt.fromPath ~path with
    | None -> None
    | Some ({file} as full) -> (
      match References.getLocItem ~full ~line ~col with
      | None -> None
      | Some locItem -> (
        match References.definitionForLocItem ~full locItem with
        | None -> None
        | Some (uri, loc) ->
          let isInterface = file.uri |> Uri2.isInterface in
          let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
            (* range is zero *)
            pos_lnum = 1 && pos_cnum - pos_bol = 0
          in
          let isModule =
            match locItem.locType with
            | LModule _ | TopLevelModule _ -> true
            | TypeDefinition _ | Typed _ | Constant _ -> false
          in
          let skipLoc =
            (not isModule) && (not isInterface) && posIsZero loc.loc_start
            && posIsZero loc.loc_end
          in
          if skipLoc then None
          else
            Some
              {
                Protocol.uri = Uri2.toString uri;
                range = Utils.cmtLocToRange loc;
              }))
  in
  print_endline
    (match locationOpt with
    | None -> Protocol.null
    | Some location -> location |> Protocol.stringifyLocation)

let typeDefinition ~path ~line ~col =
  let maybeLocation =
    match Cmt.fromPath ~path with
    | None -> None
    | Some full -> (
      match References.getLocItem ~full ~line ~col with
      | None -> None
      | Some locItem -> (
        match References.typeDefinitionForLocItem ~full locItem with
        | None -> None
        | Some (uri, loc) ->
          Some
            {Protocol.uri = Uri2.toString uri; range = Utils.cmtLocToRange loc})
      )
  in
  print_endline
    (match maybeLocation with
    | None -> Protocol.null
    | Some location -> location |> Protocol.stringifyLocation)

let references ~path ~line ~col =
  let allLocs =
    match Cmt.fromPath ~path with
    | None -> []
    | Some full -> (
      match References.getLocItem ~full ~line ~col with
      | None -> []
      | Some locItem ->
        let allReferences = References.allReferencesForLocItem ~full locItem in
        allReferences
        |> List.fold_left
             (fun acc {References.uri = uri2; locOpt} ->
               let loc =
                 match locOpt with
                 | Some loc -> loc
                 | None -> Uri2.toTopLevelLoc uri2
               in
               Protocol.stringifyLocation
                 {uri = Uri2.toString uri2; range = Utils.cmtLocToRange loc}
               :: acc)
             [])
  in
  print_endline
    (if allLocs = [] then Protocol.null
    else "[\n" ^ (allLocs |> String.concat ",\n") ^ "\n]")

let rename ~path ~line ~col ~newName =
  let result =
    match Cmt.fromPath ~path with
    | None -> Protocol.null
    | Some full -> (
      match References.getLocItem ~full ~line ~col with
      | None -> Protocol.null
      | Some locItem ->
        let allReferences = References.allReferencesForLocItem ~full locItem in
        let referencesToToplevelModules =
          allReferences
          |> Utils.filterMap (fun {References.uri = uri2; locOpt} ->
                 if locOpt = None then Some uri2 else None)
        in
        let referencesToItems =
          allReferences
          |> Utils.filterMap (function
               | {References.uri = uri2; locOpt = Some loc} -> Some (uri2, loc)
               | {locOpt = None} -> None)
        in
        let fileRenames =
          referencesToToplevelModules
          |> List.map (fun uri ->
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

let format ~path =
  if Filename.check_suffix path ".res" then
    let {Res_driver.parsetree = structure; comments; diagnostics} =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:true
        ~filename:path
    in
    if List.length diagnostics > 0 then ""
    else
      Res_printer.printImplementation !Res_cli.ResClflags.width structure
        comments
  else if Filename.check_suffix path ".resi" then
    let {Res_driver.parsetree = signature; comments; diagnostics} =
      Res_driver.parsingEngine.parseInterface ~forPrinter:true ~filename:path
    in
    if List.length diagnostics > 0 then ""
    else Res_printer.printInterface !Res_cli.ResClflags.width signature comments
  else ""

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
          | "db+" -> Log.verbose := true
          | "db-" -> Log.verbose := false
          | "def" ->
            print_endline
              ("Definition " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            definition ~path ~line ~col
          | "typ" ->
            print_endline
              ("TypeDefinition " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            typeDefinition ~path ~line ~col
          | "hov" ->
            print_endline
              ("Hover " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            hover ~path ~line ~col ~currentFile:path ~debug:true
          | "ref" ->
            print_endline
              ("References " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            references ~path ~line ~col
          | "doc" ->
            print_endline ("DocumentSymbol " ^ path);
            DocumentSymbol.command ~path
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
            let removeLineComment l =
              let len = String.length l in
              let rec loop i =
                if i + 2 <= len && l.[i] = '/' && l.[i + 1] = '/' then
                  Some (i + 2)
                else if i + 2 < len && l.[i] = ' ' then loop (i + 1)
                else None
              in
              match loop 0 with
              | None -> l
              | Some indexAfterComment ->
                String.make indexAfterComment ' '
                ^ String.sub l indexAfterComment (len - indexAfterComment)
            in
            lines
            |> List.iteri (fun j l ->
                   let lineToOutput =
                     if j == i - 1 then removeLineComment l else l
                   in
                   Printf.fprintf cout "%s\n" lineToOutput);
            close_out cout;
            completion ~debug:true ~path ~pos:(line, col) ~currentFile;
            Sys.remove currentFile
          | "hig" ->
            print_endline ("Highlight " ^ path);
            SemanticTokens.command ~debug:true
              ~emitter:(SemanticTokens.Token.createEmitter ())
              ~path
          | "int" ->
            print_endline ("Create Interface " ^ path);
            let cmiFile =
              let open Filename in
              let ( ++ ) = concat in
              let name = chop_extension (basename path) ^ ".cmi" in
              let dir = dirname path in
              dir ++ parent_dir_name ++ "lib" ++ "bs" ++ "src" ++ name
            in
            Printf.printf "%s" (CreateInterface.command ~path ~cmiFile)
          | "xfm" ->
            print_endline
              ("Xform " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let codeActions =
              Xform.extractCodeActions ~path ~pos:(line, col) ~currentFile:path
            in
            codeActions
            |> List.iter (fun {Protocol.title; edit = {documentChanges}} ->
                   Printf.printf "Hit: %s\n" title;
                   documentChanges
                   |> List.iter (fun {Protocol.edits} ->
                          edits
                          |> List.iter (fun {Protocol.range; newText} ->
                                 let indent =
                                   String.make range.start.character ' '
                                 in
                                 Printf.printf "%s\nnewText:\n%s<--here\n%s%s\n"
                                   (Protocol.stringifyRange range)
                                   indent indent newText)))
          | _ -> ());
          print_newline ())
    in
    lines |> List.iteri processLine
