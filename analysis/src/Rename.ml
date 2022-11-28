let command ~path ~pos ~newName ~debug =
  match Cmt.fullFromPath ~path with
  | None -> None
  | Some full -> (
    match References.getLocItem ~full ~pos ~debug with
    | None -> None
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
               let path = Uri.toPath uri in
               let dir = Filename.dirname path in
               let newPath =
                 Filename.concat dir (newName ^ Filename.extension path)
               in
               let newUri = Uri.fromPath newPath in
               Protocol.RenameFile
                 {oldUri = uri |> Uri.toString; newUri = newUri |> Uri.toString})
      in
      let textDocumentEdits =
        let module StringMap = Misc.StringMap in
        let textEditsByUri =
          referencesToItems
          |> List.map (fun (uri, loc) -> (Uri.toString uri, loc))
          |> List.fold_left
               (fun acc (uri, loc) ->
                 let textEdit =
                   Protocol.{range = Utils.cmtLocToRange loc; newText = newName}
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
              Protocol.TextDocumentEdit
                {textDocument = {uri; version = None}; edits}
            in
            textDocumentEdit :: acc)
          textEditsByUri []
      in
      let documentChanges = fileRenames @ textDocumentEdits in
      Some Protocol.{documentChanges})
