let typeDefinition ~path ~pos ~debug =
  match Cmt.loadFullCmtFromPath ~path with
  | None -> None
  | Some full -> (
    match References.getLocItem ~full ~pos ~debug with
    | None -> None
    | Some locItem -> (
      match References.typeDefinitionForLocItem ~full locItem with
      | None -> None
      | Some (uri, loc) ->
        Some {Protocol.uri = Uri.toString uri; range = Utils.cmtLocToRange loc})
    )
