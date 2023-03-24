let definition ~path ~pos ~debug =
  match Cmt.loadFullCmtFromPath ~path with
  | None -> None
  | Some full -> (
    match References.getLocItem ~full ~pos ~debug with
    | None -> None
    | Some locItem -> (
      match References.definitionForLocItem ~full locItem with
      | None -> None
      | Some (uri, loc) ->
        let isInterface = full.file.uri |> Uri.isInterface in
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
            {Protocol.uri = Uri.toString uri; range = Utils.cmtLocToRange loc}))
