let fileForCmt ~moduleName ~uri cmt =
  match Shared.tryReadCmt cmt with
  | Error e -> Error e
  | Ok infos -> Ok (ProcessCmt.forCmt ~moduleName ~uri infos)

let fullForCmt ~moduleName ~uri cmt =
  match Shared.tryReadCmt cmt with
  | Error e -> Error e
  | Ok infos ->
    let file = ProcessCmt.forCmt ~moduleName ~uri infos in
    let extra = ProcessExtra.forCmt ~file infos in
    Ok {SharedTypes.file; extra}

module PrintType = PrintType
