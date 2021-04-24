open SharedTypes

let fileForCmt ~moduleName ~uri cmt processDoc =
  match Shared.tryReadCmt cmt with
  | Error e -> Error e
  | Ok infos -> Ok (ProcessCmt.forCmt ~moduleName ~uri processDoc infos)

let fullForCmt ~moduleName ~uri cmt processDoc =
  match Shared.tryReadCmt cmt with
  | Error e -> Error e
  | Ok infos ->
    let file = ProcessCmt.forCmt ~moduleName ~uri processDoc infos in
    let extra = ProcessExtra.forCmt ~file infos in
    Ok {file; extra}

module PrintType = PrintType
