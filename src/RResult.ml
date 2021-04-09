let resultOfOption err v = match v with Some v -> Ok v | None -> Error err

let orError = resultOfOption

let toOptionAndLog err =
  match err with
  | Error e ->
    Log.log e;
    None
  | Ok v -> Some v
