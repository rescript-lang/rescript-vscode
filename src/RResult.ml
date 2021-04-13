let toOptionAndLog err =
  match err with
  | Error e ->
    Log.log e;
    None
  | Ok v -> Some v
