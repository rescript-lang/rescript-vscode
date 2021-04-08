let resultOfOption err v = match v with Some v -> Ok v | None -> Error err

let orError = resultOfOption

let toOptionAndLog err =
  match err with
  | Error e ->
    Log.log e;
    None
  | Ok v -> Some v

module InfixResult = struct
  let ( |?>> ) a fn = match a with Ok a -> Ok (fn a) | Error e -> Error e

  let ( |? ) a default = match a with Ok a -> a | Error _ -> default
end

open InfixResult

let withDefault d v = v |? d
