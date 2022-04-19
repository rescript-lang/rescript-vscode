type item = Module of string list | Value of unit
type t = item list

let create () : t = []
let addModule id t = Module (Utils.flattenLongIdent id @ ["place holder"]) :: t

let getRawOpens x =
  x |> Utils.filterMap (function Module path -> Some path | _ -> None)
