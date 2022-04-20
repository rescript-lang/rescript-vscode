type item = Module of string list | Value of string * Location.t
type t = item list

let itemToString item =
  let str s = if s = "" then "\"\"" else s in
  let list l = "[" ^ (l |> List.map str |> String.concat ", ") ^ "]" in
  match item with
  | Module sl -> "Module " ^ list sl
  | Value (s, loc) -> "Value " ^ s ^ " " ^ Loc.toString loc

let create () : t = []
let addModule id x = Module (Utils.flattenLongIdent id @ ["place holder"]) :: x
let addValue name loc x = Value (name, loc) :: x

let getRawOpens x =
  x |> Utils.filterMap (function Module path -> Some path | _ -> None)
