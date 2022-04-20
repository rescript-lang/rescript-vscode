type item =
  | Constructor of string * Location.t
  | Field of string * Location.t
  | Open of string list
  | Type of string * Location.t
  | Value of string * Location.t

type t = item list

let itemToString item =
  let str s = if s = "" then "\"\"" else s in
  let list l = "[" ^ (l |> List.map str |> String.concat ", ") ^ "]" in
  match item with
  | Constructor (s, loc) -> "Constructor " ^ s ^ " " ^ Loc.toString loc
  | Field (s, loc) -> "Field " ^ s ^ " " ^ Loc.toString loc
  | Open sl -> "Module " ^ list sl
  | Value (s, loc) -> "Value " ^ s ^ " " ^ Loc.toString loc
  | Type (s, loc) -> "Type " ^ s ^ " " ^ Loc.toString loc

let create () : t = []
let addConstructor ~name ~loc x = Constructor (name, loc) :: x
let addField ~name ~loc x = Field (name, loc) :: x
let addOpen ~lid x = Open (Utils.flattenLongIdent lid @ ["place holder"]) :: x
let addValue ~name ~loc x = Value (name, loc) :: x
let addType ~name ~loc x = Type (name, loc) :: x

let getRawOpens x =
  x |> Utils.filterMap (function Open path -> Some path | _ -> None)
