type t = Location.t

let start (loc : t) = Pos.ofLexing loc.loc_start
let end_ (loc : t) = Pos.ofLexing loc.loc_end
let range loc : Range.t = (start loc, end_ loc)
let toString (loc : t) = loc |> range |> Range.toString
let hasPos ~pos loc = start loc <= pos && pos < end_ loc
