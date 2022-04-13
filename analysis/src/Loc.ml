type t = Location.t

let toString (loc : t) = loc |> Range.ofLoc |> Range.toString
