type t = Pos.t * Pos.t

let ofLoc (loc : Location.t) =
  (Pos.ofLexing loc.loc_start, Pos.ofLexing loc.loc_end)

let toString ((posStart, posEnd) : t) =
  Printf.sprintf "[%s->%s]" (Pos.toString posStart) (Pos.toString posEnd)
