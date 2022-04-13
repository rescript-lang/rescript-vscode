type t = int * int

let ofLexing {Lexing.pos_lnum; pos_cnum; pos_bol} =
  (pos_lnum - 1, pos_cnum - pos_bol)

let toString (loc, col) = Printf.sprintf "%d:%d" loc col
