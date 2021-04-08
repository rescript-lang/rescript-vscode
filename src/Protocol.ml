module J = JsonShort

let posOfLexing {Lexing.pos_lnum; pos_cnum; pos_bol} =
  J.o [("line", J.i (pos_lnum - 1)); ("character", J.i (pos_cnum - pos_bol))]

let rangeOfLoc {Location.loc_start; loc_end} =
  J.o [("start", posOfLexing loc_start); ("end", posOfLexing loc_end)]
