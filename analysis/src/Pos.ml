type t = int * int

let ofLexing = Utils.tupleOfLexing
let toString (loc, col) = Printf.sprintf "%d:%d" loc col
