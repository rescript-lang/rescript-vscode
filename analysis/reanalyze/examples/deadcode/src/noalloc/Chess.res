type pos // positions
type board
type score
type move

let computeScore: (~board: board, ~pos: pos) => score = assert false
let findNextMoves: (~board: board, ~pos: pos) => list<move> = assert false
let makeMove: (move, ~board: board, ~pos: pos) => (board, pos) = assert false

let rec chess = (~pos, ~board, ~n) => {
  if n == 0 {
    list{computeScore(~board, ~pos)}
  } else {
    let nextMoves = findNextMoves(~board, ~pos)
    nextMoves
    ->Belt.List.map(move => {
      let (nextBoard, nextPos) = makeMove(move, ~board, ~pos)

      @local
      chess(~board=nextBoard, ~pos=nextPos, ~n=n - 1)
    })
    ->Belt.List.flatten
  }
}
