let xx = 10

let y = xx
//      ^def

module Inner = {
  type tInner = int
  let vInner = 34
}

type typeInner = Inner.tInner
//                     ^def