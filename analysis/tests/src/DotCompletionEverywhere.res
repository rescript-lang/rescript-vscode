let someObj = {
  "name": "hello",
  "age": 123,
}

// someObj.
//         ^com

// someObj.na
//           ^com

type rrr = {name: string}
let rrr = {name: "hello"}

// rrr.n
//      ^com

module SomeMod = {
  module SomeOtherMod = {
    type x

    @send external do: x => unit = "do"

    external xx: x = "xx"
  }
}

external x: SomeMod.SomeOtherMod.x = "x"

// x.
//   ^com

// SomeMod.SomeOtherMod.xx.
//                         ^com

module Sss = {
  type rrr = {name: string}
  let rrr = {name: "hello"}
  let do = rrr => rrr.name
}

// Sss.rrr.
//         ^com

@editor.completeFrom(DotCompletionEverywhere.X2)
type x2x2 = {namee: string}
let x2x2 = {namee: "hello"}

module X2 = {
  let stuff = x => x.namee
}

// x2x2.
//      ^com

let obj = {
  "name": "ReScript",
  "number": 1,
  "nothing": true,
}

// obj.
//     ^com

let arr = [1, 2, 3]

// arr.m
//      ^com
