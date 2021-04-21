let abc = 22 + 34
//  ^hov

type t = (int, float)
//   ^hov

module Id = {
//     ^hov
  type x = int
}

@ocaml.doc("This module is commented")
module Dep: {
  @ocaml.doc("Some doc comment")
  let customDouble: int => int
} = {
  let customDouble = foo => foo * 2
}

module D = Dep
//         ^hov

let cd = D.customDouble
//         ^hov

module HoverInsideModuleWithComponent = {
  let x = 2 // check that hover on x works
//    ^hov
  @react.component
  let make = () => React.null
}

@ocaml.doc("Doc comment for functionWithTypeAnnotation")
let functionWithTypeAnnotation : unit => int = () => 1
//  ^hov

@react.component
let make = (~name) => React.string(name)
//           ^hov

@react.component
let make2 = (~name:string) => React.string(name)
//           ^hov