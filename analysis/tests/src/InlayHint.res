let string = "ReScript"
let number = 1
let float = 1.1
let char = 'c'

let add = (x, y) => x + y

let my_sum = 3
  -> add(1)
  -> add(1)
  -> add(1)
  -> add(8)

let withAs = (~xx as yyy) => yyy + 1

let arity0a = (. ()) => {
  //^hov
  let f = () => 3
  f
}

let arity0b = (. (), . ()) => 3

type t = (int, float)

@ocaml.doc("This module is commented")
module Dep: {
  @ocaml.doc("Some doc comment")
  let customDouble: int => int
} = {
  let customDouble = foo => foo * 2
}

module D = Dep

let functionWithTypeAnnotation: unit => int = () => 1

@react.component
let make = (~name) => React.string(name)

let tuple = ("ReScript", "lol")

let (lang, _) = tuple