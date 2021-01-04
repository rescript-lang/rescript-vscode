let a = 12

let b = [1, 2, 3, a]

let c = <div />

let s = React.string

module M = {
  @react.component
  let make = (~x) => React.string(x)
}

let d = <M x="abc" />

module J = {
  @react.component
  export make = (~children: React.element) => React.null
}

let z = <J> {React.string("")} {React.string("")} </J>

type inline =
  | A({x: int, y: string})
  | B({x: int, y: string})
  | C({
      x: int,
      y: string,
      z: string,
      w: string,
      x0: string,
      q1: string,
      q2: string,
      q3: string,
      q4: string,
    })
  | D({x: int, y: string})
  | E({x: int, y: string})
  | F

module MSig: {
  type rec t = A(list<s>)
  and s = list<t>

  let x: int
} = {
  type rec t = A(list<s>)
  and s = list<t>

  let x = 14
}

module Impl = {
  type rec t = A(list<s>)
  and s = list<t>

  type w = int

  let x = 14
}

module Impl2 = {
  include Impl
}

module D = MSig
module E = Impl
module F = Impl2

@ocaml.doc("str docstring")
type str = string

@ocaml.doc("gr docstring")
type gr = {x: int, s: str}

let testRecordFields = (gr: gr) => {
  let str = gr.s
  str
}

@ocaml.doc("vr docstring")
type vr = | V1 | V2

let v1 = V1