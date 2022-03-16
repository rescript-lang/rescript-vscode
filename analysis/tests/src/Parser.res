module M = {
  module C = Component
}

let _c = <Component />

let _mc = <M.C />

let _d = <div />

let _d2 =
  <div>
    {React.string("abc")}
    <div> {React.string("abc")} </div>
    {React.string("abc")}
    {React.string("abc")}
  </div>

type pair<'x, 'y> = ('x, 'y)

type looooooooooooooooooooooooooooooooooooooong_int = int

type looooooooooooooooooooooooooooooooooooooong_string = string

type pairIntString = list<
  pair<
    looooooooooooooooooooooooooooooooooooooong_int,
    looooooooooooooooooooooooooooooooooooooong_string,
  >,
>

let _ = 3 < 4 || 3 > 4

module type MT = {
  module DDF: {

  }
}

module DDF: MT = {
  module DDF = {

  }
}

module XX = {
  module YY = {
    type t = int
  }
}

open XX.YY

type tt = t

// ^par

module T = {
  type someRecord<'typeParameter> = {
    someField: int,
    someOtherField: string,
    theParam: 'typeParameter,
  }

  type someEnum = A | B | C
}

let foo = x => x.T.someField

let add = (~hello as x, ~world) => x + world

let _ = add(~hello=3)

let _ = <div scale="abc"> <div /> </div>

module SomeComponent = {
  module Nested = {
    @react.component
    let make = (~children) => {
      <> {children} </>
    }
  }
}

let _ = <SomeComponent.Nested> <div /> </SomeComponent.Nested>

let _ = true
