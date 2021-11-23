module MyList = Belt.List
//^com MyList.m
//^com Array.
//^com Array.m

module Dep: {
  @ocaml.doc("Some doc comment") @deprecated("Use customDouble instead")
  let customDouble: int => int
} = {
  let customDouble = foo => foo * 2
}

//^com let cc = Dep.c

module Lib = {
  let foo = (~age, ~name) => name ++ string_of_int(age)
  let next = (~number=0, ~year) => number + year
}

//^com let x = Lib.foo(~

//^com [1,2,3]->m

//^com "abc"->toU

let op = Some(3)

//^com op->e

module ForAuto = {
  type t = int
  let abc = (x: t, _y: int) => x
  let abd = (x: t, _y: int) => x
}

let fa: ForAuto.t = 34
//^com fa->

//^com "hello"->Js.Dict.u

module O = {
  module Comp = {
    @react.component
    let make = (~first="", ~zoo=3, ~second) => React.string(first ++ second ++ string_of_int(zoo))
  }
}

let zzz = 11

//^com let comp = <O.Comp second=z

//^com let comp = <O.Comp z

//^doc

//^com @reac

//^com @react.

//^com let x = Lib.foo(~name, ~

//^com let x = Lib.foo(~age, ~

//^com let x = Lib.foo(~age={3+4}, ~

let _ = Lib.foo(
  //~age,
  //^com ~
  ~age=3,
  ~name="",
)

let someObj = {"name": "a", "age": 32}

//^com someObj["a

let nestedObj = {"x": {"y": {"name": "a", "age": 32}}}

//^com nestedObj["x"]["y"]["
