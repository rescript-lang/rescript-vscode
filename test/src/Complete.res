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

//^com let x = foo(~

//^com [1,2,3]->m

//^com "abc"->toU

let op = Some(3)

//^com op->e

module ForAuto = {
  type t = int
  let abc = (x:t, _y:int) => x
  let abd = (x:t, _y:int) => x
}

let fa:ForAuto.t = 34
//^com fa->

