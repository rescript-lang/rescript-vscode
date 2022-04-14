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
  let abc = (x:t, _y:int) => x
  let abd = (x:t, _y:int) => x
}

let fa:ForAuto.t = 34
//^com fa->

//^com "hello"->Js.Dict.u

module O = {
  module Comp = {
    @react.component
    let make = (~first="", ~zoo=3, ~second) =>
      React.string(first ++ second ++ string_of_int(zoo))
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

let _ = Lib.foo(//~age,
//^com ~
~age=3, ~name="")

let someObj = {"name": "a", "age": 32}

//^com someObj["a

let nestedObj = {"x": {"y": {"name": "a", "age": 32}}}

//^com nestedObj["x"]["y"]["

let o : Obj.objT = assert false
//^com o["a

type nestedObjT = {"x": Obj.nestedObjT}
let no : nestedObjT = assert false
//^com no["x"]["y"]["

type r = {x:int, y:string}
type rAlias = r
let r:rAlias = assert false
// ^com r.

// ^com Obj.Rec.recordVal.

let myAmazingFunction = (x,y) => x+y

@react.component
let make = () => {
// ^com my
  <> </>
}

// ^com Obj.object["

let foo = {
  let x = {
    3
  }
  let y = 4
  let add = (a, b) =>
    switch a {
    | 3 => a + b
    | _ => 42
    }
  let z = assert false
  let _ = z
  module Inner = {
    type z = int
    let v = 44
  }
  exception MyException (int, string, float, array<Js.Json.t>)
  let _ = raise(MyException(2, "", 1.0, []))
  add((x: Inner.z), Inner.v + y)
}

exception MyOtherException

// ^com <O.

type aa= {x:int, name:string}
type bb = {aa:aa, w:int}
let q:bb = assert false
// ^com q.aa.
// ^com q.aa.n