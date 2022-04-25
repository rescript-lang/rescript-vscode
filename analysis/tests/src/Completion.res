module MyList = Belt.List
// MyList.m
//         ^com
// Array.
//       ^com
// Array.m
//        ^com

module Dep: {
  @ocaml.doc("Some doc comment") @deprecated("Use customDouble instead")
  let customDouble: int => int
} = {
  let customDouble = foo => foo * 2
}

// let cc = Dep.c
//               ^com

module Lib = {
  let foo = (~age, ~name) => name ++ string_of_int(age)
  let next = (~number=0, ~year) => number + year
}

// let x = Lib.foo(~
//                  ^com

// [1,2,3]->m
//           ^com

// "abc"->toU
//           ^com

let op = Some(3)

// op->e
//      ^com

module ForAuto = {
  type t = int
  let abc = (x: t, _y: int) => x
  let abd = (x: t, _y: int) => x
}

let fa: ForAuto.t = 34
// fa->
//     ^com

// "hello"->Js.Dict.u
//                   ^com

module O = {
  module Comp = {
    @react.component
    let make = (~first="", ~zoo=3, ~second) => React.string(first ++ second ++ string_of_int(zoo))
  }
}

let zzz = 11

// let comp = <O.Comp second=z
//                            ^com

// let comp = <O.Comp z
//                     ^com

//^doc

// @reac
//      ^com

// @react.
//        ^com

// let x = Lib.foo(~name, ~
//                         ^com

// let x = Lib.foo(~age, ~
//                        ^com

// let x = Lib.foo(~age={3+4}, ~
//                              ^com

let _ = Lib.foo(
  //~age,
  //~
  // ^com
  ~age=3,
  ~name="",
)

let someObj = {"name": "a", "age": 32}

// someObj["a
//           ^com

let nestedObj = {"x": {"y": {"name": "a", "age": 32}}}

// nestedObj["x"]["y"]["
//                      ^com

let o: Obj.objT = assert false
// o["a
//     ^com

type nestedObjT = {"x": Obj.nestedObjT}
let no: nestedObjT = assert false
// no["x"]["y"]["
//               ^com

type r = {x: int, y: string}
type rAlias = r
let r: rAlias = assert false
// r.
//   ^com

// Obj.Rec.recordVal.
//                   ^com

let myAmazingFunction = (x, y) => x + y

@react.component
let make = () => {
  // my
  //   ^com
  <> </>
}

// Obj.object["
//             ^com

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
  exception MyException(int, string, float, array<Js.Json.t>)
  let _ = raise(MyException(2, "", 1.0, []))
  add((x: Inner.z), Inner.v + y)
}

exception MyOtherException

// <O.
//    ^com

type aa = {x: int, name: string}
type bb = {aa: aa, w: int}
let q: bb = assert false
// q.aa.
//      ^com
// q.aa.n
//       ^com

// Lis
//    ^com

module WithChildren = {
  @react.component
  let make = (~children, ~name as _: string) => <jsx> children </jsx>
}
// <WithChildren
//              ^com

// type t = Js.n
//              ^com
// type t = ForAuto.
//                  ^com

type z = Allo | Asterix | Baba

// let q = As
//           ^com

// module M = For
//               ^com

module Private = {
  %%private(let awr = 3)
  let b = awr
}

// Private.
//         ^com

module Shadow = {
  module A = {
    let shadowed = 3
  }
  module B = {
    let shadowed = ""
  }
}

// sha
//    ^com
open Shadow.A
// sha
//    ^com
open Shadow.B
// sha
//    ^com
let _ = shadowed

module FAR = {
  type forAutoRecord = {forAuto: ForAuto.t, something: option<int>}
  let forAutoRecord: forAutoRecord = assert false
}

module FAO = {
  let forAutoObject = {"forAutoLabel": FAR.forAutoRecord, "age": 32}
}

// FAO.forAutoObject["
//                    ^com

// FAO.forAutoObject["forAutoLabel"].
//                                   ^com

// FAO.forAutoObject["forAutoLabel"].forAuto->
//                                            ^com

// FAO.forAutoObject["forAutoLabel"].forAuto->ForAuto.a
//                                                     ^com

let name = "abc"
// let template = `My name is ${na}`
//                                ^com