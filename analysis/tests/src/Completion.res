module MyList = Belt.List
// MyList.m
//         ^co2
// Array.
//       ^co2
// Array.m
//        ^co2

module Dep: {
  @ocaml.doc("Some doc comment") @deprecated("Use customDouble instead")
  let customDouble: int => int
} = {
  let customDouble = foo => foo * 2
}

// let cc = Dep.c
//               ^co2

module Lib = {
  let foo = (~age, ~name) => name ++ string_of_int(age)
  let next = (~number=0, ~year) => number + year
}

// let x = Lib.foo(~
//                  ^co2

// [1,2,3]->m
//           ^co2

// "abc"->toU
//           ^co2

let op = Some(3)

// op->e
//      ^co2

module ForAuto = {
  type t = int
  let abc = (x: t, _y: int) => x
  let abd = (x: t, _y: int) => x
}

let fa: ForAuto.t = 34
// fa->
//     ^co2

// "hello"->Js.Dict.u
//                   ^co2

module O = {
  module Comp = {
    @react.component
    let make = (~first="", ~zoo=3, ~second) => React.string(first ++ second ++ string_of_int(zoo))
  }
}

let zzz = 11

// let comp = <O.Comp second=z
//                            ^co2

// let comp = <O.Comp z
//                     ^co2

//^doc

// @reac
//      ^co2

// @react.
//        ^co2

// let x = Lib.foo(~name, ~
//                         ^co2

// let x = Lib.foo(~age, ~
//                        ^co2

// let x = Lib.foo(~age={3+4}, ~
//                              ^co2

let _ = Lib.foo(
  //~age,
  //~
  // ^co2
  ~age=3,
  ~name="",
)

let someObj = {"name": "a", "age": 32}

// someObj["a
//           ^co2

let nestedObj = {"x": {"y": {"name": "a", "age": 32}}}

// nestedObj["x"]["y"]["
//                      ^co2

let o: Obj.objT = assert false
// o["a
//     ^co2

type nestedObjT = {"x": Obj.nestedObjT}
let no: nestedObjT = assert false
// no["x"]["y"]["
//               ^co2

type r = {x: int, y: string}
type rAlias = r
let r: rAlias = assert false
// r.
//   ^co2

// Obj.Rec.recordVal.
//                   ^co2

let myAmazingFunction = (x, y) => x + y

@react.component
let make = () => {
  // my
  //   ^co2
  <> </>
}

// Obj.object["
//             ^co2

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
//    ^co2

type aa = {x: int, name: string}
type bb = {aa: aa, w: int}
let q: bb = assert false
// q.aa.
//      ^co2
// q.aa.n
//       ^co2

// Lis
//    ^co2

module WithChildren = {
  @react.component
  let make = (~children, ~name as _: string) => <jsx> children </jsx>
}
// <WithChildren
//              ^co2

// type t = Js.n
//              ^co2
// type t = ForAuto.
//                  ^co2

type z = Allo | Asterix | Baba

// let q = As
//           ^co2

// module M = For
//               ^co2

module Private = {
  %%private(let awr = 3)
  let b = awr
}

// Private.
//         ^co2

module Shadow = {
  module A = {
    let shadowed = 3
  }
  module B = {
    let shadowed = ""
  }
}

// sha
//    ^co2
open Shadow.A
// sha
//    ^co2
open Shadow.B
// sha
//    ^co2
let _ = shadowed

module FAR = {
  type forAutoRecord = {forAuto: ForAuto.t, something: option<int>}
  let forAutoRecord: forAutoRecord = assert false
}

module FAO = {
  let forAutoObject = {"forAutoLabel": FAR.forAutoRecord, "age": 32}
}

// FAO.forAutoObject["
//                    ^co2

// FAO.forAutoObject["forAutoLabel"].
//                                   ^co2

// FAO.forAutoObject["forAutoLabel"].forAuto->
//                                            ^co2

// FAO.forAutoObject["forAutoLabel"].forAuto->ForAuto.a
//                                                     ^co2
