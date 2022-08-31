let abc = 22 + 34
//  ^hov

type t = (int, float)
//   ^hov

module Id = {
  //   ^hov
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
  //  ^hov
  @react.component
  let make = () => React.null
}

@ocaml.doc("Doc comment for functionWithTypeAnnotation")
let functionWithTypeAnnotation: unit => int = () => 1
//  ^hov

@react.component
let make = (~name) => React.string(name)
//           ^hov

@react.component
let make2 = (~name: string) => React.string(name)
//           ^hov

let num = 34
//        ^hov

module type Logger = {
  //         ^hov
  let log: string => unit
}

module JsLogger: Logger = {
  //   ^hov
  let log = (msg: string) => Js.log(msg)
  let _oneMore = 3
}

module JJ = JsLogger
//            ^def

module IdDefinedTwice = {
  //     ^hov
  let _x = 10
  let y = 20
  let _x = 10
}

module A = {
  let x = 13
}

module B = A
//     ^hov

module C = B
//     ^hov

module Comp = {
  @react.component
  let make = (~children: React.element) => children
}

module Comp1 = Comp

let _ =
  <Comp>
    <div />
    <div />
  </Comp>
//        ^hov

let _ =
  <Comp1>
    <div />
    <div />
  </Comp1>
//        ^hov

type r<'a> = {i: 'a, f: float}

let _get = r => r.f +. r.i
//                       ^hov

let withAs = (~xx as yyy) => yyy + 1
//                   ^hov

module AA = {
  type cond<'a> = [< #str(string)] as 'a
  let fnnxx = (b: cond<_>) => true ? b : b
}

let funAlias = AA.fnnxx

let typeOk = funAlias
//              ^hov

let typeDuplicate = AA.fnnxx
//                       ^hov

@live let dd = 34
// ^hov

let arity0a = (. ()) => {
  //^hov
  let f = () => 3
  f
}

let arity0b = (. (), . ()) => 3
//  ^hov

let arity0c = (. (), ()) => 3
//  ^hov

let arity0d = (. ()) => {
  // ^hov
  let f = () => 3
  f
}

/**doc comment 1*/
let docComment1 = 12
//       ^hov

/** doc comment 2 */
let docComment2 = 12
//    ^hov

module ModWithDocComment = {
  /*** module level doc comment 1 */

  /** doc comment for x */
  let x = 44

  /*** module level doc comment 2 */
}

module TypeSubstitutionRecords = {
  type foo<'a> = {content: 'a, zzz: string}
  type bar = {age: int}
  type foobar = foo<bar>

  let x1: foo<bar> = {content: {age: 42}, zzz: ""}
  //                   ^hov
  let x2: foobar = {content: {age: 42}, zzz: ""}
  //                  ^hov

  // x1.content.
  //            ^com

  // x2.content.
  //            ^com
}
