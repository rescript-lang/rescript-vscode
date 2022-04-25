module M = {
  @react.component
  let make = (~first, ~fun="", ~second="") => React.string(first ++ fun ++ second)
}

let _ = <M first="abc" />
//       ^def

// <M second=fi
//             ^co2


// <M second="abc" f
//                  ^co2


// let e = <M
//           ^co2

@react.component
let make = (~first) => React.string(first)

let y = 44

// <M prop={A(3)} k
//                 ^co2

// <M prop=A(3) k
//               ^co2

// <M prop=foo(1+2) k
//                   ^co2

// <M prop=list{1,2,3} k
//                      ^co2

// <M prop=<N /> k
//                ^co2

// <M prop=1.5 k
//              ^co2

// <M prop=0X33 k
//               ^co2

// <M prop=12e+3 k
//                ^co2

// <M prop='z' k
//              ^co2

// <M prop=`before${foo}` k
//                         ^co2

// <M prop=module(@foo Three: X_int) k
//                                    ^co2

// <M prop=%bs.raw("1") k
//                       ^co2

let _ = <Component />
//         ^def

module Ext = {
  @react.component @module("@material-ui/core")
  external make: (~align: string=?) => React.element = "Typography"
}

let _ = (Ext.make, Ext.makeProps)

// <Ext al
//        ^co2

// <M first
//         ^co2

// <M first=#a k
//              ^co2

// <M first =  ?   #a k
//                     ^co2

// <M>
//    ^co2

module WithChildren = {
  @react.component
  let make = (~name as _: string, ~children) => <jsx> children </jsx>
}

let _ = <WithChildren name=""> <div /> </WithChildren>
// <WithChildren
//              ^co2
// <WithChildren n
//                ^co2

// let c : React.e
//                ^co2
// let c : ReactDOMR
//                  ^co2

module DefineSomeFields = {
  type r = {thisField: int, thatField: string}
  let thisValue = 10
  // let foo x = x.th
  //                 ^co2
}

// let q = DefineSomeFields.
//                          ^co2
// let foo x = x.DefineSomeFields.th
//                                  ^co2

let _ = x => x.DefineSomeFields.thisField + DefineSomeFields.thisValue

module Outer = {
  module Inner = {
    let hello = 3
  }
}
let _ = Outer.Inner.hello

let _ =
  <div
  // x=Outer.Inner.h
  //                ^co2
    name=""
  />

let _ =
  <div
  // x=Outer.Inner.
  //               ^co2
    name=""
  />

let _ =
  <div
  // x=
  //   ^co2
    name=""
  />
