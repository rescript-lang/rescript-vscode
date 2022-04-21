module M = {
  @react.component
  let make = (~first, ~fun="", ~second="") => React.string(first ++ fun ++ second)
}

let _ = <M first="abc" />
//       ^def

//^com <M second=fi

//^com <M second="abc" f

//^com let e = <M

@react.component
let make = (~first) => React.string(first)

let y = 44

//^com <M prop={A(3)} k

//^com <M prop=A(3) k

//^com <M prop=foo(1+2) k

//^com <M prop=list{1,2,3} k

//^com <M prop=<N /> k

//^com <M prop=1.5 k

//^com <M prop=0X33 k

//^com <M prop=12e+3 k

//^com <M prop='z' k

//^com <M prop=`before${foo}` k

//^com <M prop=module(@foo Three: X_int) k

//^com <M prop=%bs.raw("1") k

let _ = <Component />
//         ^def

module Ext = {
  @react.component @module("@material-ui/core")
  external make: (~align: string=?) => React.element = "Typography"
}

let _ = (Ext.make, Ext.makeProps)

//^com <Ext al

//^com <M first

//^com <M first=#a k

//^com <M first =  ?   #a k

//^com <M>

module WithChildren = {
  @react.component
  let make = (~name as _: string, ~children) => <jsx> children </jsx>
}

let _ = <WithChildren name=""> <div /> </WithChildren>
//^com <WithChildren
//^com <WithChildren n

//^com let c : React.e
//^com let c : ReactDOMR

module DefineSomeFields = {
  type r = {thisField: int, thatField: string}
  let thisValue = 10
  // ^com let foo x = x.th
}

// ^com let q = DefineSomeFields.

// ^com let foo x = x.DefineSomeFields.th

let _ = x => x.DefineSomeFields.thisField + DefineSomeFields.thisValue

module Outer = {
  module Inner = {
    let hello = 3
  }
}
let _ = Outer.Inner.hello

let _ =
  <div
  // ^com x=Outer.Inner.h
    name=""
  />

let _ =
  <div
  // ^com x=Outer.Inner.
    name=""  />


let _ =
  <div
  // ^com x=
    name=""
  />

