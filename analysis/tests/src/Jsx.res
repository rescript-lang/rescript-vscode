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
