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