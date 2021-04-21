module M = {
  @react.component
  let make = (~first, ~fun="", ~second="") => React.string(first ++ fun++ second)
}

let d = <M first="abc" />
//       ^def

//^com <M second=fi 


//^com <M second="abc" f
