// ^db+
let _ = Belt.List.map
//                  ^def

let _ = List.map
//            ^def

open Js
module Before = {
  open Belt
  let _ = Id.getCmpInternal
}
module Inner = {
  // eqN
  //    ^co2
  open List
  let _ = map
}
// ^db-
