module Test = {
  type t = {name: int}
  let add = (a: float) => a +. 1.0
}
let a: Test.t = {name: 4}
// a->
//    ^com
