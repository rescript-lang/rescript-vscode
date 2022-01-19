let a = 4
let _ = a
let a = ""
let _ = a
module Test = {
  type t = {name: int}
  let add = (a: t) => a.name + 1
}
let a: Test.t = {name: 4}
//^com a->