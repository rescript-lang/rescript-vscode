type kind = First | Second | Third
type r = {name: string, age: int}

let ret = _ => assert false
let kind = assert false

if kind == First {
  // ^xfm
  ret("First")
} else {
  ret("Not First")
}

#kind("First", {name: "abc", age: 3}) != kind ? ret("Not First") : ret("First")
//             ^xfm

let name = "hello"
//   ^xfm

let annotated: int = 34
//   ^xfm

module T = {
  type r = {a: int, x: string}
}

let foo = x =>
  switch x {
  | None => 33
  | Some(q) => q.T.a + 1
  //     ^xfm
  }
