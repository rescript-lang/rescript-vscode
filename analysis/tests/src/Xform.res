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
  //      ^xfm
  switch x {
  | None => 33
  | Some(q) => q.T.a + 1
  //     ^xfm
  }

let withAs = (~x as name) => name + 1
//                   ^xfm

@react.component
let make = (~name) => React.string(name)
//   ^xfm

let _ = (~x) => x + 1
//       ^xfm

//
// Add braces to the body of a function
//

let noBraces = () => name
//                   ^xfm

let nested = () => {
  let _noBraces = (_x, _y, _z) => "someNewFunc"
  //                              ^xfm
}

let bar = () => {
  module Inner = {
    let foo = (_x, y, _z) =>
      switch y {
      | #some => 3
      | #stuff => 4
      }
    //^xfm
  }
  Inner.foo(1)
}

type readState = New | Unread | Read
//^xfm

type refState = readState

type account =
  | None
  | Instagram(string)
  | Facebook(string, int)
//^xfm

type person = {
  "age": int,
  "name": string
}
//^xfm

type user = {
  name: string,
  age: int,
} and response = Yes | No
//^xfm

type myType = This | That
//^xfm

let fun1 = (x: myType) => x

let fun2 = b => b ? This : That

let fun3 = b => b ? {name: "Lhs", age: 2} : {name: "Rhs", age: 3}

let fun4 = b => b ? Yes : No

let me: person = {
  "age": 5,
  "name": "Big ReScript"
}