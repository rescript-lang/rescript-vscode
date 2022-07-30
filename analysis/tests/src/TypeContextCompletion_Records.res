type someRecord = {somethingElse: int, whatIsThis: bool}

type anotherRecord = {something: someRecord, anotherThing: option<someRecord>, thirdThing: string}

let someVal = {
  something: {somethingElse: 123, whatIsThis: false},
  anotherThing: None,
  thirdThing: "test",
}

// let value: anotherRecord = {
//   anotherThing: None,
//   some
//       ^com

// let {another} = someVal
//             ^com
