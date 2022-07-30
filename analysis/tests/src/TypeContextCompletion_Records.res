type someRecord = {somethingElse: int, whatIsThis: bool}

type anotherRecord = {something: someRecord, anotherThing: option<someRecord>, thirdThing: string}

let someVal = {
  something: {somethingElse: 123, whatIsThis: false},
  anotherThing: None,
  thirdThing: "test",
}

// Simple destructuring
// let {another} = someVal
//             ^com

// let {anotherThing, s} = someVal
//                     ^com

// let {} = someVal
//      ^com

let getSomeVal = (~irrelevant: int) => {
  ignore(irrelevant)
  someVal
}

// Via function
// let {} = getSomeVal(~irrelevant=123)
//      ^com
