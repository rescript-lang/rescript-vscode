type anotherLevel = {level: int}
type someRecord = {somethingElse: int, whatIsThis: bool, anotherLevel: anotherLevel}

type anotherRecord = {something: someRecord, anotherThing: option<someRecord>, thirdThing: string}

let someVal = {
  something: {somethingElse: 123, whatIsThis: false, anotherLevel: {level: 123}},
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

// Nested destructuring
// let {something: {}} = someVal
//                  ^com

// let {something: {whatIsThis, anotherLevel: {}}} = someVal
//                                             ^com

// let {something: {whatIsThis, anotherLevel: {l}}} = someVal
//                                              ^com
