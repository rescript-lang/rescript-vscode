type anotherLevel = {level: int, someOtherArg: bool}
type someRecord = {somethingElse: int, whatIsThis: bool, anotherLevel: anotherLevel}

type anotherRecord = {something: someRecord, anotherThing: option<someRecord>, thirdThing: string}

let someVal = {
  something: {
    somethingElse: 123,
    whatIsThis: false,
    anotherLevel: {level: 123, someOtherArg: true},
  },
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

// let {something: {whatIsThis, anotherLevel,  }} = someVal
//                                             ^com

// let {something: {whatIsThis, ,anotherLevel}} = someVal
//                             ^com

// switch someVal { | {thirdThing: "123", som} => () }
//                                          ^com

// switch someVal { | {thirdThing: "1234"} => () | {} => () }
//                                                  ^com

// switch someVal { | {thirdThing: "1234"} => () | {something: {whatIsThis, anotherLevel, }} => () }
//                                                                                       ^com

type someOtherVariant = TwentyFive | SixtyTwo
type rec someVariant = One | Two | Three(int) | Four(someOtherVariant)

type someRecordWithVariant = {
  other: someRecord,
  something: someVariant,
  otherThing: option<someVariant>,
  thirdStuff: (someRecord, someVariant, option<someVariant>, int),
}

let someOtherValue: someRecordWithVariant = {
  other: someVal.something,
  something: Two,
  otherThing: None,
  thirdStuff: (someVal.something, One, None, 1),
}
// switch someOtherValue { | {something: T} => () }
//                                        ^com

// switch someOtherValue { | {otherThing: Some(T)} => () }
//                                              ^com

// switch someOtherValue { | {thirdStuff: (_, T)} => () }
//                                             ^com

// switch someOtherValue { | {thirdStuff: (_, Four())} => () }
//                                                 ^com
