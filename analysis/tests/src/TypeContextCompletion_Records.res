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
type rec someVariant =
  One | Two | Three(int) | Four(someOtherVariant) | Five(someOtherVariant, someVariant)

type someRecordWithVariant = {
  other: someRecord,
  other2: option<someRecord>,
  something: someVariant,
  otherThing: option<someVariant>,
  thirdStuff: (someRecord, someVariant, option<someVariant>, int),
  fourthStuff: (option<[#WithPayload(someVariant)]>, someVariant),
}

let someOtherValue: someRecordWithVariant = {
  other: someVal.something,
  other2: None,
  something: Two,
  otherThing: None,
  thirdStuff: (someVal.something, One, None, 1),
  fourthStuff: (None, One),
}

// switch someOtherValue { | {something: Two | T} => () }
//                                              ^com

// switch someOtherValue { | {otherThing: Some(T)} => () }
//                                              ^com

// switch someOtherValue { | {thirdStuff: (_, T)} => () }
//                                             ^com

// switch someOtherValue { | {thirdStuff: (_, Four())} => () }
//                                                 ^com

// switch someOtherValue { | {thirdStuff: (_, Five(_, O))} => () }
//                                                     ^com

// switch someOtherValue { | {fourthStuff: (Some(#WithPayload(O)), _)} => () }
//                                                             ^com

// switch someOtherValue { | {thirdStuff: (_, Five(_, One | Two | T))} => () }
//                                                                 ^com

// switch someOtherValue { | {otherThing: S} => () }
//                                         ^com

// switch someOtherValue { | {other2: S} => () }
//                                     ^com

// switch someOtherValue { | {thirdStuff: (_, Four(TwentyFive | ))} => () }
//                                                              ^com

// switch someOtherValue { | {other2: } => () }
//                                    ^com

// switch someVal { | {something: {whatIsThis: false |  } } => () }
//                                                     ^com

// switch someVal { | {something: {whatIsThis: } } => () }
//                                            ^com

// switch someVal { | {something: {whatIsThis: fa  } } => () }
//                                               ^com

let x = Some(One)

// switch x { | }
//             ^com

// switch x {
//            ^com

let y = One

// switch y { | }
//             ^com

// switch y { | One | Two | Three |  }
//                                  ^com

// Should not complete because the record has no braces
// switch someVal { | {something:  } => () }
//                               ^com

// Should complete because the record has braces
// switch someVal { | {something: {} } => () }
//                                 ^com
