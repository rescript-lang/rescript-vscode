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

let getSomeVal = (~irrelevant: int) => {
  ignore(irrelevant)
  someVal
}

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

let x = Some(One)

let y = One

// --- Labelled arguments start ---

// Should complete the labelled argument name
// let x = someVariantToString(~someVaria
//                                      ^ast

// Should complete the value for the someVariant arg
// let x = someVariantToString(~someVariant=
//                                         ^ast

// Same as above but with additional space
// let x = someVariantToString(~someVariant=
//                                           ^^ast

// Complete for variant T
// let x = someVariantToString(~someVariant=T
//                                          ^ast

// Complete for identifier t
// let x = someVariantToString(~someVariant=t
//                                          ^ast

// Complete for things in TypeDefinition. Factor in type?
// let x = someVariantToString(~someVariant=TypeDefinition.
//                                                        ^ast

// Complete for arg anotherThing's type
// let x = someVariantToString(~anotherThing=
//                                          ^ast

// Complete for thirdThing's type
// let x = someVariantToString(~thirdThing=
//                                        ^ast

// Complete for polyvariant #t
// let x = someVariantToString(~thirdThing=#t
//                                          ^ast

// Complete for polyvariant #t
// let x = someVariantToString(~thirdThing=#T
//                                          ^ast

// --- Labelled arguments end ---

// --- Record destructuring start ---

// Complete for destructured record field starting with n
// let _ = doStuff(~doThing=({age, n}) => {()})
//                                  ^ast

// Complete for record field another
// let {another} = someVal
//             ^ast

// Complete for record field s
// let {anotherThing, s} = someVal
//                     ^ast

// Complete for record fields of someVal
// let {} = someVal
//      ^ast

// Complete for record fields of fn invocation
// let {} = getSomeVal(~irrelevant=123)
//      ^ast

// Complete for record fields on record field something
// let {something: {}} = someVal
//                  ^ast

// Complete for record fields of type for anotherLevel
// let {something: {whatIsThis, anotherLevel: {}}} = someVal
//                                             ^ast

// Complete for record fields of type for anotherLevel, with prefix "l"
// let {something: {whatIsThis, anotherLevel: {l}}} = someVal
//                                              ^ast

// Complete for record fields of type for something, with seen idents whatIsThis, anotherLevel
// let {something: {whatIsThis, anotherLevel,  }} = someVal
//                                             ^ast

// Complete for record fields of type for something, with seen idents whatIsThis, anotherLevel
// let {something: {whatIsThis, ,anotherLevel}} = someVal
//                             ^ast

// --- Record destructuring end ---

// --- Pattern matching start ---

// Complete for record field som in root record
// switch someVal { | {thirdThing: "123", som} => () }
//                                          ^ast

// Complete for any record field in root record
// switch someVal { | {thirdThing: "1234"} => () | {} => () }
//                                                  ^ast

// Complete for record field in type of root record->something
// switch someVal { | {thirdThing: "1234"} => () | {something: {whatIsThis, anotherLevel, }} => () }
//                                                                                       ^ast

// Complete for variant starting with T in rfield something of root record
// switch someOtherValue { | {something: Two | T} => () }
//                                              ^ast

// switch someOtherValue { | {otherThing: Some(T)} => () }
//                                              ^ast

// switch someOtherValue { | {thirdStuff: (_, T)} => () }
//                                             ^ast

// switch someOtherValue { | {thirdStuff: (_, Four())} => () }
//                                                 ^ast

// switch someOtherValue { | {thirdStuff: (_, Five(_, O))} => () }
//                                                     ^ast

// switch someOtherValue { | {fourthStuff: (Some(#WithPayload(O)), _)} => () }
//                                                             ^ast

// switch someOtherValue { | {thirdStuff: (_, Five(_, One | Two | T))} => () }
//                                                                 ^ast

// switch someOtherValue { | {otherThing: S} => () }
//                                         ^ast

// switch someOtherValue { | {other2: S} => () }
//                                     ^ast

// switch someOtherValue { | {thirdStuff: (_, Four(TwentyFive | ))} => () }
//                                                              ^ast

// switch someOtherValue { | {other2: } => () }
//                                    ^ast

// switch someVal { | {something: {whatIsThis: false |  } } => () }
//                                                     ^ast

// switch someVal { | {something: {whatIsThis: } } => () }
//                                            ^ast

// switch someVal { | {something: {whatIsThis: fa  } } => () }
//                                               ^ast

// Complete for root type of x
// switch x { | }
//             ^ast

// Incomplete switch, no completion
// switch x {
//            ^ast

// switch y { | }
//             ^ast

// switch y { | One | Two | Three |  }
//                                  ^ast

// Should not complete because the record has no braces
// switch someVal { | {something:  } => () }
//                               ^ast

// Should not complete because the record has no braces
// switch someVal { |  => () }
//                    ^ast

// Should complete because the record has braces
// switch someVal { | {something: {} } => () }
//                                 ^ast

// --- Pattern matching end ---
