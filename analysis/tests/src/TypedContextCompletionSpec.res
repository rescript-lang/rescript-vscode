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
//                                  ^com

// Complete for record field another
// let {another} = someVal
//             ^com

// Complete for record field s
// let {anotherThing, s} = someVal
//                     ^com

// Complete for record fields of someVal
// let {} = someVal
//      ^com

// Complete for record fields of fn invocation
// let {} = getSomeVal(~irrelevant=123)
//      ^com

// Complete for record fields on record field something
// let {something: {}} = someVal
//                  ^com

// Complete for record fields of type for anotherLevel
// let {something: {whatIsThis, anotherLevel: {}}} = someVal
//                                             ^com

// Complete for record fields of type for anotherLevel, with prefix "l"
// let {something: {whatIsThis, anotherLevel: {l}}} = someVal
//                                              ^com

// Complete for record fields of type for something, with seen idents whatIsThis, anotherLevel
// let {something: {whatIsThis, anotherLevel,  }} = someVal
//                                             ^com

// Complete for record fields of type for something, with seen idents whatIsThis, anotherLevel
// let {something: {whatIsThis, ,anotherLevel}} = someVal
//                             ^com

// --- Record destructuring end ---

// --- Pattern matching start ---

// Complete for record field som in root record
// switch someVal { | {thirdThing: "123", som} => () }
//                                          ^com

// Complete for any record field in root record
// switch someVal { | {thirdThing: "1234"} => () | {} => () }
//                                                  ^com

// Complete for record field in type of root record->something
// switch someVal { | {thirdThing: "1234"} => () | {something: {whatIsThis, anotherLevel, }} => () }
//                                                                                       ^com

// Complete for variant starting with T in rfield something of root record
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

// Complete for root type of x
// switch x { | }
//             ^com

// Incomplete switch, no completion
// switch x {
//            ^com

// switch y { | }
//             ^com

// switch y { | One | Two | Three |  }
//                                  ^com

// Should not complete because the record has no braces
// switch someVal { | {something:  } => () }
//                               ^com

// Should not complete because the record has no braces
// switch someVal { |  => () }
//                    ^com

// No path, looking for record field
// switch someVal { | {} => () }
//                     ^com

// Should complete because the record has braces
// switch someVal { | {something} => () }
//                             ^com

// switch someVal { | {something: {}} => () }
//                                 ^com

// Should not complete because the record has no braces
// switch someVal { | {something: {el: } } => () }
//                                    ^com

// switch someOtherValue { | {other2: #S()} => () }
//                                       ^com

// switch someOtherValue { | {thirdStuff: Some(_, {something: t})} => () }
//                                                             ^com

// switch someOtherValue { | {thirdStuff: (_, {something: t})} => () }
//                                                         ^com

// Mixing matches in branches is still picked up as seen idents
// switch y { | One | Two => () | Three => () | T  }
//                                               ^com

// switch someVal { | {something: {whatIsThis: false |  } } => () }
//                                                     ^com

// Mixing matches in branches is still picked up as seen idents
// switch y { | One | Two => () | Three => () |   }
//                                               ^com

// switch someOtherValue { | {thirdStuff: } => () |   }
//                                       ^com

// --- Pattern matching end ---
