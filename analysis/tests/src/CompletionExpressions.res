let s = true
let f = Some([false])

// switch (s, f) { | }
//                  ^com

type otherRecord = {
  someField: int,
  otherField: string,
}

type rec someRecord = {
  age: int,
  offline: bool,
  online: option<bool>,
  variant: someVariant,
  polyvariant: somePolyVariant,
  nested: option<otherRecord>,
}
and someVariant = One | Two | Three(int, string)
and somePolyVariant = [#one | #two(bool) | #three(someRecord, bool)]

let fnTakingRecord = (r: someRecord) => {
  ignore(r)
}

// let _ = fnTakingRecord({})
//                         ^com

// let _ = fnTakingRecord({n})
//                          ^com

// let _ = fnTakingRecord({offline: })
//                                 ^com

// let _ = fnTakingRecord({age: 123, })
//                                  ^com

// let _ = fnTakingRecord({age: 123,  offline: true})
//                                   ^com

// let _ = fnTakingRecord({age: 123, nested: })
//                                          ^com

// let _ = fnTakingRecord({age: 123, nested: {}})
//                                            ^com

// let _ = fnTakingRecord({age: 123, nested: Some({})})
//                                                 ^com

// let _ = fnTakingRecord({age: 123, variant: })
//                                           ^com

// let _ = fnTakingRecord({age: 123, variant: O })
//                                             ^com

// let _ = fnTakingRecord({age: 123, polyvariant: #three() })
//                                                       ^com

// let _ = fnTakingRecord({age: 123, polyvariant: #three({}, ) })
//                                                          ^com

// let _ = fnTakingRecord({age: 123, polyvariant: #three({}, t) })
//                                                            ^com

let fnTakingArray = (arr: array<option<bool>>) => {
  ignore(arr)
}

// let _ = fnTakingArray()
//                       ^com

// let _ = fnTakingArray([])
//                        ^com

// let _ = fnTakingArray(s)
//                        ^com

// let _ = fnTakingArray([Some()])
//                             ^com

// let _ = fnTakingArray([None, ])
//                             ^com

// let _ = fnTakingArray([None, , None])
//                             ^com

let someBoolVar = true

// let _ = fnTakingRecord({offline: so })
//                                    ^com

let fnTakingOtherRecord = (r: otherRecord) => {
  ignore(r)
}

// let _ = fnTakingOtherRecord({otherField: })
//                                         ^com

type recordWithOptionalField = {
  someField: int,
  someOptField?: bool,
}

let fnTakingRecordWithOptionalField = (r: recordWithOptionalField) => {
  ignore(r)
}

// let _ = fnTakingRecordWithOptionalField({someOptField: })
//                                                       ^com
type recordWithOptVariant = {someVariant: option<someVariant>}

let fnTakingRecordWithOptVariant = (r: recordWithOptVariant) => {
  ignore(r)
}

// let _ = fnTakingRecordWithOptVariant({someVariant: })
//                                                   ^com

type variantWithInlineRecord =
  WithInlineRecord({someBoolField: bool, otherField: option<bool>, nestedRecord: otherRecord})

let fnTakingInlineRecord = (r: variantWithInlineRecord) => {
  ignore(r)
}

// let _ = fnTakingInlineRecord(WithInlineRecord())
//                                               ^com

// let _ = fnTakingInlineRecord(WithInlineRecord({}))
//                                                ^com

// let _ = fnTakingInlineRecord(WithInlineRecord({s}))
//                                                 ^com

// let _ = fnTakingInlineRecord(WithInlineRecord({nestedRecord: }))
//                                                             ^com

// let _ = fnTakingInlineRecord(WithInlineRecord({nestedRecord: {} }))
//                                                               ^com

type variant = First | Second(bool)

let fnTakingCallback = (
  cb: unit => unit,
  cb2: bool => unit,
  cb3: ReactEvent.Mouse.t => unit,
  cb4: (~on: bool, ~off: bool=?, variant) => int,
  cb5: (bool, option<bool>, bool) => unit,
  cb6: (~on: bool=?, ~off: bool=?, unit) => int,
) => {
  let _ = cb
  let _ = cb2
  let _ = cb3
  let _ = cb4
  let _ = cb5
  let _ = cb6
}

// fnTakingCallback()
//                  ^com

// fnTakingCallback(a)
//                   ^com

// fnTakingCallback(a, )
//                    ^com

// fnTakingCallback(a, b, )
//                       ^com

// fnTakingCallback(a, b, c, )
//                           ^com

// fnTakingCallback(a, b, c, d, )
//                              ^com

// fnTakingCallback(a, b, c, d, e, )
//                                ^com

let something = {
  let second = true
  let second2 = 1
  ignore(second)
  ignore(second2)
  Js.log(s)
  //      ^com
}

let fff: recordWithOptionalField = {
  someField: 123,
  someOptField: true,
}

ignore(fff)

// fff.someOpt
//            ^com
