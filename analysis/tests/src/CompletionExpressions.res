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
