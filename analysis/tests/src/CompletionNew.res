let myVar = true

// let myFunc = m
//               ^co2

type rec someVariant = One | Two | Three(bool, option<someVariant>)

// let myFunc: someVariant = O
//                            ^co2

// let myFunc: someVariant = Three(t)
//                                  ^co2

// let myFunc: someVariant = Three(true, S)
//                                        ^co2

// let myFunc: someVariant = Three(true, Some(O))
//                                             ^co2

type nestedRecord = {
  on: bool,
  off?: bool,
  maybeVariant?: someVariant,
}

type someRecord = {nested: option<nestedRecord>, variant: someVariant}

// let myFunc: someRecord = {}
//                           ^co2

// let myFunc: someRecord = {n}
//                            ^co2

// let myFunc: someRecord = {variant: O}
//                                     ^co2

// let myFunc: someRecord = {nested: {maybeVariant: Three(false, t)}}
//                                                                ^co2

// let myFunc: someRecord = {nested: {maybeVariant: One}, variant: }
//                                                                ^co2

// let myFunc: someRecord = {nested: {maybeVariant: One, }}
//                                                      ^co2

// let myFunc: someRecord = {nested: {maybeVariant: One}, }
//                                                       ^co2

// This should reset the context, meaning it should just complete for the identifier
// let myFunc: someRecord = {nested: {maybeVariant: {let x = true; if x {}}}, }
//                                                                     ^co2

// This is the last expression
// let myFunc: someRecord = {nested: {maybeVariant: {let x = true; if x {}}}, }
//                                                                       ^co2

// Complete as the last expression (looking for the record field type)
// let myFunc: someRecord = {nested: {maybeVariant: {doStuff(); let x = true; if x {v}}}, }
//                                                                                   ^co2

// Complete on the identifier, no context
// let myFunc: someRecord = {nested: {maybeVariant: {doStuff(); let x = true; if x {v}}}, }
//                                                        ^co2

type fn = (~name: string=?, string) => bool

// let getBool = (name): bool =>
//                              ^co2

// let someFun: fn = (str, ~name) => {}
//                                    ^co2

// let someFun: fn = (str, ~name) => {let whatever = true; if whatever {}}
//                                                                      ^co2

// A let binding with an annotation. Reset to annotated constraint.
// let someFun: fn = (str, ~name) => {let whatever: bool = t}
//                                                          ^co2

// A let binding without annotation. Point to inferred type if it has compiled.
// let someFun: fn = (str, ~name) => {let whatever = t}
//                                                    ^co2

// == Let binding patterns ==
// let someVar: bool =
//                    ^co2

// let {someField: s } = someRecordVar
//                  ^co2

// == Tuple patterns ==
// let (true, ) = someRecordVar
//           ^co2

// let (true, true,  , false) = someRecordVar
//                  ^co2

// ==  Arrays ==
// let [ ] = someArr
//       ^co2

// let [(true, [false, ])] = someArr
//                    ^co2

// let [(true, [false, f])] = someArr
//                      ^co2
