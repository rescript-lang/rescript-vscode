let myVar = true

// let myFunc = m
//               ^co2

type rec someVariant = One | Two | Three(bool, option<someVariant>)

// let myFunc: someVariant = O
//                            ^co2

// let myFunc: someVariant = Three(t)
//                                  ^co2

// let myFunc: someVariant = Three(true, So)
//                                         ^co2

// let myFunc: someVariant = Three(true, Some(O))
//                                             ^co2

type nestedRecord = {
  on: bool,
  off?: bool,
  maybeVariant?: someVariant,
}

type someRecord = {nested: option<nestedRecord>, variant: someVariant, someString: string}

// let myFunc: someRecord = {}
//                           ^co2

// let myFunc: someRecord = {n}
//                            ^co2

// let myFunc: someRecord = {variant: O}
//                                     ^co2

// let myFunc: someRecord = {nested: Some({maybeVariant: Three(false, So)})}
//                                                                      ^co2

// let myFunc: someRecord = {nested: Some({maybeVariant: One}), variant: }
//                                                                      ^co2

// let myFunc: someRecord = {nested: Some({maybeVariant: One, })}
//                                                           ^co2

// let myFunc: someRecord = {nested: Some({maybeVariant: One}), }
//                                                             ^co2

// This should reset the context, meaning it should just complete for the identifier
// let myFunc: someRecord = {nested: {maybeVariant: {let x = true; if x {}}}, }
//                                                                     ^co2

// This is the last expression - NOTE: This should work but it's doing a follow
// let myFunc: someRecord = {nested: {maybeVariant: {let x = true; if x {}}}, }
//                                                                       ^co2

// Complete as the last expression (looking for the record field type) - NOTE: This should work but it's doing a follow
// let myFunc: someRecord = {nested: {maybeVariant: {doStuff(); let x = true; if x {So}}}, }
//                                                                                    ^co2

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

// == Apply ==
// let x = if true && f {None}
//                     ^co2

// let x = someFunc(() => {let x = true; f})
//                                        ^co2

// let x = someFunc(~labelledArg=f)
//                                ^co2

// let x = someFunc(~labelledArg=)
//                               ^co2

// == Pipes ==
// let x = foo->id
//               ^co2

// let x = foo->
//              ^co2

// let x = foo->M
//               ^co2

// == Function arguments ==

let someFun = (~firstLabel, ~secondLabel=?, r: someRecord) => {
  firstLabel ++ secondLabel->Belt.Option.getWithDefault("") ++ r.someString
}

// let ff = someFun(~secondLabel, ~f)
//                                  ^co2

// let ff = someFun(~secondLabel, ~f)
//                                   ^co2

// == JSX ==
// let jsx = <SomeCom
//                   ^co2

// let jsx = <SomeModule.S
//                        ^co2

// let jsx = <Component
//                      ^co2

// let jsx = <Component a
//                       ^co2

// let jsx = <Component aProp=
//                            ^co2

// let jsx = <Component aProp={if true {}}
//                                      ^co2

// let jsx = <Component aProp={Stuff}
//                                 ^co2
