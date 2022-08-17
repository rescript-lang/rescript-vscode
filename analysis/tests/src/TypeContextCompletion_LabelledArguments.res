type someVariant = One | Two | Three | Four | Five(int) | Six(option<string>, int)

type otherVariant = [#one | #two | #three | #four | #five(int) | #six(option<int>, int)]

let thisIsAValue = Two

let someVariantToString = (
  ~someVariant,
  ~anotherThing: TypeDefinition.variant,
  ~thirdThing: otherVariant,
) => {
  ignore(anotherThing)
  ignore(thirdThing)
  switch someVariant {
  | One => "One"
  | Two => "Two"
  | Three => "Three"
  | Four => "Four"
  | _ => "-"
  }
}

// let x = someVariantToString(~someVaria
//                                       ^com

// let x = someVariantToString(~someVariant=
//                                          ^com

// let x = someVariantToString(~someVariant=
//                                           ^com

// let x = someVariantToString(~someVariant=T
//                                           ^com

// let x = someVariantToString(~someVariant=t
//                                           ^com

// let x = someVariantToString(~someVariant=TypeDefinition.
//                                                         ^com

// let x = someVariantToString(~anotherThing=
//                                           ^com

// let x = someVariantToString(~thirdThing=
//                                         ^com

// let x = someVariantToString(~thirdThing=
//                                          ^com

// let x = someVariantToString(~thirdThing=#t
//                                           ^com

// let x = someVariantToString(~thirdThing=#T
//                                           ^com

type someRecord = {
  age: int,
  name: string,
  maybeVariant: option<someVariant>,
  definitivelyVariant: someVariant,
  someTup: (string, someVariant),
}

let doStuff = (~doThing: someRecord => unit) => {
  ignore(
    doThing({
      age: 123,
      name: "hello",
      maybeVariant: None,
      definitivelyVariant: One,
      someTup: ("123", One),
    }),
  )
}

// let _ = doStuff(~doThing=({age, n}) => {()})
//                                  ^com

let doMoreStuff = (~someRecord: someRecord): someRecord => someRecord

// doMoreStuff(~someRecord={someTup: ("123", O)})
//                                            ^com

// let _  = doMoreStuff(~someRecord={age: 123, name: "123", })
//                                                          ^com
