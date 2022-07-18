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
