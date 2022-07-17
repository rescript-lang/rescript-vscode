type someVariant = One | Two | Three | Four | Five(int) | Six(option<string>)

let someValue = Two

let someVariantToString = (
  ~someVariant,
  ~anotherThing: TypeDefinition.variant,
  ~thirdThing: option<int>,
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

// let x = someVariantToString(~anotherThing=
//                                           ^com
