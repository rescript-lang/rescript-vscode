type someVariant = One | Two | Three | Four | Five(int) | Six(option<string>)

let someValue = Two

let someVariantToString = (~someVariant) =>
  switch someVariant {
  | One => "One"
  | Two => "Two"
  | Three => "Three"
  | Four => "Four"
  | _ => "-"
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
