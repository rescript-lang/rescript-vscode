type someVariant = One | Two | Three | Four | Five(int) | Six(option<string>)

let someVariantToString = (~someVariant) =>
  switch someVariant {
  | One => "One"
  | Two => "Two"
  | Three => "Three"
  | Four => "Four"
  | _ => "-"
  }

module SomeComponent = {
  @react.component
  let make = (~whatever) => {
    someVariantToString(~someVariant=whatever)->React.string
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

// let jsx = <SomeComponent whatever=
//                                   ^com
