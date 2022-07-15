type someVariant = One | Two | Three | Four

let someVariantToString = (~someVariant) =>
  switch someVariant {
  | One => "One"
  | Two => "Two"
  | Three => "Three"
  | Four => "Four"
  }

// let x = someVariantToString(~someVaria
//                                       ^com

// let x = someVariantToString(~someVariant=
//                                          ^com

// let x = someVariantToString(~someVariant=T
//                                           ^com

module SomeComponent = {
  @react.component
  let make = (~whatever) => {
    someVariantToString(~someVariant=whatever)->React.string
  }
}

// let jsx = <SomeComponent whatever=
//                                   ^com