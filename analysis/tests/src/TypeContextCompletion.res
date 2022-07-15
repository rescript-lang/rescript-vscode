type someVariant = One | Two | Three | Four

let someVariantToString = (~someConfig, ~otherRandomArg) =>
  switch someConfig {
  | One => "One " ++ otherRandomArg
  | Two => "Two"
  | Three => "Three"
  | Four => "Four"
  }

// let x = someVariantToString(~someConfi
//                                       ^com

// let x = someVariantToString(~someConfig=
//                                         ^com

// let x = someVariantToString(~someConfig=T
//                                          ^com

module SomeComponent = {
  @react.component
  let make = (~whatever) => {
    someVariantToString(~someConfig=whatever, ~otherRandomArg="123")->React.string
  }
}

// let jsx = <SomeComponent whatever=
//                                   ^com
