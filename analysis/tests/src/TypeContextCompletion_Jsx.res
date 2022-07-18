type someVariant = One | Two | Three | Four | Five(int) | Six(option<string>, int)

type otherVariant = [#one | #two | #three | #four | #five(int) | #six(option<int>, int)]

let someValue = Two

module SomeComponent = {
  @react.component
  let make = (~someVariant, ~anotherThing: TypeDefinition.variant, ~thirdThing: otherVariant) => {
    ignore(someVariant)
    ignore(anotherThing)
    ignore(thirdThing)
    React.null
  }
}

// let x = <SomeComponent someVaria
//                                 ^com

// let x = <SomeComponent someVariant=
//                                    ^com

// let x = <SomeComponent someVariant=
//                                     ^com

// let x = <SomeComponent someVariant=T
//                                     ^com

// let x = <SomeComponent someVariant=t
//                                     ^com

// let x = <SomeComponent anotherThing=
//                                     ^com

// let x = <SomeComponent thirdThing=
//                                   ^com

// let x = <SomeComponent thirdThing=
//                                    ^com

// let x = <SomeComponent thirdThing=#t
//                                     ^com

// let x = <SomeComponent thirdThing=#T
//                                     ^com
