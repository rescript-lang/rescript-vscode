type someVariant = One | Two | Three

let someFunc = (one: int, ~two: option<string>=?, ~three: int, ~four: someVariant, ()) => {
  ignore(one)
  ignore(two)
  ignore(three)
  ignore(four)
}

// someFunc(
//          ^she
