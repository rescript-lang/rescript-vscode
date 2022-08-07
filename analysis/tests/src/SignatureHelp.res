type someVariant = One | Two | Three

/** Does stuff. */
let someFunc = (one: int, ~two: option<string>=?, ~three: unit => unit, ~four: someVariant, ()) => {
  ignore(one)
  ignore(two)
  ignore(three())
  ignore(four)
}

let otherFunc = (first: string, second: int, third: float) => {
  ignore(first)
  ignore(second)
  ignore(third)
}

// let _ = someFunc(
//                  ^she

// let _ = someFunc(1
//                   ^she

// let _ = someFunc(123, ~two
//                           ^she

// let _ = someFunc(123, ~two=
//                             ^she

// let _ = someFunc(123, ~two="123"
//                               ^she

// let _ = someFunc(123, ~two="123", ~four
//                                    ^she

// let _ = someFunc(123, ~two="123", ~four=O
//                                        ^she

// let _ = otherFunc(
//                   ^she

// let _ = otherFunc("123"
//                      ^she

// let _ = otherFunc("123",
//                          ^she

// let _ = otherFunc("123", 123
//                            ^she

// let _ = otherFunc("123", 123,
//                               ^she

// let _ = otherFunc("123", 123, 123.0)
//                                 ^she

// let _ = Completion.Lib.foo(~age
//                               ^she

let iAmSoSpecial = (iJustHaveOneArg: string) => {
  ignore(iJustHaveOneArg)
}

// let _ = iAmSoSpecial(
//                      ^she
