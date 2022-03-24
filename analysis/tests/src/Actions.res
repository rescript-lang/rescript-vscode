type kind = First | Second | Third

let _ = (kind, kindStr) => {
  let _ifThenElse = if kind == First {
    //    ^act
    "First"
  } else {
    "Not First"
  }

  let _ternary = "First" != kindStr ? "Not First" : "First"
  //     ^act
}
