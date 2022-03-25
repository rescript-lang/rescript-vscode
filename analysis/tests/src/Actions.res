type kind = First | Second | Third
type r = {name: string, age: int}

let _ = (kind, kindStr) => {
  let _ifThenElse = if kind == First {
    //              ^act
    "First"
  } else {
    "Not First"
  }

  let _ternary = #kind("First", {name: "abc", age: 3}) != kindStr ? "Not First" : "First"
  //             ^act
}
