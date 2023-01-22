module X = {
  //   ^hov
  let \"Foo" = "foo"
}

// let foo = X.
//             ^com

module type Y = {
  //        ^hov
  let \"Foo": string
}

type x = {\"Foo": string}
//   ^hov

let f = (x: x) => {
  // let {} = x
  //      ^com
  ignore(x)
}

let g = (~\"Foo") => \"Foo"
//  ^hov

// g(~)
//    ^com

module C = {
  //   ^hov
  @react.component
  let make = (~\"Foo": option<string>=?) =>
    switch \"Foo" {
    | Some(foo) => React.string(foo)
    | None => React.null
    }
}

let _ = <C />
//         ^com 
