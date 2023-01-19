type someVariant = One | Two | Three(option<bool>)
type somePolyVariant = [#one | #two | #three(option<bool>)]

let withSomeVariant = One
let withSomePoly: somePolyVariant = #one
let someBool = true
let someOpt = Some(true)

// switch withSomeVarian
//                      ^com

// switch withSomePol
//                   ^com

// switch someBoo
//               ^com

// switch someOp
//              ^com
