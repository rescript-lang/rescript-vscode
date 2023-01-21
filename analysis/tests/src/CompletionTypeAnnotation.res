type someRecord = {
  age: int,
  name: string,
}

type someVariant = One | Two(bool)

type somePolyVariant = [#one | #two(bool)]

// let x: someRecord =
//                    ^com

// let x: someRecord = {}
//                      ^com

// let x: someVariant =
//                     ^com

// let x: someVariant = O
//                       ^com

// let x: somePolyVariant =
//                         ^com

// let x: somePolyVariant = #o
//                            ^com
