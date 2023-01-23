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

type someFunc = (int, string) => bool

// let x: someFunc =
//                  ^com

type someTuple = (bool, option<bool>)

// let x: someTuple =
//                   ^com

// let x: someTuple = (true, )
//                          ^com

// let x: option<someVariant> =
//                             ^com
