// ^int

type r = {name: string, age: int}

let add = (~x, ~y) => x + y

@react.component
let make = (~name) => React.string(name)

@react.component
let otherComponentName = (~name) => React.string(name)

module Mod = {
  @react.component
  let make = (~name) => React.string(name)
}

module type ModTyp = {
  @react.component
  let make: (~name: string) => React.element
}

@module("path") external dirname: string => string = "dirname"

@module("path") @variadic
external join: array<string> => string = "join"

@val
external padLeft: (
  string,
  @unwrap
  [
    | #Str(string)
    | #Int(int)
  ],
) => string = "padLeft"

module RFS = {
  @module("fs")
  external readFileSync: (
    ~name: string,
    @string
    [
      | #utf8
      | @as("ascii") #useAscii
    ],
  ) => string = "readFileSync"
}

module Functor = () => {
  @react.component
  let make = (~name) => React.string(name)
}

module type FT = {
  module Functor: (
    X: {
      let a: int
      @react.component
      let make: (~name: string) => React.element
      let b: int
    },
    Y: ModTyp,
  ) =>
  {
    @react.component
    let make: (~name: string) => React.element
  }
}
