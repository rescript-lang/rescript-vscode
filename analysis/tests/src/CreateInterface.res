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
  let make = () => React.null
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

module NormaList = List
open Belt
module BeltList = List

module type MT2 = ModTyp

module rec RM: ModTyp = D
and D: ModTyp = Mod

module type OptT = {
  @react.component
  let withOpt1: (~x: int=?, ~y: int) => int

  @react.component
  let withOpt2: (~x: int=?, ~y: int) => int

  @react.component
  let withOpt3: (~x: option<int>, ~y: int) => int
}

module Opt = {
  @react.component
  let withOpt1 = (~x=3, ~y) => x + y

  @react.component
  let withOpt2 = (~x=?, ~y) =>
    switch x {
    | None => 0
    | Some(x) => x
    } +
    y

  @react.component
  let withOpt3 = (~x, ~y) =>
    switch x {
    | None => 0
    | Some(x) => x
    } +
    y
}

module Opt2: OptT = Opt
module Opt3 = Opt
