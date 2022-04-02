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
