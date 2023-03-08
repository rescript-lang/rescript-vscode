module Test = {
  type t = {name: int}
  let add = (ax: t) => ax.name + 1
  let addSelf = (ax: t) => {name: ax.name + 1}
  let make = (name: int): t => {name: name}
}

type testVariant = One | Two | Three(int)

module TestComponent = {
  @react.component
  let make = (
    ~on: bool,
    ~test: testVariant,
    ~testArr: array<testVariant>,
    ~polyArg: option<[#one | #two | #two2 | #three(int, bool)]>=?,
  ) => {
    ignore(on)
    ignore(test)
    ignore(testArr)
    ignore(polyArg)
    React.null
  }
}

module Nested = {
  type config = {root: ReactDOM.Client.Root.t}
}
