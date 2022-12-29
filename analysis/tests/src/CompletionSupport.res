module Test = {
  type t = {name: int}
  let add = (ax: t) => ax.name + 1
  let addSelf = (ax: t) => {name: ax.name + 1}
  let make = (name: int): t => {name: name}
}

type testVariant = One | Two | Three(int)

module TestComponent = {
  @react.component
  let make = (~on: bool, ~test: testVariant) => {
    ignore(on)
    ignore(test)
    React.null
  }
}
