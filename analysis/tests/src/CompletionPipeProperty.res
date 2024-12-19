module ObservablePoint = {
  type t = {
    mutable x: int,
    mutable y: int,
  }

  @send
  external setBoth: (t, float) => unit = "set"

  @send
  external set: (t, float, float) => unit = "set"
}

module Sprite = {
  type t = {
    anchor: ObservablePoint.t,
  }
}

let sprite : Sprite.t = %todo

// sprite.anchor.
//               ^com