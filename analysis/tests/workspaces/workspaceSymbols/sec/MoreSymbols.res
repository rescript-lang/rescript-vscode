module Button = {
  module Comp = {
    @react.component
    let make = (~name="") => React.string(name)
  }
}