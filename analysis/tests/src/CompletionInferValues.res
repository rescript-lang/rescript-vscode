let getBool = () => true
let getInt = () => 123

type someRecord = {name: string, age: int}

let someFnWithCallback = (cb: (~num: int, ~someRecord: someRecord, ~isOn: bool) => unit) => {
  let _ = cb
}

let reactEventFn = (cb: ReactEvent.Mouse.t => unit) => {
  let _ = cb
}

@val external getSomeRecord: unit => someRecord = "getSomeRecord"

// let x = 123; let aliased = x; aliased->f
//                                         ^com

// let x = getSomeRecord(); x.
//                            ^com

// let x = getSomeRecord(); let aliased = x; aliased.
//                                                   ^com

// someFnWithCallback((~someRecord, ~num, ~isOn) => someRecord.)
//                                                             ^com

// Broken because not using the first argument (argument context seems to pile on when they should be plucked off and new one added)
// let aliasedFn = someFnWithCallback; aliasedFn((~num, ~someRecord, ~isOn) => someRecord.)
//                                                                                        ^com

// reactEventFn(event => { event->pr });
//                                  ^com

module Div = {
  @react.component
  let make = (~onMouseEnter: option<JsxEvent.Mouse.t => unit>=?) => {
    let _ = onMouseEnter
    React.null
  }
}

// let _ = <div onMouseEnter={event => { event->pr }} />
//                                                ^com

// let _ = <Div onMouseEnter={event => { event->pr }} />
//                                                ^com
