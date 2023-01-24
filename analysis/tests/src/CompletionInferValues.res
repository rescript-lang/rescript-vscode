let getBool = () => true
let getInt = () => 123

type someRecord = {name: string, age: int}

let someFnWithCallback = (cb: (~num: int, ~someRecord: someRecord, ~isOn: bool) => unit) => {
  let _ = cb
}

let reactEventFn = (cb: someRecord => unit) => {
  let _ = cb
}

@val external getSomeRecord: unit => someRecord = "getSomeRecord"

// let x = 123; let aliased = x; aliased->f
//                                         ^com

// let x = getSomeRecord(); x.
//                            ^com

// let x = getSomeRecord(); let aliased = x; aliased.
//                                                   ^com
