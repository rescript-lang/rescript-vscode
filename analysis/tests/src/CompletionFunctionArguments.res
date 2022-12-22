let someFn = (~isOn) => {
  if isOn {
    "on"
  } else {
    "off"
  }
}

// let _ = someFn(~isOn=)
//                      ^com

// let _ = someFn(~isOn=t)
//                       ^com

let _ = someFn(
  ~isOn={
    // switch someFn(~isOn=)
    //                     ^com
    true
  },
)

let someOtherFn = (includeName, age) => {
  "Hello" ++ (includeName ? " Some Name" : "") ++ ", you are age " ++ Belt.Int.toString(age)
}

// let _ = someOtherFn(f)
//                      ^com
