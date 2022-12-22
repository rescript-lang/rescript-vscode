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
