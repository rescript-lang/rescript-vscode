type kind = First | Second | Third
type r = {name: string, age: int}

let ret = _ => assert false
let kind = assert false

if kind == First {
  // ^xfm
  ret("First")
} else {
  ret("Not First")
}

#kind("First", {name: "abc", age: 3}) != kind ? ret("Not First") : ret("First")
//             ^xfm
