type m = {name: string}

module M1 = {
  type t = m

  let handle = (m: t) => {
    assert false
  }
}

module M2 = {
  type t = m

  let handle = (m: t) => {
    assert false
  }
}

let m: m = {
  name: "zm",
}

let m1: M1.t = {
  name: "zm",
}

let m2: M2.t = m1

// m1->
//     ^com
