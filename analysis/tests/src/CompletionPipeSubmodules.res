module A = {
  module B1 = {
    type b1 = B1
    type t = b1 // TODO(pipe-filter) Should be allowed without needing type t
    let xx = B1
    let d = (_: t) => ""
  }
  module B2 = {
    let yy = 20
  }
  type t = {v: B1.t} // TODO(pipe-filter) Should be allowed without needing type t
  let x = {v: B1.B1}
}

// let _ = A.B1.xx->
//                  ^com
// b1 seen from B1 is A.B1.b1

// let _ = A.x.v->
//                ^com
// B1.b1 seen from A  is A.B1.b1

module C = {
  type t = C
  let do = (_: t) => ""
}

module D = {
  module C2 = {
    type t2 = C2
    type t = t2 // TODO(pipe-filter) Should be allowed without needing type t
    let do = (_: t) => ""
  }

  type d = {v: C.t, v2: C2.t2}
  let d = {v: C.C, v2: C2.C2}
}

module E = {
  type e = {v: D.d}
  let e = {v: D.d}
}

// let _ = E.e.v.v->
//                  ^com
// C.t seen from D is C.t

// let _ = E.e.v.v2->
//                   ^com
// C2.t2 seen from D is D.C2.t2
