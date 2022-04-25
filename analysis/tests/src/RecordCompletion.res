type t = {n: array<string>}

let t = {n: []}

type t2 = {n2: t}

let t2 = {n2: t}

// t.n->m
//       ^co2

// t2.n2.n->m
//           ^co2

module R = {
  type t = {name: string}
}

let n = {R.name: ""}
// n.R.
//     ^co2
