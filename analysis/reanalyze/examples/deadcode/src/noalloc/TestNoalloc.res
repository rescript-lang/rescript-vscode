@noalloc
let x = 34

@noalloc
let foo = (x, y) => x + y

@noalloc
let bar = x => foo(x, x) + 1

@noalloc
let pair = (x, y) => (x, y)

@noalloc
let unpair = ((x, y)) => x + y

@noalloc
let mixed = ((p0, p1, p2), (p3, (p4, p5, (p6, p7, p8)), p9)) => (
  p0 + p1 + p2,
  p3 + p4 + p5 + p6 + p7 + p8 + p9,
)

@noalloc
let duplicate = (x: (int, int)) => (x, x)

@noalloc
let local = n => {
  let a = 34
  a + n
}

@noalloc
let quad = x => {
  let a = (x, x + 1)
  (a, a)
}

@noalloc
let fl = 2.

@noalloc
let unpair2 = v => {
  let (x, y) = v
  x + y
}

@noalloc
let id = x => x

@noalloc
let id2 = (x: (int, int)) => id(x)

@noalloc
let y = x

@noalloc
let retGlobal = () => y + 1

@noalloc
let globalTuple = (1, 2, 3)

@noalloc
let extractFromGlobalTuple = () => {
  let (x, _, _) = globalTuple
  x
}

type r = {x: int, y: int, name: string}

@noalloc
let recordCreation = () => {
  let r = {x: 3, y: 4, name: "abcd"}
  (r.x, r.name)
}
