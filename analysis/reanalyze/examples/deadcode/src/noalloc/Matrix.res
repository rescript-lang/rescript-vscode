@noalloc
let sumVec = v => {
  let ((x1, x2), (y1, y2)) = v
  (x1 + y1, x2 + y2)
}

@noalloc
let rotation = a => ((0.0, -1.0 *. a, 0.0), (a, 0.0, 0.0), (0.0, 0.0, a))

@noalloc
let mulVecVec = (v1, v2) => {
  let (x1, y1, z1) = v1
  let (x2, y2, z2) = v2
  let x = x1 *. x2
  let y = y1 *. y2
  let z = z1 *. z2
  x +. y +. z
}

@noalloc
let mulMatVec = (m, v) => {
  let (row1, row2, row3) = m
  let x = mulVecVec(row1, v)
  let y = mulVecVec(row2, v)
  let z = mulVecVec(row3, v)
  (x, y, z)
}

@noalloc
let scale = s => ((s, 1.0, 1.0), (1.0, s, 1.0), (1.0, 1.0, s))

@noalloc
let restMatrix = v => mulMatVec(rotation(0.123), mulMatVec(scale(2.0), v))

@noalloc
let scale2 = ((2.0, 1.0, 1.0), (1.0, 2.0, 1.0), (1.0, 1.0, 2.0))

@noalloc
let restMatrix2 = v => mulMatVec(rotation(0.123), mulMatVec(scale2, v))
