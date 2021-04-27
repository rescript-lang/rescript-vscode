module A = {
  let makeX = () => (1, 2)

  let (xxx, _) = makeX()
}

let y = A.xxx
//        ^def
