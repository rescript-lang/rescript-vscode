module A = {
  let makeX = () => (1, 2)

  let (xxx, yyy) = makeX()

  type t = {name:string, age:int}
  
  let makeT = () => {name:"", age:0}

  let {name, age} = makeT()

  let (a | a, b) = makeX()

  let [v1,v2,v3] = [1,2,3]

  let lazy lazyy = lazy 3
}

let y = A.xxx
//        ^def

let z = A.yyy

let n = A.name
//         ^def

let n = A.a
//        ^def

let n = A.v1
//        ^def

let n = A.lazyy
//        ^def
