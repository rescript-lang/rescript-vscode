let crossRef = References.x
//               ^ref

let crossRef2 = References.x


module Ref = References

let crossRef3 = References.x


let crossRefWithInterface = ReferencesWithInterface.x
//                             ^ref

let crossRefWithInterface2 = ReferencesWithInterface.x

module RefWithInterface = ReferencesWithInterface

let crossRefWithInterface3 = ReferencesWithInterface.x
