let mapArr = arr => arr->Js.Array2.map(v => v + 2)
let arr = [1, 2, 3]
// let _ = arr->
//              ^com

// let _ = arr->Js.Array2.map(v => v)->
//                                     ^com

// let _ = arr->Js.Array2.filter(v => v)->Js.Array2.filter(v => v)->
//                                                                  ^com

// let _ = arr->Js.Array2.filter(v => v)->
//                                        ^com

// let _ = arr->Js.Array2-map(v => v)->Belt.List.fromArray->
//                                                          ^com

// let _ = arr->Js.Array2-map(v => v)->Belt.List.fromArray->s
//                                                           ^com
