module MyList = Belt.List
//^com MyList.m
//^com Array.
//^com Array.m


module Dep: {
  @ocaml.doc("Some doc comment") @deprecated("Use customDouble instead")
  let customDouble: int => int
} = {
  let customDouble = foo => foo * 2
}

//^com let cc = Dep.c