module Test = {
  type t = {name: int}
  let add = (ax: t) => ax.name + 1
  let addSelf = (ax: t) => {name: ax.name + 1}
  let make = (name: int): t => {name: name}
}
