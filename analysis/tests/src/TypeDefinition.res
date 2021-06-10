type variant = Foo | Bar

type record = {item: string}

let x = Foo
//  ^typ

let y = {item: "foo"}
//  ^typ

type obj = {"foo": string}

let obj: obj = {"foo": "bar"}
//  ^type

let f = r => r.item
//           ^typ

let g = v =>
  switch v {
  //     ^typ
  | Foo => "Foo"
  | Bar => "Bar"
  }
