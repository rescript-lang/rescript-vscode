let v = (true, Some(false), (true, true))

let _ = switch v {
| (true, _, _) => 1
| _ => 2
}

// switch v {
//           ^com

// switch v { | }
//             ^com

// switch v { | (t, _) }
//                ^com

// switch v { | (_, _, (f, _)) }
//                       ^com

let x = true

// switch x { |
//             ^com

// switch x { | t
//               ^com

type nestedRecord = {nested: bool}

type rec someRecord = {
  first: int,
  second: (bool, option<someRecord>),
  optThird: option<[#first | #second(someRecord)]>,
  nest: nestedRecord,
}

let f: someRecord = {
  first: 123,
  second: (true, None),
  optThird: None,
  nest: {nested: true},
}

let z = (f, true)
ignore(z)

// switch f { | }
//             ^com

// switch f { | {}}
//               ^com

// switch f { | {first,  , second }}
//                      ^com

// switch f { | {fi}}
//                 ^com

// switch z { | ({o}, _)}
//                 ^com

// switch f { | {nest: }}
//                    ^com

// switch f { | {nest: {}}}
//                      ^com

let _ = switch f {
| {first: 123, nest} =>
  ()
  // switch nest { | {}}
  //                  ^com
  nest.nested
| _ => false
}

// let {} = f
//      ^com

// let {nest: {n}}} = f
//              ^com

type someVariant = One | Two(bool) | Three(someRecord, bool)

let z = Two(true)
ignore(z)

// switch z { | Two()}
//                  ^com

// switch z { | Two(t)}
//                   ^com

// switch z { | Three({})}
//                     ^com

// switch z { | Three({}, t)}
//                         ^com

type somePolyVariant = [#one | #two(bool) | #three(someRecord, bool)]
let b: somePolyVariant = #two(true)
ignore(b)

// switch b { | #two()}
//                   ^com

// switch b { | #two(t)}
//                    ^com

// switch b { | #three({})}
//                      ^com

// switch b { | #three({}, t)}
//                          ^com

let c: array<bool> = []
ignore(c)

// switch c { | }
//             ^com

// switch c { | [] }
//               ^com

let o = Some(true)
ignore(o)

// switch o { | Some() }
//                   ^com
