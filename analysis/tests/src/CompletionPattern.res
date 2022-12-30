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
//              ^com

// switch x { | t
//               ^com
