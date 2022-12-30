let v = (true, Some(false))

// switch v {
//           ^com

// switch v { | }
//             ^com

// switch v { | (t, _) }
//                ^com

let x = true

// switch x { |
//              ^com

// switch x { | t
//               ^com
