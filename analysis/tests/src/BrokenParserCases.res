// I would want this to recover to {someProp: %rescript.exprhole, ...}, but it currently eats the tuple + parses to {someProp: otherProp}
// let _ = ({someProp: , otherProp: 123}, 123)
//                    ^ast

// Same problem here, does not recover from the missing prop, and ends up eating the rest of the record declaration while only parsing {firstProp: secondProp}
// let _ = someFunc({firstProp: , secondProp: 123, thirdProp: 123})
//                             ^ast

// I'd like the commas to be parsed as (broken) tuple "members" too. There's currently no robust way to derive _where_ in that tuple the cursor is, or even how many members the tuple has, because there's no pos match, and the empty comma item is just discarded.
// switch v { | (_, , _) => () }
//                 ^ast

// Same problem here, does not recover from the missing prop, and ends up eating the rest of the record declaration while only parsing {firstProp: secondProp}
// let {someField, , otherField, } = someVal
//                ^ast

