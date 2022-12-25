let someFn = (~isOn, ~isOff=false, ()) => {
  if isOn && !isOff {
    "on"
  } else {
    "off"
  }
}

let tLocalVar = false

// let _ = someFn(~isOn=)
//                      ^com

// let _ = someFn(~isOn=t)
//                       ^com

// let _ = someFn(~isOff=)
//                       ^com

let _ = someFn(
  ~isOn={
    // switch someFn(~isOn=)
    //                     ^com
    true
  },
)

let someOtherFn = (includeName, age, includeAge) => {
  "Hello" ++
  (includeName ? " Some Name" : "") ++
  ", you are age " ++
  Belt.Int.toString(includeAge ? age : 0)
}

// let _ = someOtherFn(f)
//                      ^com

module OIncludeMeInCompletions = {}

type someVariant = One | Two | Three(int, string)

let someFnTakingVariant = (
  configOpt: option<someVariant>,
  ~configOpt2=One,
  ~config: someVariant,
) => {
  ignore(config)
  ignore(configOpt)
  ignore(configOpt2)
}

// let _ = someFnTakingVariant(~config=)
//                                     ^com

// let _ = someFnTakingVariant(~config=O)
//                                      ^com

// let _ = someFnTakingVariant(S)
//                              ^com

// let _ = someFnTakingVariant(~configOpt2=O)
//                                          ^com

// --- UNIMPLEMENTED CASES ---
// The following two cases does not complete to a values because of ambiguity - should it complete for a value, or for a named argument? We don't know whether the function has args or not when deciding, since we're just in the AST at that point. Can potentially be fixed in the future.
// let _ = someOtherFn()
//                     ^com
// let _ = someOtherFn(1, 2, )
//                          ^com

// --- BROKEN PARSER CASES ---
// This below demonstrates an issue when what you're completing is the _last_ labelled argument, and there's a unit application after it. The parser wrongly merges the unit argument as the expression of the labelled argument assignment, where is should really let the trailing unit argument be, and set a %rescript.exprhole as the expression of the assignment, just like it normally does.
// let _ = someFn(~isOff=, ())
//                       ^com
