// Used to check completions across files
// WRONG! Missing pipe functions (because of type t)
// CompletionFromModule.n.
//                        ^com

// CompletionFromModule.nn.
//                         ^com

// CompletionFromModule.nnn->
//                           ^com

open CompletionFromModule.SomeOtherModule
// CompletionFromModule.nnn->
//                           ^com
