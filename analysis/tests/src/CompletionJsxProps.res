// let _ = <CompletionSupport.TestComponent on=
//                                             ^com

// let _ = <CompletionSupport.TestComponent on=t
//                                              ^com

// let _ = <CompletionSupport.TestComponent test=T
//                                                ^com

// let _ = <CompletionSupport.TestComponent polyArg=
//                                                  ^com

// let _ = <CompletionSupport.TestComponent polyArg=#t
//                                                    ^com

// let _ = <div muted= />
//                    ^com

// let _ = <div onMouseEnter= />
//                           ^com

// Should wrap in {}
// let _ = <CompletionSupport.TestComponent testArr=
//                                                  ^com

// Should not wrap in {}
// let _ = <CompletionSupport.TestComponent testArr={[]}
//                                                    ^com

