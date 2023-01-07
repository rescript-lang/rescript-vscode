let someString = "hello"
ignore(someString)

// someString->st
//               ^com

module SomeComponent = {
  @react.component
  let make = (~someProp) => {
    let someInt = 12
    let someArr = [React.null]
    let someInvalidArr = [12]
    ignore(someInt)
    ignore(someArr)
    ignore(someInvalidArr)
    // someString->st
    //               ^com
    <div>
      {React.string(someProp)}
      <div> {React.null} </div>
      // {someString->st}
      //                ^com
      // {"Some string"->st}
      //                   ^com
      // {"Some string"->Js.String2.trim->st}
      //                                    ^com
      // {someInt->}
      //           ^com
      // {12->}
      //      ^com
      // {someArr->a}
      //            ^com
      // {someInvalidArr->a}
      //                   ^com
    </div>
  }
}
