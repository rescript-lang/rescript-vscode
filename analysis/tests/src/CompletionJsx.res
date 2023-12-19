let someString = "hello"
ignore(someString)

// someString->st
//               ^com

module SomeComponent = {
  @react.component
  let make = (~someProp) => {
    let someInt = 12
    let someArr = [React.null]
    ignore(someInt)
    ignore(someArr)
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
      // <di
      //    ^com
    </div>
  }
}

module CompWithoutJsxPpx = {
  type props = {name: string}

  let make = ({name}) => {
    ignore(name)
    React.null
  }
}

// <CompWithoutJsxPpx n
//                     ^com

// <SomeComponent someProp=>
//                         ^com
