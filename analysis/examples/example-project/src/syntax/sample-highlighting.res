// Bindings
let numberBinding = 123

let someFunction = (param: int): int => {
  let innerBinding = param + 2
  innerBinding
}

// Types
type someRecord<'typeParameter> = {
  someField: int,
  someOtherField: string,
  theParam: typeParameter,
}

type someEnum =
  | SomeMember
  | AnotherMember
  | SomeMemberWithPayload(someRecord<int>)

// Destructuring
let destructuring = () => {
  let someVar = (1, 2, 3)
  let (one, two, three) = someVar
  let someObj: someRecord<int> = {
    someField: 1,
    someOtherField: "hello",
    theParam: 2,
  }
  let {someField, someOtherField, theParam} = someObj

  someField
}

// JSX
module SomeComponent = {
  @react.component
  let make = () => {
    React.null
  }
}

let jsx = <div> <SomeComponent /> </div>
