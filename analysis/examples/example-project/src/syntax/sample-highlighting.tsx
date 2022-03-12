// Bindings
let numberBinding = 123;

let someFunction = (param: number): number => {
  let innerBinding = param + 2;
  return innerBinding;
};

// Types
type someRecord<typeParameter> = {
  someField: number;
  someOtherField: string;
  theParam: typeParameter;
};

enum someEnum {
  SomeMember,
  AnotherMember,
}

// Destructuring
let destructuring = () => {
  let someVar = [1, 2, 3];
  let [one, two, three] = someVar;
  let someObj: someRecord<number> = {
    someField: 1,
    someOtherField: "hello",
    theParam: 2,
  };
  let { someField, someOtherField, theParam } = someObj;

  return someField;
};

// JSX
const SomeComponent = () => {
  return null;
};

let jsx = (
  <div>
    <SomeComponent />
  </div>
);
