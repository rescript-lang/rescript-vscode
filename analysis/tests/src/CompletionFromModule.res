module SomeModule = {
  type t = {name: string}

  @get external getName: t => string = "name"

  let thisShouldNotBeCompletedFor = () => "hi"
}

let n = {SomeModule.name: "hello"}

// ^dv+
// n.
//   ^com
// ^dv-
