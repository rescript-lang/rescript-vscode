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

@mainTypeForModule(SomeOtherModule)
type typeOutsideModule = {nname: string}

module SomeOtherModule = {
  type t = typeOutsideModule

  type irrelevantType = string

  @get external getNName: t => string = "nname"
  @get external getNName2: typeOutsideModule => string = "nname"
  @get external getNName3: irrelevantType => string = "nname"

  let thisShouldNotBeCompletedFor = () => "hi"
}

let nn: SomeOtherModule.t = {nname: "hello"}

// ^dv+
// nn.
//    ^com
// ^dv-

// @mainTypeForModule(SomeOthe) type typeOutsideModule = {nname: string}
//                            ^com
