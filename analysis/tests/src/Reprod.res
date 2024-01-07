module Query = {
  let use = (~variables: QueryFile.Types.variables) => {
    ignore(variables)
    ""
  }
}

// let x = Query.use(~variables={location: ByAddress()})
//                                                   ^com

type nestedRecord = {nested: bool}

type rec someRecord = {
  first: int,
  second: (bool, option<someRecord>),
  optThird: option<[#first | #second(someRecord)]>,
  nest: nestedRecord,
}

type somePolyVariant = [#one | #two(bool) | #three(someRecord, bool)]

type someVariant = One | Two(bool) | Three(someRecord, bool)

let res: result<someVariant, somePolyVariant> = Ok(One)

// switch res { | Ok() }
//                   ^com

// switch res { | Error() }
//                      ^com
