module Query = {
  let use = (~variables: QueryFile.Types.variables) => {
    ignore(variables)
    ""
  }
}

// let x = Query.use(~variables={location: ByAddress()})
//                                                   ^com
