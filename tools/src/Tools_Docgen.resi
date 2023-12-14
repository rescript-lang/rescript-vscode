type field = {
  name: string,
  docstrings: array<string>,
  signature: string,
  optional: bool,
  deprecated?: string,
}
type constructor = {
  name: string,
  docstrings: array<string>,
  signature: string,
  deprecated?: string,
}
type detail = Record({items: array<field>}) | Variant({items: array<constructor>})
type rec item =
  | Value({
      id: string,
      docstrings: array<string>,
      signature: string,
      name: string,
      deprecated?: string,
    })
  | Type({
      id: string,
      docstrings: array<string>,
      signature: string,
      name: string,
      deprecated?: string,
      detail?: detail,
    })
  | Module({
      id: string,
      docstrings: array<string>,
      deprecated?: string,
      name: string,
      items: array<item>,
    })
  | ModuleAlias({id: string, docstrings: array<string>, name: string, items: array<item>})

type doc = {name: string, deprecated: option<string>, docstrings: array<string>, items: array<item>}

let decodeValue: Js.Dict.t<Js.Json.t> => item
let decodeType: Js.Dict.t<Js.Json.t> => item
let decodeModule: Js.Dict.t<Js.Json.t> => item
let decodeModuleAlias: Js.Dict.t<Js.Json.t> => item
let decodeItem: Js.Json.t => item
let decodeFromJson: Js.Json.t => doc
