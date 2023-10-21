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
  | Module(docsForModule)
  | ModuleAlias({id: string, docstrings: array<string>, name: string, items: array<item>})
and docsForModule = {
  id: string,
  docstrings: array<string>,
  deprecated?: string,
  name: string,
  items: array<item>,
}
type doc = {name: string, deprecated: option<string>, docstrings: array<string>, items: array<item>}
let decodeFromJson: Js.Json.t => doc
