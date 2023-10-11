type field = {
  name: string,
  docstrings: array<string>,
  signature: string,
  optional: bool,
  deprecated: option<string>,
}
type constructor = {
  name: string,
  docstrings: array<string>,
  signature: string,
  deprecated: option<string>,
}
type detail = Record(array<field>) | Variant(array<constructor>)
type rec item =
  | Value({
      id: string,
      docstrings: array<string>,
      signature: string,
      name: string,
      deprecated: option<string>,
    })
  | Type({
      id: string,
      docstrings: array<string>,
      signature: string,
      name: string,
      deprecated: option<string>,
      detail: option<detail>,
    })
  | Module(docsForModule)
  | ModuleAlias({id: string, docstrings: array<string>, name: string, items: array<item>})
and docsForModule = {
  id: string,
  docstrings: array<string>,
  deprecated: option<string>,
  name: string,
  items: array<item>,
}
type doc = {name: string, deprecated: option<string>, docstrings: array<string>, items: array<item>}
let decodeFromJson: Js.Json.t => doc
