type field = {
  name: string,
  docstrings: array<string>,
  signature: string,
  optional: bool,
  deprecated?: string,
}

@tag("kind")
type constructorPayload = | @as("inlineRecord") InlineRecord({fields: array<field>})

type constructor = {
  name: string,
  docstrings: array<string>,
  signature: string,
  deprecated?: string,
  payload?: constructorPayload,
}

@tag("kind")
type detail =
  | @as("record") Record({items: array<field>})
  | @as("variant") Variant({items: array<constructor>})

@tag("kind")
type rec item =
  | @as("value")
  Value({
      id: string,
      docstrings: array<string>,
      signature: string,
      name: string,
      deprecated?: string,
    })
  | @as("type")
  Type({
      id: string,
      docstrings: array<string>,
      signature: string,
      name: string,
      deprecated?: string,
      /** Additional documentation for constructors and record fields, if available. */
      detail?: detail,
    })
  | @as("module")
  Module({
      id: string,
      docstrings: array<string>,
      deprecated?: string,
      name: string,
      items: array<item>,
    })
  | @as("moduleAlias")
  ModuleAlias({
      id: string,
      docstrings: array<string>,
      name: string,
      items: array<item>,
    })

type doc = {
  name: string,
  deprecated: option<string>,
  docstrings: array<string>,
  items: array<item>,
}

/**
`decodeFromJson(json)` parse JSON generated from `restool doc` command
*/
external decodeFromJson: Js.Json.t => doc = "%identity"
