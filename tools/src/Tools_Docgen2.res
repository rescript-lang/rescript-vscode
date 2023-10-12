type recordField = {
  name: string,
  deprecated?: string,
  optional: bool,
  docstrings: array<string>,
  signature: string,
}

type constructor = {
  name: string,
  deprecated?: string,
  optional: bool,
  docstrings: array<string>,
  signature: string,
}

@tag("kind")
type typeDetail =
  | @as("record") Record({items: array<recordField>})
  | @as("variant") Variant({items: array<constructor>})

@tag("kind")
type rec docItem =
  | @as("value")
  Value({
      id: string,
      name: string,
      deprecated?: string,
      signature: string,
      docstrings: array<string>,
    })
  | @as("type")
  Type({
      id: string,
      name: string,
      deprecated?: string,
      signature: string,
      detail?: typeDetail,
    })
  | @as("module") Module({id: string, name: string, items: array<docItem>})
  | @as("moduleAlias")
  ModuleAlias({
      id: string,
      name: string,
      docstrings: array<string>,
      items: array<docItem>,
    })

type docsForModule = {
  name: string,
  deprecated?: string,
  docstrings: array<string>,
  items: array<docItem>,
}
