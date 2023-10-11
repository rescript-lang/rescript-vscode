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

@tag("kind")
type detail =
  | @as("record") Record(array<field>)
  | @as("variant") Variant(array<constructor>)

@tag("kind")
type rec item =
  | @as("value")
  Value({
      id: string,
      docstrings: array<string>,
      signature: string,
      name: string,
      deprecated: option<string>,
    })
  | @as("type")
  Type({
      id: string,
      docstrings: array<string>,
      signature: string,
      name: string,
      deprecated: option<string>,
      /** Additional documentation for constructors and record fields, if available. */
      detail: option<detail>,
    })
  | @as("module") Module(docsForModule)
  | @as("moduleAlias")
  ModuleAlias({
      id: string,
      docstrings: array<string>,
      name: string,
      items: array<item>,
    })
and docsForModule = {
  id: string,
  docstrings: array<string>,
  deprecated: option<string>,
  name: string,
  items: array<item>,
}

let decodeDocstrings = item => {
  open Js.Json
  switch item->Js.Dict.get("docstrings") {
  | Some(j) =>
    switch j {
    | Array(arr) =>
      arr->Js.Array2.map(s =>
        switch s {
        | String(s) => s
        | _ => assert(false)
        }
      )
    | _ => assert(false)
    }
  | None => []
  }
}

let decodeStringByField = (item, field) => {
  open Js.Json
  switch item->Js.Dict.get(field) {
  | Some(j) =>
    switch j {
    | String(s) => s
    | _ => assert(false)
    }
  | None => assert(false)
  }
}

let decodeDepreacted = item => {
  open Js.Json
  switch item->Js.Dict.get("deprecated") {
  | Some(j) =>
    switch j {
    | String(j) => Some(j)
    | _ => assert(false)
    }

  | None => None
  }
}

let decodeRecordFields = fields => {
  open Js.Json
  let fields = fields->Js.Array2.map(field => {
    switch field {
    | Object(doc) => {
        let name = doc->decodeStringByField("name")
        let docstrings = doc->decodeDocstrings
        let signature = doc->decodeStringByField("signature")
        let deprecated = doc->decodeDepreacted
        let optional = switch Js.Dict.get(doc, "optional") {
        | Some(value) =>
          switch value {
          | Boolean(bool) => bool
          | _ => assert(false)
          }
        | None => assert(false)
        }

        {name, docstrings, signature, optional, deprecated}
      }

    | _ => assert(false)
    }
  })
  Record(fields)
}

let decodeConstructorFields = fields => {
  open Js.Json
  let fields = fields->Js.Array2.map(field => {
    switch field {
    | Object(doc) => {
        let name = doc->decodeStringByField("name")
        let docstrings = doc->decodeDocstrings
        let signature = doc->decodeStringByField("signature")
        let deprecated = doc->decodeDepreacted

        {name, docstrings, signature, deprecated}
      }

    | _ => assert(false)
    }
  })
  Variant(fields)
}

let decodeDetail = detail => {
  open Js.Json

  switch detail {
  | Object(detail) =>
    switch (detail->Js.Dict.get("kind"), detail->Js.Dict.get("items")) {
    | (Some(kind), Some(items)) =>
      switch (kind, items) {
      | (String(kind), Array(items)) =>
        switch kind {
        | "record" => decodeRecordFields(items)
        | "variant" => decodeConstructorFields(items)
        | _ => assert(false)
        }

      | _ => assert(false)
      }
    | _ => assert(false)
    }

  | _ => assert(false)
  }
}

let rec decodeValue = item => {
  let id = item->decodeStringByField("id")
  let signature = item->decodeStringByField("signature")
  let name = item->decodeStringByField("name")
  let deprecated = item->decodeDepreacted
  let docstrings = item->decodeDocstrings
  Value({id, docstrings, signature, name, deprecated})
}
and decodeType = item => {
  let id = item->decodeStringByField("id")
  let signature = item->decodeStringByField("signature")
  let name = item->decodeStringByField("name")
  let deprecated = item->decodeDepreacted
  let docstrings = item->decodeDocstrings
  let detail = switch item->Js_dict.get("detail") {
  | Some(field) => decodeDetail(field)->Some
  | None => None
  }
  Type({id, docstrings, signature, name, deprecated, detail})
}
and decodeModuleAlias = item => {
  open Js.Json
  let id = item->decodeStringByField("id")
  let name = item->decodeStringByField("name")
  let docstrings = item->decodeDocstrings
  let items = switch Js.Dict.get(item, "items") {
  | Some(items) =>
    switch items {
    | Array(arr) => arr->Js.Array2.map(i => decodeItem(i))
    | _ => assert(false)
    }
  | None => assert(false)
  }
  ModuleAlias({id, items, name, docstrings})
}
and decodeModule = item => {
  open Js.Json
  let id = item->decodeStringByField("id")
  let name = item->decodeStringByField("name")
  let deprecated = item->decodeDepreacted
  let docstrings = item->decodeDocstrings
  let items = switch Js.Dict.get(item, "items") {
  | Some(items) =>
    switch items {
    | Array(arr) => arr->Js.Array2.map(i => decodeItem(i))
    | _ => assert(false)
    }
  | None => assert(false)
  }
  Module({id, name, docstrings, deprecated, items})
}
and decodeItem = item => {
  open Js.Json
  switch item {
  | Object(value) =>
    switch Js.Dict.get(value, "kind") {
    | Some(kind) =>
      switch kind {
      | String(type_) =>
        switch type_ {
        | "type" => decodeType(value)
        | "value" => decodeValue(value)
        | "module" => decodeModule(value)
        | "moduleAlias" => decodeModuleAlias(value)
        | _ => assert(false)
        }

      | _ => assert(false)
      }

    | None => assert(false)
    }

  | _ => assert(false)
  }
}

type doc = {
  name: string,
  deprecated: option<string>,
  docstrings: array<string>,
  items: array<item>,
}

/**
`decodeFromJson(json)` parse JSON generated from `restool doc` command
*/
let decodeFromJson = json => {
  open Js.Json

  switch json {
  | Object(mod) => {
      let name = mod->decodeStringByField("name")
      let deprecated = mod->decodeDepreacted
      let docstrings = mod->decodeDocstrings
      let items = switch Js.Dict.get(mod, "items") {
      | Some(items) =>
        switch items {
        | Array(arr) => arr->Js.Array2.map(i => decodeItem(i))
        | _ => assert(false)
        }

      | None => assert(false)
      }

      {name, deprecated, docstrings, items}
    }

  | _ => assert(false)
  }
}
