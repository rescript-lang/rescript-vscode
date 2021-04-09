let array l = "[" ^ (String.concat ", " l) ^ "]"

type markupContent = {
  kind: string;
  value: string;
}

type completionItem = {
  label: string;
  kind: int;
  tags: int list;
  detail: string;
  documentation: markupContent;
}

type hover = {
  contents: string;
}

let stringifyMarkupContent (m: markupContent) =
  Printf.sprintf {|{"kind": "%s", "value": "%s"}|}
  m.kind (String.escaped m.value)

let stringifyCompletionItem c =
  Printf.sprintf {|{
    "label": "%s",
    "kind": %i,
    "tags": %s,
    "detail": "%s",
    "documentation": %s
  }|}
  (String.escaped c.label)
  c.kind
  (c.tags |> List.map string_of_int |> array)
  (String.escaped c.detail)
  (stringifyMarkupContent c.documentation)

let stringifyHover h =
  Printf.sprintf {|{"contents": "%s"}|}
  (String.escaped h.contents)

let null = "null"
