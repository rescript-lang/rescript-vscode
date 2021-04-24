let array l = "[" ^ (String.concat ", " l) ^ "]"

type position = {
  line: int;
  character: int;
}

type range = {
  start: position;
  end_: position;
}

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

type location = {
  uri: string;
  range: range;
}

let stringifyPosition p =
   Printf.sprintf {|{"line": %i, "character": %i}|} p.line p.character

let stringifyRange r =
   Printf.sprintf {|{"start": %s, "end": %s}|}
    (stringifyPosition r.start)
    (stringifyPosition r.end_)

let stringifyMarkupContent (m: markupContent) =
  Printf.sprintf {|{"kind": "%s", "value": "%s"}|}
  m.kind (Json.escape m.value)

let stringifyCompletionItem c =
  Printf.sprintf {|{
    "label": "%s",
    "kind": %i,
    "tags": %s,
    "detail": "%s",
    "documentation": %s
  }|}
  (Json.escape c.label)
  c.kind
  (c.tags |> List.map string_of_int |> array)
  (Json.escape c.detail)
  (stringifyMarkupContent c.documentation)

let stringifyHover h =
  Printf.sprintf {|{"contents": "%s"}|}
  (Json.escape h.contents)

let stringifyLocation h =
  Printf.sprintf {|{"uri": "%s", "range": %s}|}
  (Json.escape h.uri)
  (stringifyRange h.range)

let null = "null"
