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
   Printf.sprintf {|{"line": "%i", "character": "%i"}|} p.line p.character

let stringifyRange r =
   Printf.sprintf {|{"start": "%s", "end": "%s"}|}
    (stringifyPosition r.start)
    (stringifyPosition r.end_)

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

let stringifyLocation h =
  Printf.sprintf {|{"uri": "%s", "range": "%s"}|}
  (String.escaped h.uri)
  (stringifyRange h.range)

let null = "null"
