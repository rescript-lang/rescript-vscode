type position = {line : int; character : int}

type range = {start : position; end_ : position}

type markupContent = {kind : string; value : string}

type completionItem = {
  label : string;
  kind : int;
  tags : int list;
  detail : string;
  documentation : markupContent option;
}

type hover = {contents : string}

type location = {uri : string; range : range}

type documentSymbolItem = {name : string; kind : int; location : location}

let null = "null"

let array l = "[" ^ String.concat ", " l ^ "]"

let stringifyPosition p =
  Printf.sprintf {|{"line": %i, "character": %i}|} p.line p.character

let stringifyRange r =
  Printf.sprintf {|{"start": %s, "end": %s}|}
    (stringifyPosition r.start)
    (stringifyPosition r.end_)

let stringifyMarkupContent (m : markupContent) =
  Printf.sprintf {|{"kind": "%s", "value": "%s"}|} m.kind (Json.escape m.value)

let stringifyCompletionItem c =
  Printf.sprintf
    {|{
    "label": "%s",
    "kind": %i,
    "tags": %s,
    "detail": "%s",
    "documentation": %s
  }|}
    (Json.escape c.label) c.kind
    (c.tags |> List.map string_of_int |> array)
    (Json.escape c.detail)
    (match c.documentation with
    | None -> null
    | Some doc -> stringifyMarkupContent doc)

let stringifyHover h =
  Printf.sprintf {|{"contents": "%s"}|} (Json.escape h.contents)

let stringifyLocation h =
  Printf.sprintf {|{"uri": "%s", "range": %s}|} (Json.escape h.uri)
    (stringifyRange h.range)

let stringifyDocumentSymbolItem i =
  Printf.sprintf
    {|{
        "namea": "%s",
        "kind": %i,
        "location": %s
}|}
    (Json.escape i.name) i.kind
    (stringifyLocation i.location)
