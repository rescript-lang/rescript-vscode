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

type renameFile = {kind : [`rename]; oldUri : string; newUri : string}

type textEdit = {range : range; newText : string}

type optionalVersionedTextDocumentIdentifier = {
  version : int option;
  uri : string;
}

type textDocumentEdit = {
  textDocument : optionalVersionedTextDocumentIdentifier;
  edits : textEdit list;
}

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

let stringifyLocation (h : location) =
  Printf.sprintf {|{"uri": "%s", "range": %s}|} (Json.escape h.uri)
    (stringifyRange h.range)

let stringifyDocumentSymbolItem i =
  Printf.sprintf
    {|{
        "name": "%s",
        "kind": %i,
        "location": %s
}|}
    (Json.escape i.name) i.kind
    (stringifyLocation i.location)

let stringifyRenameFile rf =
  Printf.sprintf {|{
  "kind": "%s",
  "oldUri": "%s",
  "newUri": "%s"
}|}
    (match rf.kind with `rename -> "rename")
    (Json.escape rf.oldUri) (Json.escape rf.newUri)

let stringifyTextEdit te =
  Printf.sprintf {|{
  "range": %s,
  "newText": "%s"
  }|}
    (stringifyRange te.range) (Json.escape te.newText)

let stringifyoptionalVersionedTextDocumentIdentifier td =
  Printf.sprintf {|{
  "version": %s,
  "uri": "%s"
  }|}
    (match td.version with None -> null | Some v -> string_of_int v)
    (Json.escape td.uri)

let stringifyTextDocumentEdit tde =
  Printf.sprintf {|{
  "textDocument": %s,
  "edits": %s
  }|}
    (stringifyoptionalVersionedTextDocumentIdentifier tde.textDocument)
    (tde.edits |> List.map stringifyTextEdit |> array)
