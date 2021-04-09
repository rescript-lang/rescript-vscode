module J = JsonShort

let posOfLexing {Lexing.pos_lnum; pos_cnum; pos_bol} =
  J.o [("line", J.i (pos_lnum - 1)); ("character", J.i (pos_cnum - pos_bol))]

let rangeOfLoc {Location.loc_start; loc_end} =
  J.o [("start", posOfLexing loc_start); ("end", posOfLexing loc_end)]

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
