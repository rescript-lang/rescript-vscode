type position = {line: int; character: int}
type range = {start: position; end_: position}
type markupContent = {kind: string; value: string}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#command *)
type command = {title: string; command: string}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeLens *)
type codeLens = {range: range; command: command option}

type inlayHint = {
  position: position;
  label: string;
  kind: int;
  paddingLeft: bool;
  paddingRight: bool;
}

type completionItem = {
  label: string;
  kind: int;
  tags: int list;
  detail: string;
  sortText: string option;
  documentation: markupContent option;
}

type location = {uri: string; range: range}
type documentSymbolItem = {name: string; kind: int; location: location}
type renameFile = {oldUri: string; newUri: string}
type textEdit = {range: range; newText: string}

type diagnostic = {range: range; message: string; severity: int}

type optionalVersionedTextDocumentIdentifier = {
  version: int option;
  uri: string;
}

type textDocumentEdit = {
  textDocument: optionalVersionedTextDocumentIdentifier;
  edits: textEdit list;
}

type codeActionEdit = {documentChanges: textDocumentEdit list}
type codeActionKind = RefactorRewrite

type codeAction = {
  title: string;
  codeActionKind: codeActionKind;
  edit: codeActionEdit;
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
    "documentation": %s,
    "sortText": %s
  }|}
    (Json.escape c.label) c.kind
    (c.tags |> List.map string_of_int |> array)
    (Json.escape c.detail)
    (match c.documentation with
    | None -> null
    | Some doc -> stringifyMarkupContent doc)
    (match c.sortText with
    | None -> null
    | Some sortText -> Printf.sprintf "\"%s\"" (Json.escape sortText))

let stringifyHover s = Printf.sprintf {|{"contents": "%s"}|} (Json.escape s)

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

let stringifyRenameFile {oldUri; newUri} =
  Printf.sprintf {|{
  "kind": "rename",
  "oldUri": "%s",
  "newUri": "%s"
}|}
    (Json.escape oldUri) (Json.escape newUri)

let stringifyTextEdit (te : textEdit) =
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
    (match td.version with
    | None -> null
    | Some v -> string_of_int v)
    (Json.escape td.uri)

let stringifyTextDocumentEdit tde =
  Printf.sprintf {|{
  "textDocument": %s,
  "edits": %s
  }|}
    (stringifyoptionalVersionedTextDocumentIdentifier tde.textDocument)
    (tde.edits |> List.map stringifyTextEdit |> array)

let codeActionKindToString kind =
  match kind with
  | RefactorRewrite -> "refactor.rewrite"

let stringifyCodeActionEdit cae =
  Printf.sprintf {|{"documentChanges": %s}|}
    (cae.documentChanges |> List.map stringifyTextDocumentEdit |> array)

let stringifyCodeAction ca =
  Printf.sprintf {|{"title": "%s", "kind": "%s", "edit": %s}|} ca.title
    (codeActionKindToString ca.codeActionKind)
    (ca.edit |> stringifyCodeActionEdit)

let stringifyHint hint =
  Printf.sprintf
    {|{
    "position": %s,
    "label": "%s",
    "kind": %i,
    "paddingLeft": %b,
    "paddingRight": %b
}|}
    (stringifyPosition hint.position)
    (Json.escape hint.label) hint.kind hint.paddingLeft hint.paddingRight

let stringifyCommand (command : command) =
  Printf.sprintf {|{"title": "%s", "command": "%s"}|}
    (Json.escape command.title)
    (Json.escape command.command)

let stringifyCodeLens (codeLens : codeLens) =
  Printf.sprintf
    {|{
        "range": %s,
        "command": %s
    }|}
    (stringifyRange codeLens.range)
    (match codeLens.command with
    | None -> ""
    | Some command -> stringifyCommand command)

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic *)
let stringifyDiagnostic d =
  Printf.sprintf
    {|{
  "range": %s,
  "message": "%s",
  "severity": %d,
  "source": "ReScript"
}|}
    (stringifyRange d.range) (Json.escape d.message) d.severity
