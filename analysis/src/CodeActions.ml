type kind = RefactorRewrite

let kindToString kind = match kind with RefactorRewrite -> "refactor.rewrite"

module TextDocument = struct
  type t = {version : string option; uri : string}

  let make ~version ~uri = {version; uri}

  let toString t =
    Printf.sprintf {|{"version": %s, "uri": "%s"}|}
      (match t.version with
      | Some version -> "\"" ^ Json.escape version ^ "\""
      | None -> "null")
      t.uri
end

module Position = struct
  type t = {line : int; character : int}

  let make ~line ~character = {line; character}

  let toString t =
    Printf.sprintf {|{"line": %s, "character": %s}|} (string_of_int t.line)
      (string_of_int t.character)
end

module TextEditRange = struct
  type t = {start : Position.t; end_ : Position.t}

  let make ~start ~end_ = {start; end_}

  let toString t =
    Printf.sprintf {|{"start": %s, "end": %s}|}
      (t.start |> Position.toString)
      (t.end_ |> Position.toString)
end

module TextEdit = struct
  type t = {newText : string; range : TextEditRange.t}

  let make ~newText ~range = {newText; range}

  let toString t =
    Printf.sprintf {|{"newText": "%s", "range": %s}|} (Json.escape t.newText)
      (t.range |> TextEditRange.toString)
end

module DocumentChange = struct
  type t = {textDocument : TextDocument.t; edits : TextEdit.t list}

  let make ~textDocument ~edits = {textDocument; edits}

  let toString t =
    Printf.sprintf {|{"textDocument": %s, "edits": [%s]}|}
      (t.textDocument |> TextDocument.toString)
      (t.edits
      |> List.map (fun edit -> edit |> TextEdit.toString)
      |> String.concat ",")
end

module CodeActionEdit = struct
  type t = {documentChanges : DocumentChange.t list}

  let make ~documentChanges = {documentChanges}

  let toString t =
    Printf.sprintf {|{"documentChanges": [%s]}|}
      (t.documentChanges
      |> List.map (fun documentChange ->
             documentChange |> DocumentChange.toString)
      |> String.concat ",")
end

module CodeAction = struct
  type t = {title : string; kind : kind; edit : CodeActionEdit.t}

  let make ~title ~kind ~edit = {title; kind; edit}

  let toString t =
    Printf.sprintf {|{"title": "%s", "kind": "%s", "edit": %s}|} t.title
      (kindToString t.kind)
      (t.edit |> CodeActionEdit.toString)
end

(* This is the return that's expected when resolving code actions *)
type result = CodeAction.t list

let stringifyCodeActions codeActions =
  Printf.sprintf {|[%s]|}
    (codeActions
    |> List.map (fun codeAction -> codeAction |> CodeAction.toString)
    |> String.concat ",")
