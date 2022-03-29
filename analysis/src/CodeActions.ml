open Protocol

type kind = RefactorRewrite

let kindToString kind = match kind with RefactorRewrite -> "refactor.rewrite"

type codeAction = {title : string; kind : kind; edit : codeActionEdit}

let stringifyCodeAction ca =
  Printf.sprintf {|{"title": "%s", "kind": "%s", "edit": %s}|} ca.title
    (kindToString ca.kind)
    (ca.edit |> stringifyCodeActionEdit)

(* This is the return that's expected when resolving code actions *)
type result = codeAction list

let stringifyCodeActions codeActions =
  Printf.sprintf {|%s|} (codeActions |> List.map stringifyCodeAction |> array)

module CodeAction = struct
  let makeRangeReplace ~title ~kind ~uri ~newText ~range =
    {
      title;
      kind;
      edit =
        {
          documentChanges =
            [{textDocument = {version = None; uri}; edits = [{newText; range}]}];
        };
    }
end
