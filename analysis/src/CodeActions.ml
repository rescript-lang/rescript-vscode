(* This is the return that's expected when resolving code actions *)
type result = Protocol.codeAction list

let stringifyCodeActions codeActions =
  Printf.sprintf {|%s|}
    (codeActions |> List.map Protocol.stringifyCodeAction |> Protocol.array)

let make ~title ~kind ~uri ~newText ~range =
  {
    Protocol.title;
    codeActionKind = kind;
    edit =
      {
        documentChanges =
          [{textDocument = {version = None; uri}; edits = [{newText; range}]}];
      };
  }
