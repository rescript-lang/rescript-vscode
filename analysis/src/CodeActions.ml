(* This is the return that's expected when resolving code actions *)
type result = Protocol.codeAction list

let stringifyCodeActions codeActions =
  Printf.sprintf {|%s|}
    (codeActions |> List.map Protocol.stringifyCodeAction |> Protocol.array)

let make ~title ~kind ~edit ~command =
  {Protocol.title; codeActionKind = kind; edit; command}

let makeEdit edits uri =
  Protocol.
    {
      documentChanges =
        [
          {
            textDocument =
              {version = None; uri = uri |> Uri.fromPath |> Uri.toString};
            edits;
          };
        ];
    }
