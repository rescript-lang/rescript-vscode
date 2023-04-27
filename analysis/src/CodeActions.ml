(* This is the return that's expected when resolving code actions *)
type result = Protocol.codeAction list

let stringifyCodeActions codeActions =
  Printf.sprintf {|%s|}
    (codeActions |> List.map Protocol.stringifyCodeAction |> Protocol.array)

let make ~title ~kind ~(edit : Protocol.codeActionEdit option)
    ~(command : Protocol.command option) =
  {Protocol.title; codeActionKind = kind; edit; command}
