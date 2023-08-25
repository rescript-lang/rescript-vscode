let getCompletions ~debug ~path ~pos ~currentFile ~forHover =
  let textOpt = Files.readFile currentFile in
  match textOpt with
  | None | Some "" -> None
  | Some text -> (
    match
      CompletionFrontEnd.completionWithParser ~debug ~path ~posCursor:pos
        ~currentFile ~text
    with
    | None -> None
    | Some (completable, scope) -> (
      (* Only perform expensive ast operations if there are completables *)
      match Cmt.loadFullCmtFromPath ~path with
      | None -> None
      | Some full ->
        let env = SharedTypes.QueryEnv.fromFile full.file in
        let completables =
          completable
          |> CompletionBackEnd.processCompletable ~debug ~full ~pos ~scope ~env
               ~forHover
        in
        Some (completables, full, scope)))

let getCompletions2 ~debug ~path ~pos ~currentFile ~forHover =
  let textOpt = Files.readFile currentFile in
  match textOpt with
  | None | Some "" -> ()
  | Some text -> (
    match Pos.positionToOffset text pos with
    | None -> ()
    | Some offset -> (
      match
        CompletionFrontEndNew.completion ~offset ~debug ~path ~posCursor:pos
          ~currentFile text
      with
      | None -> print_endline "No completions"
      | Some (res, ctx) ->
        Printf.printf "Result: %s\n"
          (match res with
          | CId (path, _ctx) -> "CId " ^ SharedTypes.ident path
          | CtxPath ctxPath ->
            "CtxPath: "
            ^ (ctxPath |> List.rev
              |> List.map CompletionFrontEndNew.ctxPathToString
              |> String.concat "->")
          | CType {prefix} -> "CType:<todo-typ> =" ^ prefix);
        Printf.printf "Scope: %i items\n" (List.length ctx.scope);
        Printf.printf "CtxPath: %s\n"
          (ctx.ctxPath |> List.rev
          |> List.map CompletionFrontEndNew.ctxPathToString
          |> String.concat "->")))
