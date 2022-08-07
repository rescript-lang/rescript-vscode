type cursorAtArg = Unlabelled of int | Labelled of string

let signatureHelp ~path ~pos ~currentFile ~debug =
  let posBeforeCursor = (fst pos, max 0 (snd pos - 1)) in
  let foundFunctionApplicationExpr = ref None in
  let setFound r = foundFunctionApplicationExpr := Some r in
  let expr (iterator : Ast_iterator.iterator) (expr : Parsetree.expression) =
    (match expr with
    | {
     pexp_desc = Pexp_apply (({pexp_desc = Pexp_ident _} as exp), args);
     pexp_loc;
    }
      when pexp_loc |> Loc.hasPosInclusive ~pos:posBeforeCursor ->
      (* TODO: Move extractExpApplyArgs to a shared module that doesn't look like it's only for completion. *)
      let extractedArgs = CompletionFrontEnd.extractExpApplyArgs ~args in
      let argAtCursor =
        let unlabelledArgCount = ref 0 in
        extractedArgs
        |> List.find_map (fun arg ->
               match arg.CompletionFrontEnd.label with
               | None ->
                 let currentUnlabelledArgCount = !unlabelledArgCount in
                 unlabelledArgCount := currentUnlabelledArgCount + 1;
                 (* An argument without a label is just the expression, so we can use that. *)
                 if arg.exp.pexp_loc |> Loc.hasPosInclusive ~pos:posBeforeCursor
                 then Some (Unlabelled currentUnlabelledArgCount)
                 else None
               | Some {name; posStart; posEnd} ->
                 (* Check for the label identifier itself having the cursor *)
                 let cursorIsWithinLabelIdPos =
                   posStart <= pos && pos <= posEnd
                 in
                 (* Check for the expr assigned to the label having the cursor *)
                 let cursorIsWithinArgExprPos =
                   arg.exp.pexp_loc |> Loc.hasPosInclusive ~pos:posBeforeCursor
                 in
                 (* This most likely means that parsing failed and recovered. Happens for empty assignments for example. *)
                 let argExprIsParserRecovery =
                   arg.exp.pexp_loc |> Loc.end_ = (Location.none |> Loc.end_)
                 in
                 if
                   cursorIsWithinLabelIdPos || cursorIsWithinArgExprPos
                   || argExprIsParserRecovery
                 then Some (Labelled name)
                 else None)
      in
      setFound (argAtCursor, exp)
    | _ -> ());
    Ast_iterator.default_iterator.expr iterator expr
  in
  let iterator = {Ast_iterator.default_iterator with expr} in
  let parser = Res_driver.parsingEngine.parseImplementation ~forPrinter:false in
  let {Res_driver.parsetree = structure} = parser ~filename:currentFile in
  iterator.structure iterator structure |> ignore;
  match !foundFunctionApplicationExpr with
  | Some (argAtCursor, exp) -> (
    (* Not looking for the cursor position after this, but rather the target function expression's loc. *)
    let pos = exp.pexp_loc |> Loc.end_ in
    let completions, env, package =
      let textOpt = Files.readFile currentFile in
      match textOpt with
      | None | Some "" -> ([], None, None)
      | Some text -> (
        (* Leverage the completion functionality to pull out the type of the identifier doing the function application.
           This lets us leverage all of the smart work done in completions to find the correct type in many cases even
           for files not saved yet. *)
        (* TODO: This should probably eventually be factored out to its own thing, like "find type for pos", that feels less like abusing completions. *)
        match
          CompletionFrontEnd.completionWithParser ~debug ~path ~posCursor:pos
            ~currentFile ~text
        with
        | None -> ([], None, None)
        | Some (completable, scope) -> (
          match Cmt.fullFromPath ~path with
          | None -> ([], None, None)
          | Some {file; package} ->
            let env = SharedTypes.QueryEnv.fromFile file in
            ( completable
              |> CompletionBackEnd.processCompletable ~debug ~package ~pos
                   ~scope ~env ~forHover:true,
              Some env,
              Some package )))
    in
    match (completions, env, package) with
    | {kind = Value type_expr; name} :: _, Some env, Some package ->
      let args, _ =
        CompletionBackEnd.extractFunctionType type_expr ~env ~package
      in
      if debug then
        Printf.printf "argAtCursor: %s\n"
          (match argAtCursor with
          | None -> "none"
          | Some (Labelled name) -> "~" ^ name
          | Some (Unlabelled index) -> "unlabelled<" ^ string_of_int index ^ ">");

      (* The LS protocol wants us to send both the full type signature (label) that the end user sees as the signature help, and all parameters in that label
         in the form of a list of start/end character offsets. We'll leverage the parser to figure the offsets out by parsing the label, and extract the
         offsets from the parser. *)

      (* Put together a label here that both makes sense to show to the end user in the signature help, but also can be passed to the parser. *)
      let label = "let " ^ name ^ ": " ^ Shared.typeToString type_expr in
      (* TODO: Refactor the parser to support reading string contents directly in addition to taking a file path.
         For now, create a temp file, pass it to the parser, and then immediately delete it. *)
      let fileWithTypeSignature =
        Files.writeTempFile ~prefix:"typ_sig" ~suffix:".resi" label
      in
      let parser = Res_driver.parsingEngine.parseInterface ~forPrinter:false in
      let {Res_driver.parsetree = signature} =
        parser ~filename:fileWithTypeSignature
      in
      Sys.remove fileWithTypeSignature;

      let parameters =
        match signature with
        | [
         {
           Parsetree.psig_desc =
             Psig_value {pval_type = {ptyp_desc = Ptyp_arrow _} as expr};
         };
        ] ->
          let rec extractParams expr params =
            match expr with
            | {
             (* Gotcha: functions with multiple arugments are modelled as a series of single argument functions. *)
             Parsetree.ptyp_desc =
               Ptyp_arrow (argumentLabel, argumentTypeExpr, nextFunctionExpr);
             ptyp_loc;
            } ->
              let startOffset =
                ptyp_loc |> Loc.start
                |> CompletionFrontEnd.positionToOffset label
                |> Option.get
              in
              let endOffset =
                argumentTypeExpr.ptyp_loc |> Loc.end_
                |> CompletionFrontEnd.positionToOffset label
                |> Option.get
              in
              (* The AST locations does not account for "=?" of optional arguments, so add that to the offset here if needed. *)
              let endOffset =
                match argumentLabel with
                | Asttypes.Optional _ -> endOffset + 2
                | _ -> endOffset
              in
              extractParams nextFunctionExpr
                (params @ [(startOffset, endOffset)])
            | _ -> params
          in
          extractParams expr []
        | _ -> []
      in

      if debug then
        Printf.printf "extracted params: \n%s\n"
          (parameters
          |> List.map (fun (start, end_) ->
                 String.sub label start (end_ - start))
          |> SharedTypes.list);

      (* Figure out the active parameter *)
      let activeParameter =
        match argAtCursor with
        | None -> None
        | Some (Unlabelled unlabelledArgumentIndex) ->
          let index = ref 0 in
          args
          |> List.find_map (fun (label, _) ->
                 match label with
                 | Asttypes.Nolabel when !index = unlabelledArgumentIndex ->
                   Some !index
                 | _ ->
                   index := !index + 1;
                   None)
        | Some (Labelled name) ->
          let index = ref 0 in
          args
          |> List.find_map (fun (label, _) ->
                 match label with
                 | (Asttypes.Labelled labelName | Optional labelName)
                   when labelName = name ->
                   Some !index
                 | _ ->
                   index := !index + 1;
                   None)
      in
      Some
        {
          Protocol.signatures =
            [
              {
                label;
                parameters =
                  parameters
                  |> List.map (fun params -> {Protocol.label = params});
              };
            ];
          activeSignature = Some 0;
          activeParameter =
            (match activeParameter with
            | None -> Some (-1)
            | activeParameter -> activeParameter);
        }
    | _ -> None)
  | _ -> None
