(* This returns a string for a label + type expression that is identical to what that label + type expression would look like inside of a function type.
   Example: let someFn = (~first: int, ~second: float) => unit. Running this on the label "second" would return "~second: float". *)
let getPrintedAsLabel label type_expr =
  let stringified =
    Shared.typeToString
      (* Set up a synthetical, dummy function with a single argument (the label we're looking for) and a return value we know exactly what it will be,
         so we can slice it off to extract the label annotation we're after. *)
      Types.
        {
          level = 0;
          id = 0;
          desc =
            Tarrow
              ( label,
                type_expr,
                {level = 0; id = 0; desc = Tvar (Some "a")},
                Cunknown );
        }
      ~lineWidth:400
  in
  let extracted =
    match label with
    (* No label means function is printed without parens. Account for that.
       The final thing subtracted number accounts for the dummy ` => 'a` that automatically comes from printing the label as a function. *)
    | Nolabel -> String.sub stringified 0 (String.length stringified - 6)
    | Labelled _ | Optional _ ->
      (* Labelled arguments are printed with parens. *)
      String.sub stringified 1 (String.length stringified - 8)
  in
  extracted

type cursorAtArg = Unlabelled of int | Labelled of string

let signatureHelp ~path ~pos ~currentFile ~debug =
  let posBeforeCursor = (fst pos, max 0 (snd pos - 1)) in
  let foundFunctionApplicationExpr = ref None in
  let setFound r = foundFunctionApplicationExpr := Some r in
  let expr (iterator : Ast_iterator.iterator) (expr : Parsetree.expression) =
    (match expr with
    | {
     pexp_desc = Pexp_apply (({pexp_desc = Pexp_ident ident} as exp), args);
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
      setFound (argAtCursor, exp, ident)
    | _ -> ());
    Ast_iterator.default_iterator.expr iterator expr
  in
  let iterator = {Ast_iterator.default_iterator with expr} in
  let parser = Res_driver.parsingEngine.parseImplementation ~forPrinter:false in
  let {Res_driver.parsetree = structure} = parser ~filename:currentFile in
  iterator.structure iterator structure |> ignore;
  match !foundFunctionApplicationExpr with
  | Some (argAtCursor, exp, ident) -> (
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
    | {kind = Value type_expr} :: _, Some env, Some package ->
      let args, _ =
        CompletionBackEnd.extractFunctionType type_expr ~env ~package
      in
      if debug then
        Printf.printf "argAtCursor: %s\n"
          (match argAtCursor with
          | None -> "none"
          | Some (Labelled name) -> "~" ^ name
          | Some (Unlabelled index) -> "unlabelled<" ^ string_of_int index ^ ">");

      (* The LS protocol wants us to return a string "label" that contains what we want to put inside of the signature help hint itself.
         It then also wants a list of arguments by character start/end offsets that denotes every paramter of the main type.
         That list then helps us say which parameter is currently highlighted via the `activeParamter` index.
         So, we figure those out here. *)

      (* The offset belows accounts for `let <something> = ` which is what we're starting with.
         We're prepending `let` because it looks a bit better than just having the plain type definition with no identifier. *)
      let name = Utils.flattenLongIdent ident.txt |> SharedTypes.ident in
      let currentOffset = ref (4 + String.length name + 4) in
      let label =
        "let " ^ name ^ " = " ^ Shared.typeToString type_expr ~lineWidth:400
      in

      (* Calculate all parameter character offsets. *)
      let argsCount = List.length args in
      let parameters =
        args
        |> List.mapi (fun index (label, type_expr) ->
               let separatorLength =
                 (* comma + space, unless last arg *)
                 if index = argsCount - 1 then 0 else 2
               in
               let stringified = getPrintedAsLabel label type_expr in
               let offset = String.length stringified in
               let start = !currentOffset in
               let end_ = start + offset in
               let parameterCharacterOffset = (start, end_) in
               currentOffset := !currentOffset + offset + separatorLength;
               parameterCharacterOffset)
      in
      if debug then
        Printf.printf "extracted params: %s\n"
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
