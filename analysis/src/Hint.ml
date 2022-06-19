open SharedTypes

let locItemToTypeHint ~full:{file; package} locItem =
  match locItem.locType with
  | Constant t ->
    let typehint = (match t with
    | Const_int _ -> "int"
    | Const_char _ -> "char"
    | Const_string _ -> "string"
    | Const_float _ -> "float"
    | Const_int32 _ -> "int32"
    | Const_int64 _ -> "int64"
    | Const_nativeint _ -> "int") in
    Some(typehint, typehint)
  | Typed (_, t, locKind) ->
    let fromType ~docstring typ =
      (* TODO: get only type return *)
      let typeString = typ 
      |> Shared.typeToString 
      |> Str.split (Str.regexp "\n")
      |> List.filter(fun c -> c <> "\n") 
      |> String.concat "" in

      let extraTypeInfo =
        let env = QueryEnv.fromFile file in
        match typ |> Shared.digConstructor with
        | None -> None
        | Some path -> (
          match References.digConstructor ~env ~package path with
          | None -> None
          | Some (_env, {docstring; name = {txt}; item = {decl}}) ->
            if Utils.isUncurriedInternal path then None
            else Some (decl |> Shared.declToString txt, docstring))
      in
      let typeString, docstring =
        match extraTypeInfo with
        | None -> (typeString, [typeString])
        | Some (extra, extraDocstring) ->
          (typeString, extra :: extraDocstring)
      in
      (typeString, String.concat "" docstring)
    in
    Some
      (match References.definedForLoc ~file ~package locKind with
      | None -> t |> fromType ~docstring:[]
      | Some (docstring, res) -> (
        match res with
        | `Declared -> t |> fromType ~docstring:[]
        | `Constructor {cname = {txt}; args} -> 
            let typeString, docstring = t |> fromType ~docstring in
            let argsString =
              match args with
              | [] -> ""
              | _ ->
                args
                |> List.map (fun (t, _) -> Shared.typeToString t)
                |> String.concat ", " |> Printf.sprintf "(%s)"
            in
            (typeString, Hover.codeBlock (txt ^ argsString ^ docstring))
        | `Field -> t |> fromType ~docstring)
        )
  | _ -> None

(* TODO: filter for range of lines*)
let inlay ~path ~pos ~debug =
  let symbols = ref [] in
  (* TODO: Handle with tuples let-bindings *)
  let value_binding (iterator : Ast_iterator.iterator)
      (vb : Parsetree.value_binding) =
    (match vb.pvb_pat.ppat_desc with
    | Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _) ->
      symbols := vb.pvb_pat.ppat_loc :: !symbols
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator vb
  in
  let iterator = {Ast_iterator.default_iterator with value_binding} in
  (if Filename.check_suffix path ".res" then
   let parser =
     Res_driver.parsingEngine.parseImplementation ~forPrinter:false
   in
   let {Res_driver.parsetree = structure} = parser ~filename:path in
   iterator.structure iterator structure |> ignore
  else
    let parser = Res_driver.parsingEngine.parseInterface ~forPrinter:false in
    let {Res_driver.parsetree = signature} = parser ~filename:path in
    iterator.signature iterator signature |> ignore);
  !symbols
  |> List.rev_map (fun locOfName ->
         let range = Utils.cmtLocToRange locOfName in
         match Cmt.fullFromPath ~path with
         | None -> None
         | Some full -> (
           match
             References.getLocItem ~full
               ~pos:(range.start.line, range.start.character + 1)
               ~debug
           with
           | None -> None
           | Some locItem ->
             let position : Protocol.position =
               {line = range.start.line; character = range.end_.character}
             in
            match locItemToTypeHint locItem ~full with
            | Some (label, tooltip) -> 
              let result =
                Protocol.stringifyHint
                  {
                    kind = 1;
                    position;
                    tooltip =
                      {kind = "markdown"; value = Hover.codeBlock tooltip};
                    paddingLeft = false;
                    paddingRight = false;
                    label = ": " ^ label;
                  }
              in
              Some(result)
           | None -> None))
  |> List.filter_map (fun x -> x)
