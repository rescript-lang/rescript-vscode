open SharedTypes
open CompletionNewTypes

let rec getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos
    ~(env : QueryEnv.t) ~exact ~(scope : Scope.t) (contextPath : ctxPath) =
  if debug then Printf.printf "ContextPath %s\n" (ctxPathToString contextPath);
  let package = full.package in
  match contextPath with
  | CString ->
    [
      Completion.create "dummy" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "string")) []));
    ]
  | CBool ->
    [
      Completion.create "dummy" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "bool")) []));
    ]
  | CInt ->
    [
      Completion.create "dummy" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "int")) []));
    ]
  | CFloat ->
    [
      Completion.create "dummy" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "float")) []));
    ]
  | CArray None ->
    [
      Completion.create "array" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "array")) []));
    ]
  | CArray (Some cp) -> (
    match
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> CompletionBackEnd.completionsGetCompletionType ~full
    with
    | None -> []
    | Some (typ, env) ->
      [
        Completion.create "dummy" ~env
          ~kind:
            (Completion.ExtractedType (Tarray (env, ExtractedType typ), `Type));
      ])
  | COption cp -> (
    match
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> CompletionBackEnd.completionsGetCompletionType ~full
    with
    | None -> []
    | Some (typ, env) ->
      [
        Completion.create "dummy" ~env
          ~kind:
            (Completion.ExtractedType (Toption (env, ExtractedType typ), `Type));
      ])
  | CAwait cp -> (
    match
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> CompletionBackEnd.completionsGetCompletionType ~full
    with
    | Some (Tpromise (env, typ), _env) ->
      [Completion.create "dummy" ~env ~kind:(Completion.Value typ)]
    | _ -> [])
  | CId (path, completionContext) ->
    path
    |> CompletionBackEnd.getCompletionsForPath ~debug ~package ~opens ~full ~pos
         ~exact
         ~completionContext:
           (match completionContext with
           | Value -> Value
           | Module -> Module
           | Field -> Field
           | Type -> Type)
         ~env ~scope
  | CApply (cp, labels) -> (
    match
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> CompletionBackEnd.completionsGetCompletionType2 ~debug ~full ~opens
           ~rawOpens ~pos ~scope
    with
    | Some ((TypeExpr typ | ExtractedType (Tfunction {typ})), env) -> (
      let rec reconstructFunctionType args tRet =
        match args with
        | [] -> tRet
        | (label, tArg) :: rest ->
          let restType = reconstructFunctionType rest tRet in
          {typ with desc = Tarrow (label, tArg, restType, Cok)}
      in
      let rec processApply args labels =
        match (args, labels) with
        | _, [] -> args
        | _, label :: (_ :: _ as nextLabels) ->
          (* compute the application of the first label, then the next ones *)
          let args = processApply args [label] in
          processApply args nextLabels
        | (Asttypes.Nolabel, _) :: nextArgs, [Asttypes.Nolabel] -> nextArgs
        | ((Labelled _, _) as arg) :: nextArgs, [Nolabel] ->
          arg :: processApply nextArgs labels
        | (Optional _, _) :: nextArgs, [Nolabel] -> processApply nextArgs labels
        | ( (((Labelled s1 | Optional s1), _) as arg) :: nextArgs,
            [(Labelled s2 | Optional s2)] ) ->
          if s1 = s2 then nextArgs else arg :: processApply nextArgs labels
        | ((Nolabel, _) as arg) :: nextArgs, [(Labelled _ | Optional _)] ->
          arg :: processApply nextArgs labels
        | [], [(Nolabel | Labelled _ | Optional _)] ->
          (* should not happen, but just ignore extra arguments *) []
      in
      match TypeUtils.extractFunctionType ~env ~package typ with
      | args, tRet when args <> [] ->
        let args = processApply args labels in
        let retType = reconstructFunctionType args tRet in
        [Completion.create "dummy" ~env ~kind:(Completion.Value retType)]
      | _ -> [])
    | _ -> [])
  | CField (CId (path, Module), fieldName) ->
    (* M.field *)
    path @ [fieldName]
    |> CompletionBackEnd.getCompletionsForPath ~debug ~package ~opens ~full ~pos
         ~exact ~completionContext:Field ~env ~scope
  | CField (cp, fieldName) -> (
    let completionsForCtxPath =
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
    in
    let extracted =
      match
        completionsForCtxPath
        |> CompletionBackEnd.completionsGetCompletionType2 ~debug ~full ~opens
             ~rawOpens ~pos ~scope
      with
      | Some (TypeExpr typ, env) -> (
        match typ |> TypeUtils.extractRecordType ~env ~package with
        | Some (env, fields, typDecl) ->
          Some
            ( env,
              fields,
              typDecl.item.decl |> Shared.declToString typDecl.name.txt )
        | None -> None)
      | Some (ExtractedType typ, env) -> (
        match typ with
        | Trecord {fields} ->
          Some (env, fields, typ |> TypeUtils.extractedTypeToString)
        | _ -> None)
      | None -> None
    in
    match extracted with
    | None -> []
    | Some (env, fields, recordAsString) ->
      fields
      |> Utils.filterMap (fun field ->
             if Utils.checkName field.fname.txt ~prefix:fieldName ~exact then
               Some
                 (Completion.create field.fname.txt ~env
                    ?deprecated:field.deprecated ~docstring:field.docstring
                    ~kind:(Completion.Field (field, recordAsString)))
             else None))
  | CObj (cp, label) -> (
    (* TODO: Also needs to support ExtractedType *)
    match
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> CompletionBackEnd.completionsGetTypeEnv2 ~debug ~full ~opens ~rawOpens
           ~pos ~scope
    with
    | Some (typ, env) -> (
      match typ |> TypeUtils.extractObjectType ~env ~package with
      | Some (env, tObj) ->
        let rec getFields (texp : Types.type_expr) =
          match texp.desc with
          | Tfield (name, _, t1, t2) ->
            let fields = t2 |> getFields in
            (name, t1) :: fields
          | Tlink te | Tsubst te | Tpoly (te, []) -> te |> getFields
          | Tvar None -> []
          | _ -> []
        in
        tObj |> getFields
        |> Utils.filterMap (fun (field, typ) ->
               if Utils.checkName field ~prefix:label ~exact then
                 Some
                   (Completion.create field ~env ~kind:(Completion.ObjLabel typ))
               else None)
      | None -> [])
    | None -> [])
  | CPipe {ctxPath = cp; id = funNamePrefix; lhsLoc} -> (
    match
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> CompletionBackEnd.completionsGetTypeEnv2 ~debug ~full ~opens ~rawOpens
           ~pos ~scope
    with
    | None -> []
    | Some (typ, envFromCompletionItem) -> (
      let env, typ =
        typ
        |> TypeUtils.resolveTypeForPipeCompletion ~env ~package ~full ~lhsLoc
      in
      if debug then
        if env <> envFromCompletionItem then
          Printf.printf "CPPipe env:%s envFromCompletionItem:%s\n"
            (QueryEnv.toString env)
            (QueryEnv.toString envFromCompletionItem)
        else Printf.printf "CPPipe env:%s\n" (QueryEnv.toString env);
      let completionPath =
        match typ with
        | Builtin (builtin, _) ->
          let {
            arrayModulePath;
            optionModulePath;
            stringModulePath;
            intModulePath;
            floatModulePath;
            promiseModulePath;
            listModulePath;
            resultModulePath;
          } =
            package.builtInCompletionModules
          in
          Some
            (match builtin with
            | Array -> arrayModulePath
            | Option -> optionModulePath
            | String -> stringModulePath
            | Int -> intModulePath
            | Float -> floatModulePath
            | Promise -> promiseModulePath
            | List -> listModulePath
            | Result -> resultModulePath
            | Lazy -> ["Lazy"]
            | Char -> ["Char"])
        | TypExpr t -> (
          match t.Types.desc with
          | Tconstr (path, _typeArgs, _)
          | Tlink {desc = Tconstr (path, _typeArgs, _)}
          | Tsubst {desc = Tconstr (path, _typeArgs, _)}
          | Tpoly ({desc = Tconstr (path, _typeArgs, _)}, []) -> (
            if debug then Printf.printf "CPPipe type path:%s\n" (Path.name path);
            match Utils.expandPath path with
            | _ :: pathRev ->
              (* type path is relative to the completion environment
                 express it from the root of the file *)
              let found, pathFromEnv =
                QueryEnv.pathFromEnv envFromCompletionItem (List.rev pathRev)
              in
              if debug then
                Printf.printf "CPPipe pathFromEnv:%s found:%b\n"
                  (pathFromEnv |> String.concat ".")
                  found;
              if pathFromEnv = [] then None
              else if
                env.file.moduleName <> envFromCompletionItem.file.moduleName
                && found
                (* If the module names are different, then one needs to qualify the path.
                   But only if the path belongs to the env from completion *)
              then Some (envFromCompletionItem.file.moduleName :: pathFromEnv)
              else Some pathFromEnv
            | _ -> None)
          | _ -> None)
      in
      match completionPath with
      | Some completionPath -> (
        let rec removeRawOpen rawOpen modulePath =
          match (rawOpen, modulePath) with
          | [_], _ -> Some modulePath
          | s :: inner, first :: restPath when s = first ->
            removeRawOpen inner restPath
          | _ -> None
        in
        let rec removeRawOpens rawOpens modulePath =
          match rawOpens with
          | rawOpen :: restOpens -> (
            let newModulePath = removeRawOpens restOpens modulePath in
            match removeRawOpen rawOpen newModulePath with
            | None -> newModulePath
            | Some mp -> mp)
          | [] -> modulePath
        in
        let completionPathMinusOpens =
          completionPath
          |> removeRawOpens package.opens
          |> removeRawOpens rawOpens |> String.concat "."
        in
        let completionName name =
          if completionPathMinusOpens = "" then name
          else completionPathMinusOpens ^ "." ^ name
        in
        let completions =
          completionPath @ [funNamePrefix]
          |> CompletionBackEnd.getCompletionsForPath ~debug
               ~completionContext:Value ~exact:false ~package ~opens ~full ~pos
               ~env ~scope
        in
        let completions =
          completions
          |> List.map (fun (completion : Completion.t) ->
                 {
                   completion with
                   name = completionName completion.name;
                   env
                   (* Restore original env for the completion after x->foo()... *);
                 })
        in
        (* We add React element functions to the completion if we're in a JSX context *)
        let inJsx = false in
        (* TODO(1) *)
        let forJsxCompletion =
          if inJsx then
            match typ with
            | Builtin (Int, t) -> Some ("int", t)
            | Builtin (Float, t) -> Some ("float", t)
            | Builtin (String, t) -> Some ("string", t)
            | Builtin (Array, t) -> Some ("array", t)
            | _ -> None
          else None
        in
        match forJsxCompletion with
        | Some (builtinNameToComplete, typ)
          when Utils.checkName builtinNameToComplete ~prefix:funNamePrefix
                 ~exact:false ->
          [
            Completion.createWithSnippet
              ~name:("React." ^ builtinNameToComplete)
              ~kind:(Value typ) ~env ~sortText:"A"
              ~docstring:
                [
                  "Turns `" ^ builtinNameToComplete
                  ^ "` into `React.element` so it can be used inside of JSX.";
                ]
              ();
          ]
          @ completions
        | _ -> completions)
      | None -> []))
  | CTuple ctxPaths ->
    (* Turn a list of context paths into a list of type expressions. *)
    let typeExrps =
      ctxPaths
      |> List.map (fun contextPath ->
             contextPath
             |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos
                  ~env ~exact:true ~scope)
      |> List.filter_map (fun completionItems ->
             match completionItems with
             | {Completion.kind = Value typ} :: _ -> Some typ
             | _ -> None)
    in
    if List.length ctxPaths = List.length typeExrps then
      [
        Completion.create "dummy" ~env
          ~kind:(Completion.Value (Ctype.newty (Ttuple typeExrps)));
      ]
    else []
  | CJsxPropValue {pathToComponent; propName} -> (
    let findTypeOfValue path =
      path
      |> CompletionBackEnd.getCompletionsForPath ~debug ~completionContext:Value
           ~exact:true ~package ~opens ~full ~pos ~env ~scope
      |> CompletionBackEnd.completionsGetTypeEnv2 ~debug ~full ~opens ~rawOpens
           ~pos ~scope
    in
    let lowercaseComponent =
      match pathToComponent with
      | [elName] when Char.lowercase_ascii elName.[0] = elName.[0] -> true
      | _ -> false
    in
    let targetLabel =
      if lowercaseComponent then
        let rec digToTypeForCompletion path =
          match
            path
            |> CompletionBackEnd.getCompletionsForPath ~debug
                 ~completionContext:Type ~exact:true ~package ~opens ~full ~pos
                 ~env ~scope
          with
          | {kind = Type {kind = Abstract (Some (p, _))}} :: _ ->
            (* This case happens when what we're looking for is a type alias.
               This is the case in newer rescript-react versions where
               ReactDOM.domProps is an alias for JsxEvent.t. *)
            let pathRev = p |> Utils.expandPath in
            pathRev |> List.rev |> digToTypeForCompletion
          | {kind = Type {kind = Record fields}} :: _ -> (
            match fields |> List.find_opt (fun f -> f.fname.txt = propName) with
            | None -> None
            | Some f -> Some (f.fname.txt, f.typ, env))
          | _ -> None
        in
        ["ReactDOM"; "domProps"] |> digToTypeForCompletion
      else
        CompletionJsx.getJsxLabels ~componentPath:pathToComponent
          ~findTypeOfValue ~package
        |> List.find_opt (fun (label, _, _) -> label = propName)
    in
    match targetLabel with
    | None -> []
    | Some (_, typ, env) ->
      [
        Completion.create "dummy" ~env
          ~kind:(Completion.Value (Utils.unwrapIfOption typ));
      ])
  | CFunctionArgument {functionContextPath; argumentLabel} -> (
    let labels, env =
      match
        functionContextPath
        |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
             ~exact:true ~scope
        |> CompletionBackEnd.completionsGetCompletionType2 ~debug ~full ~opens
             ~rawOpens ~pos ~scope
      with
      | Some ((TypeExpr typ | ExtractedType (Tfunction {typ})), env) ->
        (typ |> TypeUtils.getArgs ~full ~env, env)
      | _ -> ([], env)
    in
    let targetLabel =
      labels
      |> List.find_opt (fun (label, _) ->
             match (argumentLabel, label) with
             | ( Unlabelled {argumentPosition = pos1},
                 Completable.Unlabelled {argumentPosition = pos2} ) ->
               pos1 = pos2
             | ( (Labelled name1 | Optional name1),
                 (Labelled name2 | Optional name2) ) ->
               name1 = name2
             | _ -> false)
    in
    let expandOption =
      match targetLabel with
      | None | Some ((Unlabelled _ | Labelled _), _) -> false
      | Some (Optional _, _) -> true
    in
    match targetLabel with
    | None -> []
    | Some (_, typ) ->
      [
        Completion.create "dummy" ~env
          ~kind:
            (Completion.Value
               (if expandOption then Utils.unwrapIfOption typ else typ));
      ])
  | CUnknown -> []
  | CVariantPayload {ctxPath; itemNum} -> (
    match
      ctxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> CompletionBackEnd.completionsGetCompletionType2 ~debug ~full ~opens
           ~rawOpens ~pos ~scope
    with
    | Some (typ, env) -> (
      let typ =
        match typ with
        | ExtractedType inner -> Some inner
        | TypeExpr t -> t |> TypeUtils.extractType ~env ~package:full.package
      in
      match typ with
      | Some (Tvariant {constructors}) -> (
        let targetType =
          constructors
          |> Utils.findMap (fun (c : Constructor.t) ->
                 match c.args with
                 | Args args -> (
                   match List.nth_opt args itemNum with
                   | None -> None
                   | Some (typ, _) -> Some typ)
                 | _ -> None)
        in
        match targetType with
        | None -> []
        | Some t -> [Completion.create "dummy" ~env ~kind:(Completion.Value t)])
      | _ -> [])
    | _ -> [])
  | CTupleItem _ -> []
  | CRecordField {ctxPath; prefix; seenFields} -> (
    let completionsForCtxPath =
      ctxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
    in
    let extracted =
      match
        completionsForCtxPath
        |> CompletionBackEnd.completionsGetCompletionType2 ~debug ~full ~opens
             ~rawOpens ~pos ~scope
      with
      | Some (TypeExpr typ, env) -> (
        match typ |> TypeUtils.extractRecordType ~env ~package with
        | Some (env, fields, typDecl) ->
          Some
            ( env,
              fields,
              typDecl.item.decl |> Shared.declToString typDecl.name.txt )
        | None -> None)
      | Some (ExtractedType typ, env) -> (
        match typ with
        | Trecord {fields} ->
          Some (env, fields, typ |> TypeUtils.extractedTypeToString)
        | _ -> None)
      | None -> None
    in
    match extracted with
    | None -> []
    | Some (env, fields, recordAsString) ->
      let field =
        fields
        |> Utils.filterMap (fun field ->
               if Utils.checkName field.fname.txt ~prefix ~exact then
                 Some
                   (Completion.create field.fname.txt ~env
                      ?deprecated:field.deprecated ~docstring:field.docstring
                      ~kind:(Completion.Field (field, recordAsString)))
               else None)
      in
      field)
  | CFunction _ -> []
