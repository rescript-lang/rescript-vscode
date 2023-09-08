open SharedTypes
open CompletionNewTypes
open CompletionsNewTypesCtxPath

(* TODO: Unify and clean these up once we have tests *)

let getCompletionsForPath = CompletionBackEnd.getCompletionsForPath
let getOpens = CompletionBackEnd.getOpens
let getComplementaryCompletionsForTypedValue =
  CompletionBackEnd.getComplementaryCompletionsForTypedValue

let rec completionsGetCompletionType ~full = function
  | {Completion.kind = Value typ; env} :: _
  | {Completion.kind = ObjLabel typ; env} :: _
  | {Completion.kind = Field ({typ}, _); env} :: _ ->
    typ
    |> TypeUtils.extractType ~env ~package:full.package
    |> Option.map (fun typ -> (typ, env))
  | {Completion.kind = Type typ; env} :: _ -> (
    match TypeUtils.extractTypeFromResolvedType typ ~env ~full with
    | None -> None
    | Some extractedType -> Some (extractedType, env))
  | {Completion.kind = ExtractedType (typ, _); env} :: _ -> Some (typ, env)
  | _ -> None

and completionsGetCompletionTypeX ~full = function
  | {Completion.kind = Value typ; env} :: _
  | {Completion.kind = ObjLabel typ; env} :: _
  | {Completion.kind = Field ({typ}, _); env} :: _ ->
    typ
    |> TypeUtils.extractType ~env ~package:full.package
    |> Option.map (fun typ -> (typ, env))
  | {Completion.kind = Type typ; env} :: _ -> (
    match TypeUtils.extractTypeFromResolvedType typ ~env ~full with
    | None -> None
    | Some extractedType -> Some (extractedType, env))
  | {Completion.kind = ExtractedType (typ, _); env} :: _ -> Some (typ, env)
  | _ -> None

and completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos ~scope =
  function
  | {Completion.kind = Value typ; env} :: _
  | {Completion.kind = ObjLabel typ; env} :: _
  | {Completion.kind = Field ({typ}, _); env} :: _ ->
    Some (TypeExpr typ, env)
  | {Completion.kind = FollowContextPath (`New ctxPath); env} :: _ ->
    ctxPath
    |> getCompletionsForContextPath ~debug ~full ~env ~exact:true ~opens
         ~rawOpens ~pos ~scope
    |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos ~scope
  | {Completion.kind = Type typ; env} :: _ -> (
    match TypeUtils.extractTypeFromResolvedType typ ~env ~full with
    | None -> None
    | Some extractedType -> Some (ExtractedType extractedType, env))
  | {Completion.kind = ExtractedType (typ, _); env} :: _ ->
    Some (ExtractedType typ, env)
  | _ -> None

and completionsGetTypeEnv ~debug (completions : Completion.t list) ~full ~opens
    ~rawOpens ~pos ~scope =
  match completions with
  | {Completion.kind = Value typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = ObjLabel typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = Field ({typ}, _); env} :: _ -> Some (typ, env)
  | {Completion.kind = FollowContextPath (`New ctxPath); env} :: _ ->
    ctxPath
    |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
         ~exact:true ~scope
    |> completionsGetTypeEnv ~debug ~full ~opens ~rawOpens ~pos ~scope
  | _ -> None

and getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos
    ~(env : QueryEnv.t) ~exact ~(scope : Scope.t) (contextPath : ctxPath) =
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
      |> completionsGetCompletionType ~full
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
      |> completionsGetCompletionType ~full
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
      |> completionsGetCompletionType ~full
    with
    | Some (Tpromise (env, typ), _env) ->
      [Completion.create "dummy" ~env ~kind:(Completion.Value typ)]
    | _ -> [])
  | CId (path, completionContext) ->
    path
    |> getCompletionsForPath ~debug ~package ~opens ~full ~pos ~exact
         ~completionContext:
           (match completionContext with
           | Value -> Value
           | Module -> Module
           | Field -> Field
           | Type -> Type)
         ~env ~scope
  | CApply {functionCtxPath; args = labels} -> (
    match
      functionCtxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos ~scope
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
  | CRecordFieldAccess {recordCtxPath = CId (path, Module); fieldName} ->
    (* M.field *)
    path @ [fieldName]
    |> getCompletionsForPath ~debug ~package ~opens ~full ~pos ~exact
         ~completionContext:Field ~env ~scope
  | CRecordFieldAccess {recordCtxPath; fieldName} -> (
    let completionsForCtxPath =
      recordCtxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
    in
    let extracted =
      match
        completionsForCtxPath
        |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos
             ~scope
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
  | CObj {objectCtxPath; propertyName} -> (
    (* TODO: Also needs to support ExtractedType *)
    match
      objectCtxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetTypeEnv ~debug ~full ~opens ~rawOpens ~pos ~scope
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
               if Utils.checkName field ~prefix:propertyName ~exact then
                 Some
                   (Completion.create field ~env ~kind:(Completion.ObjLabel typ))
               else None)
      | None -> [])
    | None -> [])
  | CPipe {functionCtxPath = cp; id = funNamePrefix; lhsLoc} -> (
    match
      cp
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetTypeEnv ~debug ~full ~opens ~rawOpens ~pos ~scope
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
          |> getCompletionsForPath ~debug ~completionContext:Value ~exact:false
               ~package ~opens ~full ~pos ~env ~scope
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
      |> getCompletionsForPath ~debug ~completionContext:Value ~exact:true
           ~package ~opens ~full ~pos ~env ~scope
      |> completionsGetTypeEnv ~debug ~full ~opens ~rawOpens ~pos ~scope
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
            |> getCompletionsForPath ~debug ~completionContext:Type ~exact:true
                 ~package ~opens ~full ~pos ~env ~scope
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
        |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos
             ~scope
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
  | CVariantPayload {variantCtxPath; itemNum; constructorName} -> (
    match
      variantCtxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos ~scope
    with
    | Some (typ, env) -> (
      let typ =
        match typ with
        | ExtractedType inner -> Some inner
        | TypeExpr t -> t |> TypeUtils.extractType ~env ~package:full.package
      in
      match typ with
      | Some (Toption (env, innerType))
        when constructorName = "Some" && itemNum = 0 ->
        (* Special handling for option which is represented as itself even though it's technically a variant. *)
        [
          Completion.create "dummy" ~env
            ~kind:
              (match innerType with
              | ExtractedType innerType -> ExtractedType (innerType, `Type)
              | TypeExpr t -> Value t);
        ]
      | Some (Tvariant {constructors}) -> (
        let targetType =
          constructors
          |> Utils.findMap (fun (c : Constructor.t) ->
                 if c.cname.txt = constructorName then
                   match c.args with
                   | Args args -> (
                     match List.nth_opt args itemNum with
                     | None -> None
                     | Some (typ, _) -> Some typ)
                   | _ -> None
                 else None)
        in
        match targetType with
        | None -> []
        | Some t -> [Completion.create "dummy" ~env ~kind:(Completion.Value t)])
      | _ -> [])
    | _ -> [])
  | CTupleItem {tupleCtxPath; itemNum} -> (
    match
      tupleCtxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos ~scope
    with
    | Some (typ, env) -> (
      let typ =
        match typ with
        | ExtractedType t -> Some t
        | TypeExpr t -> TypeUtils.extractType ~env ~package t
      in
      match typ with
      | Some (Tuple (env, items, _)) -> (
        match List.nth_opt items itemNum with
        | None -> []
        | Some tupleItemType ->
          [Completion.create "dummy" ~env ~kind:(Value tupleItemType)])
      | _ -> [])
    | _ -> [])
  | CRecordField {recordCtxPath; prefix} when true -> (
    let completionsForCtxPath =
      recordCtxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
    in
    let extracted =
      match
        completionsForCtxPath
        |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos
             ~scope
      with
      | Some (TypeExpr typ, env) -> typ |> TypeUtils.extractType ~env ~package
      | Some (ExtractedType typ, _env) -> Some typ
      | None -> None
    in
    match extracted with
    | Some (Trecord _ as typ) ->
      [Completion.create "dummy" ~env ~kind:(ExtractedType (typ, `Value))]
    | _ -> [])
  | CRecordField {recordCtxPath; prefix; seenFields} -> (
    let completionsForCtxPath =
      recordCtxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
    in
    let extracted =
      match
        completionsForCtxPath
        |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos
             ~scope
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
          Printf.printf "fields: %s"
            (fields |> List.map (fun (f : field) -> f.fname.txt) |> list);
          Some (env, fields, typ |> TypeUtils.extractedTypeToString)
        | _ -> None)
      | None -> None
    in
    match extracted with
    | None -> []
    | Some (env, fields, recordAsString) ->
      let fields =
        fields
        |> Utils.filterMap (fun field ->
               if
                 List.mem field.fname.txt seenFields = false
                 && Utils.checkName field.fname.txt ~prefix ~exact:false
               then
                 Some
                   (Completion.create field.fname.txt ~env
                      ?deprecated:field.deprecated ~docstring:field.docstring
                      ~kind:(Completion.Field (field, recordAsString)))
               else None)
      in
      Printf.printf "len: %i" (List.length fields);
      fields)
  | CRecordBody {recordCtxPath; seenFields} -> (
    let completionsForCtxPath =
      recordCtxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
    in
    let extracted =
      match
        completionsForCtxPath
        |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos
             ~scope
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
      let fields =
        fields
        |> Utils.filterMap (fun field ->
               if List.mem field.fname.txt seenFields = false then
                 Some
                   (Completion.create field.fname.txt ~env
                      ?deprecated:field.deprecated ~docstring:field.docstring
                      ~kind:(Completion.Field (field, recordAsString)))
               else None)
      in
      fields)
  | CFunction _ ->
    (* TODO: Support more function stuff? Going from this to Tfunction *) []
  | CNone -> []
  | CRecordFieldFollow {recordCtxPath; fieldName} -> (
    match
      recordCtxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos ~scope
    with
    | Some (typ, env) -> (
      let typ =
        match typ with
        | ExtractedType t -> Some t
        | TypeExpr t -> TypeUtils.extractType ~env ~package t
      in
      match typ with
      | Some (Trecord {fields}) -> (
        match
          fields
          |> Utils.findMap (fun (field : field) ->
                 if field.fname.txt = fieldName then Some field.typ else None)
        with
        | None -> []
        | Some fieldType ->
          [Completion.create "dummy" ~env ~kind:(Value fieldType)])
      | _ -> [])
    | _ -> [])
  | CTypeAtLoc loc -> (
    match
      References.getLocItem ~full ~pos:(Pos.ofLexing loc.loc_start) ~debug
    with
    | None -> []
    | Some {locType = Typed (_, typExpr, _)} ->
      [Completion.create "dummy" ~env ~kind:(Value typExpr)]
    | _ -> [])
  | CFunctionReturnType {functionCtxPath} -> (
    match functionCtxPath with
    | CFunction {returnType} ->
      returnType
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
    | _ -> (
      match
        functionCtxPath
        |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
             ~exact:true ~scope
        |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos
             ~scope
      with
      | Some (ExtractedType (Tfunction {returnType}), env) ->
        [Completion.create "dummy" ~env ~kind:(Completion.Value returnType)]
      | _ -> []))

type completionMode = Pattern of Completable.patternMode | Expression

let rec completeTypedValue ~full ~prefix ~completionContext ~mode
    (t : SharedTypes.completionType) =
  match t with
  | Tbool env ->
    [
      Completion.create "true" ~kind:(Label "bool") ~env;
      Completion.create "false" ~kind:(Label "bool") ~env;
    ]
    |> CompletionBackEnd.filterItems ~prefix
  | Tvariant {env; constructors; variantDecl; variantName} ->
    constructors
    |> List.map (fun (constructor : Constructor.t) ->
           let numArgs =
             match constructor.args with
             | InlineRecord _ -> 1
             | Args args -> List.length args
           in
           Completion.createWithSnippet ?deprecated:constructor.deprecated
             ~name:
               (constructor.cname.txt
               ^ CompletionBackEnd.printConstructorArgs numArgs ~asSnippet:false
               )
             ~insertText:
               (constructor.cname.txt
               ^ CompletionBackEnd.printConstructorArgs numArgs ~asSnippet:true
               )
             ~kind:
               (Constructor
                  (constructor, variantDecl |> Shared.declToString variantName))
             ~env ())
    |> CompletionBackEnd.filterItems ~prefix
  | Tpolyvariant {env; constructors; typeExpr} ->
    constructors
    |> List.map (fun (constructor : polyVariantConstructor) ->
           Completion.createWithSnippet
             ~name:
               ("#" ^ constructor.name
               ^ CompletionBackEnd.printConstructorArgs
                   (List.length constructor.args)
                   ~asSnippet:false)
             ~insertText:
               ((if Utils.startsWith prefix "#" then "" else "#")
               ^ constructor.name
               ^ CompletionBackEnd.printConstructorArgs
                   (List.length constructor.args)
                   ~asSnippet:true)
             ~kind:
               (PolyvariantConstructor
                  (constructor, typeExpr |> Shared.typeToString))
             ~env ())
    |> CompletionBackEnd.filterItems ~prefix
  | Toption (env, t) ->
    let innerType =
      match t with
      | ExtractedType t -> Some t
      | TypeExpr t -> t |> TypeUtils.extractType ~env ~package:full.package
    in
    let expandedCompletions =
      match innerType with
      | None -> []
      | Some innerType ->
        innerType
        |> completeTypedValue ~full ~prefix ~completionContext ~mode
        |> List.map (fun (c : Completion.t) ->
               {
                 c with
                 name = "Some(" ^ c.name ^ ")";
                 sortText = None;
                 insertText =
                   (match c.insertText with
                   | None -> None
                   | Some insertText -> Some ("Some(" ^ insertText ^ ")"));
               })
    in
    let noneCase = Completion.create "None" ~kind:(kindFromInnerType t) ~env in
    let someAnyCase =
      Completion.createWithSnippet ~name:"Some(_)" ~kind:(kindFromInnerType t)
        ~env ~insertText:"Some(${1:_})" ()
    in
    let completions =
      match completionContext with
      | Some (Completable.CameFromRecordField fieldName) ->
        [
          Completion.createWithSnippet
            ~name:("Some(" ^ fieldName ^ ")")
            ~kind:(kindFromInnerType t) ~env
            ~insertText:("Some(${1:" ^ fieldName ^ "})")
            ();
          someAnyCase;
          noneCase;
        ]
      | _ -> [noneCase; someAnyCase]
    in
    completions @ expandedCompletions |> CompletionBackEnd.filterItems ~prefix
  | Tuple (env, exprs, typ) ->
    let numExprs = List.length exprs in
    [
      Completion.createWithSnippet
        ~name:(CompletionBackEnd.printConstructorArgs numExprs ~asSnippet:false)
        ~insertText:
          (CompletionBackEnd.printConstructorArgs numExprs ~asSnippet:true)
        ~kind:(Value typ) ~env ();
    ]
  | Trecord {env; fields} as extractedType -> (
    (* As we're completing for a record, we'll need a hint (completionContext)
       here to figure out whether we should complete for a record field, or
       the record body itself. *)
    match completionContext with
    | Some (Completable.RecordField {seenFields}) ->
      fields
      |> List.filter (fun (field : field) ->
             List.mem field.fname.txt seenFields = false)
      |> List.map (fun (field : field) ->
             match (field.optional, mode) with
             | true, Pattern Destructuring ->
               Completion.create ("?" ^ field.fname.txt)
                 ?deprecated:field.deprecated
                 ~docstring:
                   [
                     field.fname.txt
                     ^ " is an optional field, and needs to be destructured \
                        using '?'.";
                   ]
                 ~kind:
                   (Field (field, TypeUtils.extractedTypeToString extractedType))
                 ~env
             | _ ->
               Completion.create field.fname.txt ?deprecated:field.deprecated
                 ~kind:
                   (Field (field, TypeUtils.extractedTypeToString extractedType))
                 ~env)
      |> CompletionBackEnd.filterItems ~prefix
    | _ ->
      if prefix = "" then
        [
          Completion.createWithSnippet ~name:"{}"
            ~insertText:(if !Cfg.supportsSnippets then "{$0}" else "{}")
            ~sortText:"A"
            ~kind:
              (ExtractedType
                 ( extractedType,
                   match mode with
                   | Pattern _ -> `Type
                   | Expression -> `Value ))
            ~env ();
        ]
      else [])
  | TinlineRecord {env; fields} -> (
    match completionContext with
    | Some (Completable.RecordField {seenFields}) ->
      fields
      |> List.filter (fun (field : field) ->
             List.mem field.fname.txt seenFields = false)
      |> List.map (fun (field : field) ->
             Completion.create field.fname.txt ~kind:(Label "Inline record")
               ?deprecated:field.deprecated ~env)
      |> CompletionBackEnd.filterItems ~prefix
    | _ ->
      if prefix = "" then
        [
          Completion.createWithSnippet ~name:"{}"
            ~insertText:(if !Cfg.supportsSnippets then "{$0}" else "{}")
            ~sortText:"A" ~kind:(Label "Inline record") ~env ();
        ]
      else [])
  | Tarray (env, typ) ->
    if prefix = "" then
      [
        Completion.createWithSnippet ~name:"[]"
          ~insertText:(if !Cfg.supportsSnippets then "[$0]" else "[]")
          ~sortText:"A"
          ~kind:
            (match typ with
            | ExtractedType typ ->
              ExtractedType
                ( typ,
                  match mode with
                  | Pattern _ -> `Type
                  | Expression -> `Value )
            | TypeExpr typ -> Value typ)
          ~env ();
      ]
    else []
  | Tstring env ->
    if prefix = "" then
      [
        Completion.createWithSnippet ~name:"\"\""
          ~insertText:(if !Cfg.supportsSnippets then "\"$0\"" else "\"\"")
          ~sortText:"A"
          ~kind:
            (Value (Ctype.newconstr (Path.Pident (Ident.create "string")) []))
          ~env ();
      ]
    else []
  | Tfunction {env; typ; args; uncurried} when prefix = "" && mode = Expression
    ->
    let shouldPrintAsUncurried = uncurried && !Config.uncurried <> Uncurried in
    let mkFnArgs ~asSnippet =
      match args with
      | [(Nolabel, argTyp)] when TypeUtils.typeIsUnit argTyp ->
        if shouldPrintAsUncurried then "(. )" else "()"
      | [(Nolabel, argTyp)] ->
        let varName =
          CompletionExpressions.prettyPrintFnTemplateArgName ~env ~full argTyp
        in
        let argsText = if asSnippet then "${1:" ^ varName ^ "}" else varName in
        if shouldPrintAsUncurried then "(. " ^ argsText ^ ")" else argsText
      | _ ->
        let currentUnlabelledIndex = ref 0 in
        let argsText =
          args
          |> List.map (fun ((label, typ) : typedFnArg) ->
                 match label with
                 | Optional name -> "~" ^ name ^ "=?"
                 | Labelled name -> "~" ^ name
                 | Nolabel ->
                   if TypeUtils.typeIsUnit typ then "()"
                   else (
                     currentUnlabelledIndex := !currentUnlabelledIndex + 1;
                     let num = !currentUnlabelledIndex in
                     let varName =
                       CompletionExpressions.prettyPrintFnTemplateArgName
                         ~currentIndex:num ~env ~full typ
                     in
                     if asSnippet then
                       "${" ^ string_of_int num ^ ":" ^ varName ^ "}"
                     else varName))
          |> String.concat ", "
        in
        "(" ^ if shouldPrintAsUncurried then ". " else "" ^ argsText ^ ")"
    in
    [
      Completion.createWithSnippet
        ~name:(mkFnArgs ~asSnippet:false ^ " => {}")
        ~insertText:
          (mkFnArgs ~asSnippet:!Cfg.supportsSnippets
          ^ " => "
          ^ if !Cfg.supportsSnippets then "{$0}" else "{}")
        ~sortText:"A" ~kind:(Value typ) ~env ();
    ]
  | Tfunction _ -> []
  | Texn env ->
    [
      Completion.create
        (full.package.builtInCompletionModules.exnModulePath @ ["Error(error)"]
        |> ident)
        ~kind:(Label "Catches errors from JavaScript errors.")
        ~docstring:
          [
            "Matches on a JavaScript error. Read more in the [documentation on \
             catching JS \
             exceptions](https://rescript-lang.org/docs/manual/latest/exception#catching-js-exceptions).";
          ]
        ~env;
    ]
  | Tpromise _ -> []

let rec processCompletable ~debug ~full ~scope ~env ~pos ~forHover completable =
  if debug then
    Printf.printf "Completable: %s\n"
      (CompletionInstruction.toString completable);
  let package = full.package in
  let rawOpens = Scope.getRawOpens scope in
  let opens = getOpens ~debug ~rawOpens ~package ~env in
  let allFiles = allFilesInPackage package in
  let findTypeOfValue path =
    path
    |> getCompletionsForPath ~debug ~completionContext:Value ~exact:true
         ~package ~opens ~full ~pos ~env ~scope
    |> completionsGetTypeEnv ~debug ~full ~opens ~rawOpens ~pos ~scope
  in
  match completable with
  | Cnone -> []
  | CtxPath contextPath ->
    contextPath
    |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
         ~exact:forHover ~scope
  | Cjsx {pathToComponent = [id]; prefix; seenProps = identsSeen}
    when String.uncapitalize_ascii id = id ->
    (* Lowercase JSX tag means builtin *)
    let mkLabel (name, typString) =
      Completion.create name ~kind:(Label typString) ~env
    in
    let keyLabels =
      if Utils.startsWith "key" prefix then [mkLabel ("key", "string")] else []
    in
    (CompletionJsx.domLabels
    |> List.filter (fun (name, _t) ->
           Utils.startsWith name prefix
           && (forHover || not (List.mem name identsSeen)))
    |> List.map mkLabel)
    @ keyLabels
  | Cjsx {pathToComponent = componentPath; prefix; seenProps = identsSeen} ->
    let labels =
      CompletionJsx.getJsxLabels ~componentPath ~findTypeOfValue ~package
    in
    let mkLabel_ name typString =
      Completion.create name ~kind:(Label typString) ~env
    in
    let mkLabel (name, typ, _env) =
      mkLabel_ name (typ |> Shared.typeToString)
    in
    let keyLabels =
      if Utils.startsWith "key" prefix then [mkLabel_ "key" "string"] else []
    in
    if labels = [] then []
    else
      (labels
      |> List.filter (fun (name, _t, _env) ->
             Utils.startsWith name prefix
             && name <> "key"
             && (forHover || not (List.mem name identsSeen)))
      |> List.map mkLabel)
      @ keyLabels
  (* | Cdecorator prefix ->
     let mkDecorator (name, docstring) =
       {(Completion.create name ~kind:(Label "") ~env) with docstring}
     in
     let isTopLevel = String.starts_with ~prefix:"@" prefix in
     let prefix =
       if isTopLevel then String.sub prefix 1 (String.length prefix - 1)
       else prefix
     in
     let decorators =
       if isTopLevel then CompletionDecorators.toplevel
       else CompletionDecorators.local
     in
     decorators
     |> List.filter (fun (decorator, _) -> Utils.startsWith decorator prefix)
     |> List.map (fun (decorator, doc) ->
            let parts = String.split_on_char '.' prefix in
            let len = String.length prefix in
            let dec2 =
              if List.length parts > 1 then
                String.sub decorator len (String.length decorator - len)
              else decorator
            in
            (dec2, doc))
     |> List.map mkDecorator*)
  | CnamedArg {ctxPath; prefix; seenLabels} ->
    let labels =
      match
        ctxPath
        |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
             ~exact:true ~scope
        |> completionsGetTypeEnv ~debug ~full ~opens ~rawOpens ~pos ~scope
      with
      | Some (typ, _env) ->
        if debug then
          Printf.printf "Found type for function %s\n"
            (typ |> Shared.typeToString);

        typ
        |> TypeUtils.getArgs ~full ~env
        |> List.filter_map (fun arg ->
               match arg with
               | SharedTypes.Completable.Labelled name, a -> Some (name, a)
               | Optional name, a -> Some (name, a)
               | _ -> None)
      | None -> []
    in
    let mkLabel (name, typ) =
      Completion.create name ~kind:(Label (typ |> Shared.typeToString)) ~env
    in
    labels
    |> List.filter (fun (name, _t) ->
           Utils.startsWith name prefix
           && (forHover || not (List.mem name seenLabels)))
    |> List.map mkLabel
  | Cpattern {ctxPath; prefix} -> (
    match
      ctxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType2 ~debug ~full ~opens ~rawOpens ~pos ~scope
    with
    | Some (typ, env) -> (
      let typ =
        match typ with
        | ExtractedType inner -> Some inner
        | TypeExpr t -> t |> TypeUtils.extractType ~env ~package:full.package
      in
      match typ with
      | None -> []
      | Some typ ->
        let items =
          typ
          |> completeTypedValue ~mode:(Pattern Default) ~full ~prefix
               ~completionContext:None
        in
        items)
    | None -> [])
  | Cexpression {ctxPath; prefix} -> (
    (* Completions for local things like variables in scope, modules in the
       project, etc. We only add completions when there's a prefix of some sort
       we can filter on, since we know we're in some sort of context, and
       therefore don't want to overwhelm the user with completion items. *)
    let regularCompletions =
      if prefix = "" then []
      else
        prefix
        |> getComplementaryCompletionsForTypedValue ~opens ~allFiles ~env ~scope
    in

    match
      ctxPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType ~full
    with
    | None -> regularCompletions
    | Some (typ, _env) -> (
      (* TODO: We can get rid of the completion context and only use the ctx path *)
      let completionContext =
        match ctxPath with
        | CRecordBody {seenFields} | CRecordField {seenFields} ->
          Some (Completable.RecordField {seenFields})
        | CRecordFieldFollow {fieldName} -> Some (CameFromRecordField fieldName)
        | _ -> None
      in
      let wrapInsertTextInBraces =
        if List.length [] > 0 then false
        else
          match ctxPath with
          | CJsxPropValue _ -> true
          | _ -> false
      in
      let items =
        typ
        |> completeTypedValue ~mode:Expression ~full ~prefix ~completionContext
        |> List.map (fun (c : Completion.t) ->
               if wrapInsertTextInBraces then
                 {
                   c with
                   insertText =
                     (match c.insertText with
                     | None -> None
                     | Some text -> Some ("{" ^ text ^ "}"));
                 }
               else c)
      in
      match (prefix, completionContext) with
      | "", _ -> items
      | _, None ->
        let items =
          if List.length regularCompletions > 0 then
            (* The client will occasionally sort the list of completions alphabetically, disregarding the order
               in which we send it. This fixes that by providing a sort text making the typed completions
               guaranteed to end up on top. *)
            items
            |> List.map (fun (c : Completion.t) ->
                   {c with sortText = Some ("A" ^ " " ^ c.name)})
          else items
        in
        items @ regularCompletions
      | _ -> items))
  (*| CexhaustiveSwitch {contextPath; exprLoc} ->
    let range = Utils.rangeOfLoc exprLoc in
    let printFailwithStr num =
      "${" ^ string_of_int num ^ ":failwith(\"todo\")}"
    in
    let withExhaustiveItem ~cases ?(startIndex = 0) (c : Completion.t) =
      (* We don't need to write out `switch` here since we know that's what the
         user has already written. Just complete for the rest. *)
      let newText =
        c.name ^ " {\n"
        ^ (cases
          |> List.mapi (fun index caseText ->
                 "| " ^ caseText ^ " => "
                 ^ printFailwithStr (startIndex + index + 1))
          |> String.concat "\n")
        ^ "\n}"
        |> Utils.indent range.start.character
      in
      [
        c;
        {
          c with
          name = c.name ^ " (exhaustive switch)";
          filterText = Some c.name;
          insertTextFormat = Some Snippet;
          insertText = Some newText;
          kind = Snippet "insert exhaustive switch for value";
        };
      ]
    in
    let completionsForContextPath =
      contextPath
      |> getCompletionsForContextPath ~debug ~full ~opens ~rawOpens ~pos ~env
           ~exact:forHover ~scope
    in
    completionsForContextPath
    |> List.map (fun (c : Completion.t) ->
           match c.kind with
           | Value typExpr -> (
             match typExpr |> TypeUtils.extractType ~env:c.env ~package with
             | Some (Tvariant v) ->
               withExhaustiveItem c
                 ~cases:
                   (v.constructors
                   |> List.map (fun (constructor : Constructor.t) ->
                          constructor.cname.txt
                          ^
                          match constructor.args with
                          | Args [] -> ""
                          | _ -> "(_)"))
             | Some (Tpolyvariant v) ->
               withExhaustiveItem c
                 ~cases:
                   (v.constructors
                   |> List.map (fun (constructor : polyVariantConstructor) ->
                          "#" ^ constructor.name
                          ^
                          match constructor.args with
                          | [] -> ""
                          | _ -> "(_)"))
             | Some (Toption (_env, _typ)) ->
               withExhaustiveItem c ~cases:["Some($1)"; "None"] ~startIndex:1
             | Some (Tbool _) -> withExhaustiveItem c ~cases:["true"; "false"]
             | _ -> [c])
           | _ -> [c])
    |> List.flatten*)
  | ChtmlElement {prefix} ->
    CompletionJsx.htmlElements
    |> List.filter_map (fun (elementName, description, deprecated) ->
           if Utils.startsWith elementName prefix then
             let name = "<" ^ elementName ^ ">" in
             Some
               (Completion.create name ~kind:(Label name) ~detail:description
                  ~env ~docstring:[description] ~insertText:elementName
                  ?deprecated:
                    (match deprecated with
                    | true -> Some "true"
                    | false -> None))
           else None)
