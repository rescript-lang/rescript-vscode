open SharedTypes

let showConstructor {Constructor.cname = {txt}; args; res} =
  txt
  ^ (match args with
    | Args [] | InlineRecord _ -> ""
    | Args args ->
      "("
      ^ (args
        |> List.map (fun (typ, _) -> typ |> Shared.typeToString)
        |> String.concat ", ")
      ^ ")")
  ^
  match res with
  | None -> ""
  | Some typ -> "\n" ^ (typ |> Shared.typeToString)

(* TODO: local opens *)
let resolveOpens ~env opens ~package =
  List.fold_left
    (fun previous path ->
      (* Finding an open, first trying to find it in previoulsly resolved opens *)
      let rec loop prev =
        match prev with
        | [] -> (
          match path with
          | [] | [_] -> previous
          | name :: path -> (
            match ProcessCmt.fileForModule ~package name with
            | None ->
              Log.log ("Could not get module " ^ name);
              previous (* TODO: warn? *)
            | Some file -> (
              match
                ResolvePath.resolvePath ~env:(QueryEnv.fromFile file) ~package
                  ~path
              with
              | None ->
                Log.log ("Could not resolve in " ^ name);
                previous
              | Some (env, _placeholder) -> previous @ [env])))
        | env :: rest -> (
          match ResolvePath.resolvePath ~env ~package ~path with
          | None -> loop rest
          | Some (env, _placeholder) -> previous @ [env])
      in
      Log.log ("resolving open " ^ pathToString path);
      match ResolvePath.resolvePath ~env ~package ~path with
      | None ->
        Log.log "Not local";
        loop previous
      | Some (env, _) ->
        Log.log "Was local";
        previous @ [env])
    (* loop(previous) *)
    [] opens

let completionForExporteds iterExported getDeclared ~prefix ~exact ~env
    ~namesUsed transformContents =
  let res = ref [] in
  iterExported (fun name stamp ->
      (* Log.log("checking exported: " ++ name); *)
      if Utils.checkName name ~prefix ~exact then
        match getDeclared stamp with
        | Some (declared : _ Declared.t)
          when not (Hashtbl.mem namesUsed declared.name.txt) ->
          Hashtbl.add namesUsed declared.name.txt ();
          res :=
            {
              (Completion.create declared.name.txt ~env
                 ~kind:(transformContents declared.item))
              with
              deprecated = declared.deprecated;
              docstring = declared.docstring;
            }
            :: !res
        | _ -> ());
  !res

let completionForExportedModules ~env ~prefix ~exact ~namesUsed =
  completionForExporteds (Exported.iter env.QueryEnv.exported Exported.Module)
    (Stamps.findModule env.file.stamps) ~prefix ~exact ~env ~namesUsed (fun m ->
      Completion.Module m)

let completionForExportedValues ~env ~prefix ~exact ~namesUsed =
  completionForExporteds (Exported.iter env.QueryEnv.exported Exported.Value)
    (Stamps.findValue env.file.stamps) ~prefix ~exact ~env ~namesUsed (fun v ->
      Completion.Value v)

let completionForExportedTypes ~env ~prefix ~exact ~namesUsed =
  completionForExporteds (Exported.iter env.QueryEnv.exported Exported.Type)
    (Stamps.findType env.file.stamps) ~prefix ~exact ~env ~namesUsed (fun t ->
      Completion.Type t)

let completionsForExportedConstructors ~(env : QueryEnv.t) ~prefix ~exact
    ~namesUsed =
  let res = ref [] in
  Exported.iter env.exported Exported.Type (fun _name stamp ->
      match Stamps.findType env.file.stamps stamp with
      | Some ({item = {kind = Type.Variant constructors}} as t) ->
        res :=
          (constructors
          |> List.filter (fun c ->
                 Utils.checkName c.Constructor.cname.txt ~prefix ~exact)
          |> Utils.filterMap (fun c ->
                 let name = c.Constructor.cname.txt in
                 if not (Hashtbl.mem namesUsed name) then
                   let () = Hashtbl.add namesUsed name () in
                   Some
                     (Completion.create name ~env ~docstring:c.docstring
                        ~kind:
                          (Completion.Constructor
                             (c, t.item.decl |> Shared.declToString t.name.txt)))
                 else None))
          @ !res
      | _ -> ());
  !res

let completionForExportedFields ~(env : QueryEnv.t) ~prefix ~exact ~namesUsed =
  let res = ref [] in
  Exported.iter env.exported Exported.Type (fun _name stamp ->
      match Stamps.findType env.file.stamps stamp with
      | Some ({item = {kind = Record fields}} as t) ->
        res :=
          (fields
          |> List.filter (fun f -> Utils.checkName f.fname.txt ~prefix ~exact)
          |> Utils.filterMap (fun f ->
                 let name = f.fname.txt in
                 if not (Hashtbl.mem namesUsed name) then
                   let () = Hashtbl.add namesUsed name () in
                   Some
                     (Completion.create name ~env ~docstring:f.docstring
                        ~kind:
                          (Completion.Field
                             (f, t.item.decl |> Shared.declToString t.name.txt)))
                 else None))
          @ !res
      | _ -> ());
  !res

let findModuleInScope ~env ~moduleName ~scope =
  let modulesTable = Hashtbl.create 10 in
  env.QueryEnv.file.stamps
  |> Stamps.iterModules (fun _ declared ->
         Hashtbl.replace modulesTable
           (declared.name.txt, declared.extentLoc |> Loc.start)
           declared);
  let result = ref None in
  let processModule name loc =
    if name = moduleName && !result = None then
      match Hashtbl.find_opt modulesTable (name, Loc.start loc) with
      | Some declared -> result := Some declared
      | None ->
        Log.log
          (Printf.sprintf "Module Not Found %s loc:%s\n" name (Loc.toString loc))
  in
  scope |> Scope.iterModulesBeforeFirstOpen processModule;
  scope |> Scope.iterModulesAfterFirstOpen processModule;
  !result

let resolvePathFromStamps ~(env : QueryEnv.t) ~package ~scope ~moduleName ~path
    =
  (* Log.log("Finding from stamps " ++ name); *)
  match findModuleInScope ~env ~moduleName ~scope with
  | None -> None
  | Some declared -> (
    (* Log.log("found it"); *)
    match ResolvePath.findInModule ~env declared.item path with
    | None -> None
    | Some res -> (
      match res with
      | `Local (env, name) -> Some (env, name)
      | `Global (moduleName, fullPath) -> (
        match ProcessCmt.fileForModule ~package moduleName with
        | None -> None
        | Some file ->
          ResolvePath.resolvePath ~env:(QueryEnv.fromFile file) ~path:fullPath
            ~package)))

let resolveModuleWithOpens ~opens ~package ~moduleName =
  let rec loop opens =
    match opens with
    | (env : QueryEnv.t) :: rest -> (
      Log.log ("Looking for env in " ^ Uri.toString env.file.uri);
      match ResolvePath.resolvePath ~env ~package ~path:[moduleName; ""] with
      | Some (env, _) -> Some env
      | None -> loop rest)
    | [] -> None
  in
  loop opens

let resolveFileModule ~moduleName ~package =
  Log.log ("Getting module " ^ moduleName);
  match ProcessCmt.fileForModule ~package moduleName with
  | None -> None
  | Some file ->
    Log.log "got it";
    let env = QueryEnv.fromFile file in
    Some env

let getEnvWithOpens ~scope ~(env : QueryEnv.t) ~package
    ~(opens : QueryEnv.t list) ~moduleName (path : string list) =
  (* TODO: handle interleaving of opens and local modules correctly *)
  match resolvePathFromStamps ~env ~scope ~moduleName ~path ~package with
  | Some x -> Some x
  | None -> (
    match resolveModuleWithOpens ~opens ~package ~moduleName with
    | Some env -> ResolvePath.resolvePath ~env ~package ~path
    | None -> (
      match resolveFileModule ~moduleName ~package with
      | None -> None
      | Some env -> ResolvePath.resolvePath ~env ~package ~path))

let detail name (kind : Completion.kind) =
  match kind with
  | Type {decl} -> decl |> Shared.declToString name
  | Value typ -> typ |> Shared.typeToString
  | ObjLabel typ -> typ |> Shared.typeToString
  | Label typString -> typString
  | Module _ -> "module"
  | FileModule _ -> "file module"
  | Field ({typ}, s) -> name ^ ": " ^ (typ |> Shared.typeToString) ^ "\n\n" ^ s
  | Constructor (c, s) -> showConstructor c ^ "\n\n" ^ s
  | PolyvariantConstructor ({name; args}, s) ->
    "#" ^ name
    ^ (match args with
      | [] -> ""
      | typeExprs ->
        "("
        ^ (typeExprs
          |> List.map (fun typeExpr -> typeExpr |> Shared.typeToString)
          |> String.concat ", ")
        ^ ")")
    ^ "\n\n" ^ s

let findAllCompletions ~(env : QueryEnv.t) ~prefix ~exact ~namesUsed
    ~(completionContext : Completable.completionContext) =
  Log.log ("findAllCompletions uri:" ^ Uri.toString env.file.uri);
  match completionContext with
  | Value ->
    completionForExportedValues ~env ~prefix ~exact ~namesUsed
    @ completionsForExportedConstructors ~env ~prefix ~exact ~namesUsed
    @ completionForExportedModules ~env ~prefix ~exact ~namesUsed
  | Type ->
    completionForExportedTypes ~env ~prefix ~exact ~namesUsed
    @ completionForExportedModules ~env ~prefix ~exact ~namesUsed
  | Module -> completionForExportedModules ~env ~prefix ~exact ~namesUsed
  | Field ->
    completionForExportedFields ~env ~prefix ~exact ~namesUsed
    @ completionForExportedModules ~env ~prefix ~exact ~namesUsed

let processLocalValue name loc ~prefix ~exact ~env
    ~(localTables : LocalTables.t) =
  if Utils.checkName name ~prefix ~exact then
    match Hashtbl.find_opt localTables.valueTable (name, Loc.start loc) with
    | Some declared ->
      if not (Hashtbl.mem localTables.namesUsed name) then (
        Hashtbl.add localTables.namesUsed name ();
        localTables.resultRev <-
          {
            (Completion.create declared.name.txt ~env ~kind:(Value declared.item))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: localTables.resultRev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Value Not Found %s loc:%s\n" name
           (Loc.toString loc));
      localTables.resultRev <-
        Completion.create name ~env
          ~kind:
            (Value
               (Ctype.newconstr
                  (Path.Pident (Ident.create "Type Not Known"))
                  []))
        :: localTables.resultRev

let processLocalConstructor name loc ~prefix ~exact ~env
    ~(localTables : LocalTables.t) =
  if Utils.checkName name ~prefix ~exact then
    match
      Hashtbl.find_opt localTables.constructorTable (name, Loc.start loc)
    with
    | Some declared ->
      if not (Hashtbl.mem localTables.namesUsed name) then (
        Hashtbl.add localTables.namesUsed name ();
        localTables.resultRev <-
          {
            (Completion.create declared.name.txt ~env
               ~kind:
                 (Constructor
                    ( declared.item,
                      snd declared.item.typeDecl
                      |> Shared.declToString (fst declared.item.typeDecl) )))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: localTables.resultRev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Constructor Not Found %s loc:%s\n" name
           (Loc.toString loc))

let processLocalType name loc ~prefix ~exact ~env ~(localTables : LocalTables.t)
    =
  if Utils.checkName name ~prefix ~exact then
    match Hashtbl.find_opt localTables.typesTable (name, Loc.start loc) with
    | Some declared ->
      if not (Hashtbl.mem localTables.namesUsed name) then (
        Hashtbl.add localTables.namesUsed name ();
        localTables.resultRev <-
          {
            (Completion.create declared.name.txt ~env ~kind:(Type declared.item))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: localTables.resultRev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Type Not Found %s loc:%s\n" name
           (Loc.toString loc))

let processLocalModule name loc ~prefix ~exact ~env
    ~(localTables : LocalTables.t) =
  if Utils.checkName name ~prefix ~exact then
    match Hashtbl.find_opt localTables.modulesTable (name, Loc.start loc) with
    | Some declared ->
      if not (Hashtbl.mem localTables.namesUsed name) then (
        Hashtbl.add localTables.namesUsed name ();
        localTables.resultRev <-
          {
            (Completion.create declared.name.txt ~env
               ~kind:(Module declared.item))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: localTables.resultRev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Module Not Found %s loc:%s\n" name
           (Loc.toString loc))

let getItemsFromOpens ~opens ~localTables ~prefix ~exact ~completionContext =
  opens
  |> List.fold_left
       (fun results env ->
         let completionsFromThisOpen =
           findAllCompletions ~env ~prefix ~exact
             ~namesUsed:localTables.LocalTables.namesUsed ~completionContext
         in
         completionsFromThisOpen @ results)
       []

let findLocalCompletionsForValuesAndConstructors ~(localTables : LocalTables.t)
    ~env ~prefix ~exact ~opens ~scope =
  localTables |> LocalTables.populateValues ~env;
  localTables |> LocalTables.populateConstructors ~env;
  localTables |> LocalTables.populateModules ~env;
  scope
  |> Scope.iterValuesBeforeFirstOpen
       (processLocalValue ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterConstructorsBeforeFirstOpen
       (processLocalConstructor ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesBeforeFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);

  let valuesFromOpens =
    getItemsFromOpens ~opens ~localTables ~prefix ~exact
      ~completionContext:Value
  in

  scope
  |> Scope.iterValuesAfterFirstOpen
       (processLocalValue ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterConstructorsAfterFirstOpen
       (processLocalConstructor ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesAfterFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);
  List.rev_append localTables.resultRev valuesFromOpens

let findLocalCompletionsForValues ~(localTables : LocalTables.t) ~env ~prefix
    ~exact ~opens ~scope =
  localTables |> LocalTables.populateValues ~env;
  localTables |> LocalTables.populateModules ~env;
  scope
  |> Scope.iterValuesBeforeFirstOpen
       (processLocalValue ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesBeforeFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);

  let valuesFromOpens =
    getItemsFromOpens ~opens ~localTables ~prefix ~exact
      ~completionContext:Value
  in

  scope
  |> Scope.iterValuesAfterFirstOpen
       (processLocalValue ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesAfterFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);
  List.rev_append localTables.resultRev valuesFromOpens

let findLocalCompletionsForTypes ~(localTables : LocalTables.t) ~env ~prefix
    ~exact ~opens ~scope =
  localTables |> LocalTables.populateTypes ~env;
  localTables |> LocalTables.populateModules ~env;
  scope
  |> Scope.iterTypesBeforeFirstOpen
       (processLocalType ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesBeforeFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);

  let valuesFromOpens =
    getItemsFromOpens ~opens ~localTables ~prefix ~exact ~completionContext:Type
  in

  scope
  |> Scope.iterTypesAfterFirstOpen
       (processLocalType ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesAfterFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);
  List.rev_append localTables.resultRev valuesFromOpens

let findLocalCompletionsForModules ~(localTables : LocalTables.t) ~env ~prefix
    ~exact ~opens ~scope =
  localTables |> LocalTables.populateModules ~env;
  scope
  |> Scope.iterModulesBeforeFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);

  let valuesFromOpens =
    getItemsFromOpens ~opens ~localTables ~prefix ~exact
      ~completionContext:Module
  in

  scope
  |> Scope.iterModulesAfterFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);
  List.rev_append localTables.resultRev valuesFromOpens

let findLocalCompletionsWithOpens ~pos ~(env : QueryEnv.t) ~prefix ~exact ~opens
    ~scope ~(completionContext : Completable.completionContext) =
  (* TODO: handle arbitrary interleaving of opens and local bindings correctly *)
  Log.log
    ("findLocalCompletionsWithOpens uri:" ^ Uri.toString env.file.uri ^ " pos:"
   ^ Pos.toString pos);
  let localTables = LocalTables.create () in
  match completionContext with
  | Value ->
    findLocalCompletionsForValuesAndConstructors ~localTables ~env ~prefix
      ~exact ~opens ~scope
  | Type ->
    findLocalCompletionsForTypes ~localTables ~env ~prefix ~exact ~opens ~scope
  | Module ->
    findLocalCompletionsForModules ~localTables ~env ~prefix ~exact ~opens
      ~scope
  | Field ->
    (* There's no local completion for fields *)
    []

let getComplementaryCompletionsForTypedValue ~opens ~allFiles ~scope ~env prefix
    =
  let exact = false in
  let localCompletionsWithOpens =
    let localTables = LocalTables.create () in
    findLocalCompletionsForValues ~localTables ~env ~prefix ~exact ~opens ~scope
  in
  let fileModules =
    allFiles |> FileSet.elements
    |> Utils.filterMap (fun name ->
           if
             Utils.checkName name ~prefix ~exact
             && not
                  (* TODO complete the namespaced name too *)
                  (String.contains name '-')
           then
             Some
               (Completion.create name ~env ~kind:(Completion.FileModule name))
           else None)
  in
  localCompletionsWithOpens @ fileModules

let getCompletionsForPath ~package ~opens ~allFiles ~pos ~exact ~scope
    ~completionContext ~env path =
  match path with
  | [] -> []
  | [prefix] ->
    let localCompletionsWithOpens =
      findLocalCompletionsWithOpens ~pos ~env ~prefix ~exact ~opens ~scope
        ~completionContext
    in
    let fileModules =
      allFiles |> FileSet.elements
      |> Utils.filterMap (fun name ->
             if
               Utils.checkName name ~prefix ~exact
               && not
                    (* TODO complete the namespaced name too *)
                    (String.contains name '-')
             then
               Some
                 (Completion.create name ~env ~kind:(Completion.FileModule name))
             else None)
    in
    localCompletionsWithOpens @ fileModules
  | moduleName :: path -> (
    Log.log ("Path " ^ pathToString path);
    match getEnvWithOpens ~scope ~env ~package ~opens ~moduleName path with
    | Some (env, prefix) ->
      Log.log "Got the env";
      let namesUsed = Hashtbl.create 10 in
      findAllCompletions ~env ~prefix ~exact ~namesUsed ~completionContext
    | None -> [])

let mkItem ~name ~kind ~detail ~deprecated ~docstring =
  let docContent =
    (match deprecated with
    | None -> ""
    | Some s -> "Deprecated: " ^ s ^ "\n\n")
    ^
    match docstring with
    | [] -> ""
    | _ :: _ -> docstring |> String.concat "\n"
  in
  let tags =
    match deprecated with
    | None -> []
    | Some _ -> [1 (* deprecated *)]
  in
  Protocol.
    {
      label = name;
      kind;
      tags;
      detail;
      documentation =
        (if docContent = "" then None
        else Some {kind = "markdown"; value = docContent});
      sortText = None;
      insertText = None;
      insertTextFormat = None;
    }

let completionToItem
    {
      Completion.name;
      deprecated;
      docstring;
      kind;
      sortText;
      insertText;
      insertTextFormat;
    } =
  let item =
    mkItem ~name
      ~kind:(Completion.kindToInt kind)
      ~deprecated ~detail:(detail name kind) ~docstring
  in
  if !Cfg.supportsSnippets then
    {item with sortText; insertText; insertTextFormat}
  else item

let completionsGetTypeEnv = function
  | {Completion.kind = Value typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = ObjLabel typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = Field ({typ}, _); env} :: _ -> Some (typ, env)
  | _ -> None

let rec getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
    ~exact ~scope (contextPath : Completable.contextPath) =
  let package = full.package in
  match contextPath with
  | CPString ->
    [
      Completion.create "string" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "string")) []));
    ]
  | CPInt ->
    [
      Completion.create "int" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "int")) []));
    ]
  | CPFloat ->
    [
      Completion.create "float" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "float")) []));
    ]
  | CPArray ->
    [
      Completion.create "array" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "array")) []));
    ]
  | CPId (path, completionContext) ->
    path
    |> getCompletionsForPath ~package ~opens ~allFiles ~pos ~exact
         ~completionContext ~env ~scope
  | CPApply (cp, labels) -> (
    match
      cp
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
           ~exact:true ~scope
      |> completionsGetTypeEnv
    with
    | Some (typ, env) -> (
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
    | None -> [])
  | CPField (CPId (path, Module), fieldName) ->
    (* M.field *)
    path @ [fieldName]
    |> getCompletionsForPath ~package ~opens ~allFiles ~pos ~exact
         ~completionContext:Field ~env ~scope
  | CPField (cp, fieldName) -> (
    match
      cp
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
           ~exact:true ~scope
      |> completionsGetTypeEnv
    with
    | Some (typ, env) -> (
      match typ |> TypeUtils.extractRecordType ~env ~package with
      | Some (env, fields, typDecl) ->
        fields
        |> Utils.filterMap (fun field ->
               if Utils.checkName field.fname.txt ~prefix:fieldName ~exact then
                 Some
                   (Completion.create field.fname.txt ~env
                      ~docstring:field.docstring
                      ~kind:
                        (Completion.Field
                           ( field,
                             typDecl.item.decl
                             |> Shared.declToString typDecl.name.txt )))
               else None)
      | None -> [])
    | None -> [])
  | CPObj (cp, label) -> (
    match
      cp
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
           ~exact:true ~scope
      |> completionsGetTypeEnv
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
  | CPPipe {contextPath = cp; id = funNamePrefix; lhsLoc; inJsx} -> (
    match
      cp
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
           ~exact:true ~scope
      |> completionsGetTypeEnv
    with
    | Some (typ, envFromCompletionItem) -> (
      let env, typ =
        typ |> TypeUtils.resolveTypeForPipeCompletion ~env ~package
      in

      (* If the type we're completing on is a type parameter, we won't be able to do
         completion unless we know what that type parameter is compiled as. This
         attempts to look up the compiled type for that type parameter by looking
         for compiled information at the loc of that expression. *)
      let typ =
        match typ with
        | {Types.desc = Tvar _} -> (
          match
            TypeUtils.findReturnTypeOfFunctionAtLoc lhsLoc ~env ~full
              ~debug:false
          with
          | None -> typ
          | Some typFromLoc -> typFromLoc)
        | _ -> typ
      in
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
      let getBuiltinTypePath path =
        match path with
        | Path.Pident id when Ident.name id = "array" -> Some arrayModulePath
        | Path.Pident id when Ident.name id = "option" -> Some optionModulePath
        | Path.Pident id when Ident.name id = "string" -> Some stringModulePath
        | Path.Pident id when Ident.name id = "int" -> Some intModulePath
        | Path.Pident id when Ident.name id = "float" -> Some floatModulePath
        | Path.Pident id when Ident.name id = "promise" ->
          Some promiseModulePath
        | Path.Pident id when Ident.name id = "list" -> Some listModulePath
        | Path.Pident id when Ident.name id = "result" -> Some resultModulePath
        | Path.Pident id when Ident.name id = "lazy_t" -> Some ["Lazy"]
        | Path.Pident id when Ident.name id = "char" -> Some ["Char"]
        | Pdot (Pident id, "result", _) when Ident.name id = "Pervasives" ->
          Some resultModulePath
        | _ -> None
      in
      let rec expandPath (path : Path.t) =
        match path with
        | Pident id -> [Ident.name id]
        | Pdot (p, s, _) -> s :: expandPath p
        | Papply _ -> []
      in
      let getTypePath typ =
        match typ.Types.desc with
        | Tconstr (path, _typeArgs, _)
        | Tlink {desc = Tconstr (path, _typeArgs, _)}
        | Tsubst {desc = Tconstr (path, _typeArgs, _)}
        | Tpoly ({desc = Tconstr (path, _typeArgs, _)}, []) ->
          Some path
        | _ -> None
      in
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
      let completionPath =
        match getTypePath typ with
        | Some typePath -> (
          match getBuiltinTypePath typePath with
          | Some path -> Some path
          | None -> (
            match expandPath typePath with
            | _ :: pathRev ->
              (* type path is relative to the completion environment
                 express it from the root of the file *)
              let pathFromEnv_ =
                QueryEnv.pathFromEnv envFromCompletionItem (List.rev pathRev)
              in
              if pathFromEnv_ = [] then None
              else
                let pathFromEnv =
                  if env.file.moduleName = envFromCompletionItem.file.moduleName
                  then pathFromEnv_
                  else envFromCompletionItem.file.moduleName :: pathFromEnv_
                in
                Some pathFromEnv
            | _ -> None))
        | None -> None
      in
      match completionPath with
      | Some completionPath -> (
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
          |> getCompletionsForPath ~completionContext:Value ~exact:false
               ~package ~opens ~allFiles ~pos ~env ~scope
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
        let forJsxCompletion =
          if inJsx then
            match getTypePath typ with
            | Some (Path.Pident id) when Ident.name id = "int" -> Some "int"
            | Some (Path.Pident id) when Ident.name id = "float" -> Some "float"
            | Some (Path.Pident id) when Ident.name id = "string" ->
              Some "string"
            | Some (Path.Pident id) when Ident.name id = "array" -> Some "array"
            | _ -> None
          else None
        in
        match forJsxCompletion with
        | Some builtinNameToComplete
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
      | None -> [])
    | None -> [])
  | CTuple ctxPaths ->
    (* Turn a list of context paths into a list of type expressions. *)
    let typeExrps =
      ctxPaths
      |> List.map (fun contextPath ->
             contextPath
             |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles
                  ~pos ~env ~exact:true ~scope)
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
      |> getCompletionsForPath ~completionContext:Value ~exact:true ~package
           ~opens ~allFiles ~pos ~env ~scope
      |> completionsGetTypeEnv
    in
    let targetLabel =
      CompletionJsx.getJsxLabels ~componentPath:pathToComponent ~findTypeOfValue
        ~package
      |> List.find_opt (fun (label, _, _) -> label = propName)
    in
    match targetLabel with
    | None -> []
    | Some (_, typ, env) ->
      [
        Completion.create "dummy" ~env
          ~kind:(Completion.Value (Utils.unwrapIfOption typ));
      ])
  | CArgument {functionContextPath; argumentLabel} -> (
    let labels, env =
      match
        functionContextPath
        |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos
             ~env ~exact:true ~scope
        |> completionsGetTypeEnv
      with
      | Some (typ, env) -> (typ |> TypeUtils.getArgs ~full ~env, env)
      | None -> ([], env)
    in
    let targetLabel =
      labels
      |> List.find_opt (fun (label, _) ->
             match argumentLabel with
             | Unlabelled _ -> label = argumentLabel
             | Labelled name | Optional name -> (
               match label with
               | (Labelled n | Optional n) when name = n -> true
               | _ -> false))
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

let getOpens ~debug ~rawOpens ~package ~env =
  if debug && rawOpens <> [] then
    Printf.printf "%s\n"
      ("Raw opens: "
      ^ string_of_int (List.length rawOpens)
      ^ " "
      ^ String.concat " ... " (rawOpens |> List.map pathToString));
  let packageOpens = package.opens in
  if debug && packageOpens <> [] then
    Printf.printf "%s\n"
      ("Package opens "
      ^ String.concat " " (packageOpens |> List.map pathToString));
  let resolvedOpens =
    resolveOpens ~env (List.rev (packageOpens @ rawOpens)) ~package
  in
  if debug && resolvedOpens <> [] then
    Printf.printf "%s\n"
      ("Resolved opens "
      ^ string_of_int (List.length resolvedOpens)
      ^ " "
      ^ String.concat " "
          (resolvedOpens
          |> List.map (fun (e : QueryEnv.t) -> Uri.toString e.file.uri)));
  (* Last open takes priority *)
  List.rev resolvedOpens

let filterItems items ~prefix =
  if prefix = "" then items
  else
    items
    |> List.filter (fun (item : Completion.t) ->
           Utils.startsWith item.name prefix)

let printConstructorArgs argsLen ~asSnippet =
  let args = ref [] in
  for argNum = 1 to argsLen do
    args :=
      !args @ [(if asSnippet then Printf.sprintf "${%i:_}" argNum else "_")]
  done;
  if List.length !args > 0 then "(" ^ (!args |> String.concat ", ") ^ ")"
  else ""

let rec completeTypedValue (t : SharedTypes.completionType) ~env ~full ~prefix
    ~completionContext =
  let extractedType =
    match t with
    | TypeExpr t -> t |> TypeUtils.extractType ~env ~package:full.package
    | InlineRecord fields -> Some (TinlineRecord {env; fields})
  in
  match extractedType with
  | Some (Tbool env) ->
    [
      Completion.create "true" ~kind:(Label "bool") ~env;
      Completion.create "false" ~kind:(Label "bool") ~env;
    ]
    |> filterItems ~prefix
  | Some (Tvariant {env; constructors; variantDecl; variantName}) ->
    constructors
    |> List.map (fun (constructor : Constructor.t) ->
           let numArgs =
             match constructor.args with
             | InlineRecord _ -> 1
             | Args args -> List.length args
           in
           Completion.createWithSnippet
             ~name:
               (constructor.cname.txt
               ^ printConstructorArgs numArgs ~asSnippet:false)
             ~insertText:
               (constructor.cname.txt
               ^ printConstructorArgs numArgs ~asSnippet:true)
             ~kind:
               (Constructor
                  (constructor, variantDecl |> Shared.declToString variantName))
             ~env ())
    |> filterItems ~prefix
  | Some (Tpolyvariant {env; constructors; typeExpr}) ->
    constructors
    |> List.map (fun (constructor : polyVariantConstructor) ->
           Completion.createWithSnippet
             ~name:
               ("#" ^ constructor.name
               ^ printConstructorArgs
                   (List.length constructor.args)
                   ~asSnippet:false)
             ~insertText:
               ((if Utils.startsWith prefix "#" then "" else "#")
               ^ constructor.name
               ^ printConstructorArgs
                   (List.length constructor.args)
                   ~asSnippet:true)
             ~kind:
               (PolyvariantConstructor
                  (constructor, typeExpr |> Shared.typeToString))
             ~env ())
    |> filterItems ~prefix
  | Some (Toption (env, t)) ->
    let innerType = Utils.unwrapIfOption t in
    let expandedCompletions =
      TypeExpr innerType
      |> completeTypedValue ~env ~full ~prefix ~completionContext
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
    [
      Completion.create "None" ~kind:(Label (t |> Shared.typeToString)) ~env;
      Completion.createWithSnippet ~name:"Some(_)"
        ~kind:(Label (t |> Shared.typeToString))
        ~env ~insertText:"Some(${1:_})" ();
    ]
    @ expandedCompletions
    |> filterItems ~prefix
  | Some (Tuple (env, exprs, typ)) ->
    let numExprs = List.length exprs in
    [
      Completion.createWithSnippet
        ~name:(printConstructorArgs numExprs ~asSnippet:false)
        ~insertText:(printConstructorArgs numExprs ~asSnippet:true)
        ~kind:(Value typ) ~env ();
    ]
  | Some (Trecord {env; fields; typeExpr}) -> (
    (* As we're completing for a record, we'll need a hint (completionContext)
       here to figure out whether we should complete for a record field, or
       the record body itself. *)
    match completionContext with
    | Some (Completable.RecordField {seenFields}) ->
      fields
      |> List.filter (fun (field : field) ->
             List.mem field.fname.txt seenFields = false)
      |> List.map (fun (field : field) ->
             Completion.create field.fname.txt
               ~kind:(Field (field, typeExpr |> Shared.typeToString))
               ~env)
      |> filterItems ~prefix
    | None ->
      if prefix = "" then
        [
          Completion.createWithSnippet ~name:"{}"
            ~insertText:(if !Cfg.supportsSnippets then "{$0}" else "{}")
            ~sortText:"A" ~kind:(Value typeExpr) ~env ();
        ]
      else [])
  | Some (TinlineRecord {env; fields}) -> (
    match completionContext with
    | Some (Completable.RecordField {seenFields}) ->
      fields
      |> List.filter (fun (field : field) ->
             List.mem field.fname.txt seenFields = false)
      |> List.map (fun (field : field) ->
             Completion.create field.fname.txt ~kind:(Label "Inline record")
               ~env)
      |> filterItems ~prefix
    | None ->
      if prefix = "" then
        [
          Completion.createWithSnippet ~name:"{}"
            ~insertText:(if !Cfg.supportsSnippets then "{$0}" else "{}")
            ~sortText:"A" ~kind:(Label "Inline record") ~env ();
        ]
      else [])
  | Some (Tarray (env, typeExpr)) ->
    if prefix = "" then
      [
        Completion.createWithSnippet ~name:"[]"
          ~insertText:(if !Cfg.supportsSnippets then "[$0]" else "[]")
          ~sortText:"A" ~kind:(Value typeExpr) ~env ();
      ]
    else []
  | Some (Tstring env) ->
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
  | _ -> []

let rec processCompletable ~debug ~full ~scope ~env ~pos ~forHover
    (completable : Completable.t) =
  let package = full.package in
  let rawOpens = Scope.getRawOpens scope in
  let opens = getOpens ~debug ~rawOpens ~package ~env in
  let allFiles = FileSet.union package.projectFiles package.dependenciesFiles in
  let findTypeOfValue path =
    path
    |> getCompletionsForPath ~completionContext:Value ~exact:true ~package
         ~opens ~allFiles ~pos ~env ~scope
    |> completionsGetTypeEnv
  in
  match completable with
  | Cnone -> []
  | Cpath contextPath ->
    contextPath
    |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
         ~exact:forHover ~scope
  | Cjsx ([id], prefix, identsSeen) when String.uncapitalize_ascii id = id ->
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
  | Cjsx (componentPath, prefix, identsSeen) ->
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
  | Cdecorator prefix ->
    let mkDecorator (name, docstring) =
      {(Completion.create name ~kind:(Label "") ~env) with docstring}
    in
    CompletionDecorators.decorators
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
    |> List.map mkDecorator
  | CnamedArg (cp, prefix, identsSeen) ->
    let labels =
      match
        cp
        |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos
             ~env ~exact:true ~scope
        |> completionsGetTypeEnv
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
           && (forHover || not (List.mem name identsSeen)))
    |> List.map mkLabel
  | Cpattern {contextPath; prefix; nested; fallback} -> (
    let fallbackOrEmpty ?items () =
      match (fallback, items) with
      | Some fallback, (None | Some []) ->
        fallback |> processCompletable ~debug ~full ~scope ~env ~pos ~forHover
      | _, Some items -> items
      | None, None -> []
    in
    match
      contextPath
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
           ~exact:true ~scope
      |> completionsGetTypeEnv
    with
    | Some (typ, env) -> (
      match
        TypeExpr typ
        |> TypeUtils.resolveNested ~env ~package:full.package ~nested
      with
      | None -> fallbackOrEmpty ()
      | Some (typ, env, completionContext) ->
        let items =
          typ |> completeTypedValue ~env ~full ~prefix ~completionContext
        in
        fallbackOrEmpty ~items ())
    | None -> fallbackOrEmpty ())
  | Cexpression {contextPath; prefix; nested} -> (
    match
      contextPath
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
           ~exact:true ~scope
      |> completionsGetTypeEnv
    with
    | None -> []
    | Some (typ, env) -> (
      match
        TypeExpr typ
        |> TypeUtils.resolveNested ~env ~package:full.package ~nested
      with
      | None -> []
      | Some (typ, env, completionContext) -> (
        let items =
          typ |> completeTypedValue ~env ~full ~prefix ~completionContext
        in
        match (prefix, completionContext) with
        | "", _ -> items
        | _, None ->
          (* Completions for local things like variables in scope, modules in the project, etc. *)
          let regularCompletions =
            prefix
            |> getComplementaryCompletionsForTypedValue ~opens ~allFiles ~env
                 ~scope
          in
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
        | _ -> items)))
