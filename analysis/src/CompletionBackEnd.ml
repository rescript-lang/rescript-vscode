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
                        ?deprecated:c.deprecated
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
                        ?deprecated:f.deprecated
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

let kindToDetail name (kind : Completion.kind) =
  match kind with
  | Type {decl} -> decl |> Shared.declToString name
  | Value typ -> typ |> Shared.typeToString
  | ObjLabel typ -> typ |> Shared.typeToString
  | Label typString -> typString
  | Module _ -> "module"
  | FileModule _ -> "file module"
  | Field ({typ; optional}, s) ->
    (* Handle optional fields. Checking for "?" is because sometimes optional
       fields are prefixed with "?" when completing, and at that point we don't
       need to _also_ add a "?" after the field name, as that looks weird. *)
    if optional && Utils.startsWith name "?" = false then
      name ^ "?: "
      ^ (typ |> Utils.unwrapIfOption |> Shared.typeToString)
      ^ "\n\n" ^ s
    else name ^ ": " ^ (typ |> Shared.typeToString) ^ "\n\n" ^ s
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
  | Snippet s -> s
  | FollowContextPath _ -> ""
  | ExtractedType (extractedType, _) ->
    TypeUtils.extractedTypeToString extractedType

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

let processLocalValue name loc contextPath ~prefix ~exact ~env
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
            (match contextPath with
            | Some contextPath -> FollowContextPath contextPath
            | None ->
              Value
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
      filterText = None;
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
      filterText;
      detail;
    } =
  let item =
    mkItem ~name
      ~kind:(Completion.kindToInt kind)
      ~deprecated
      ~detail:
        (match detail with
        | None -> kindToDetail name kind
        | Some detail -> detail)
      ~docstring
  in
  if !Cfg.supportsSnippets then
    {item with sortText; insertText; insertTextFormat; filterText}
  else item

let completionsGetTypeEnv = function
  | {Completion.kind = Value typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = ObjLabel typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = Field ({typ}, _); env} :: _ -> Some (typ, env)
  | _ -> None

type getCompletionsForContextPathMode = Regular | Pipe

type completionsTypeEnvTyp =
  | TypeExpr of Types.type_expr
  | ExtractedType of completionType

let completionsGetCompletionType ~full = function
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

let rec completionsGetCompletionType2 ~full ~opens ~rawOpens ~allFiles ~pos
    ~scope = function
  | {Completion.kind = Value typ; env} :: _
  | {Completion.kind = ObjLabel typ; env} :: _
  | {Completion.kind = Field ({typ}, _); env} :: _ ->
    Some (TypeExpr typ, env)
  | {Completion.kind = FollowContextPath ctxPath; env} :: _ ->
    ctxPath
    |> getCompletionsForContextPath ~full ~env ~exact:true ~opens ~rawOpens
         ~allFiles ~pos ~scope
    |> completionsGetCompletionType2 ~full ~opens ~rawOpens ~allFiles ~pos
         ~scope
  | {Completion.kind = Type typ; env} :: _ -> (
    match TypeUtils.extractTypeFromResolvedType typ ~env ~full with
    | None -> None
    | Some extractedType -> Some (ExtractedType extractedType, env))
  | {Completion.kind = ExtractedType (typ, _); env} :: _ ->
    Some (ExtractedType typ, env)
  | _ -> None

and completionsGetTypeEnv2 (completions : Completion.t list) ~full ~opens
    ~rawOpens ~allFiles ~pos ~scope =
  match completions with
  | {Completion.kind = Value typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = ObjLabel typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = Field ({typ}, _); env} :: _ -> Some (typ, env)
  | {Completion.kind = FollowContextPath ctxPath; env} :: _ ->
    ctxPath
    |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
         ~exact:true ~scope
    |> completionsGetTypeEnv2 ~full ~opens ~rawOpens ~allFiles ~pos ~scope
  | _ -> None

and getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
    ~exact ~scope ?(mode = Regular) (contextPath : Completable.contextPath) =
  let package = full.package in
  match contextPath with
  | CPString ->
    [
      Completion.create "dummy" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "string")) []));
    ]
  | CPBool ->
    [
      Completion.create "dummy" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "bool")) []));
    ]
  | CPInt ->
    [
      Completion.create "dummy" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "int")) []));
    ]
  | CPFloat ->
    [
      Completion.create "dummy" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "float")) []));
    ]
  | CPArray None ->
    [
      Completion.create "array" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "array")) []));
    ]
  | CPArray (Some cp) -> (
    match mode with
    | Regular -> (
      match
        cp
        |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos
             ~env ~exact:true ~scope
        |> completionsGetCompletionType ~full
      with
      | None -> []
      | Some (typ, env) ->
        [
          Completion.create "dummy" ~env
            ~kind:
              (Completion.ExtractedType (Tarray (env, ExtractedType typ), `Type));
        ])
    | Pipe ->
      (* Pipe completion with array just needs to know that it's an array, not
         what inner type it has. *)
      [
        Completion.create "dummy" ~env
          ~kind:
            (Completion.Value
               (Ctype.newconstr (Path.Pident (Ident.create "array")) []));
      ])
  | CPOption cp -> (
    match
      cp
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
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
  | CPId (path, completionContext) ->
    path
    |> getCompletionsForPath ~package ~opens ~allFiles ~pos ~exact
         ~completionContext ~env ~scope
  | CPApply (cp, labels) -> (
    (* TODO: Also needs to support ExtractedType *)
    match
      cp
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
           ~exact:true ~scope
      |> completionsGetTypeEnv2 ~full ~opens ~rawOpens ~allFiles ~pos ~scope
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
    let completionsForCtxPath =
      cp
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
           ~exact:true ~scope
    in
    let extracted =
      match
        completionsForCtxPath
        |> completionsGetCompletionType2 ~full ~opens ~rawOpens ~allFiles ~pos
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
                    ~docstring:field.docstring
                    ~kind:(Completion.Field (field, recordAsString)))
             else None))
  | CPObj (cp, label) -> (
    (* TODO: Also needs to support ExtractedType *)
    match
      cp
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
           ~exact:true ~scope
      |> completionsGetTypeEnv2 ~full ~opens ~rawOpens ~allFiles ~pos ~scope
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
           ~exact:true ~scope ~mode:Pipe
      |> completionsGetTypeEnv2 ~full ~opens ~rawOpens ~allFiles ~pos ~scope
    with
    | None -> []
    | Some (typ, envFromCompletionItem) -> (
      let env, typ =
        typ
        |> TypeUtils.resolveTypeForPipeCompletion ~env ~package ~full ~lhsLoc
      in
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
            match Utils.expandPath path with
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
      |> completionsGetTypeEnv2 ~full ~opens ~rawOpens ~allFiles ~pos ~scope
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
            |> getCompletionsForPath ~completionContext:Type ~exact:true
                 ~package ~opens ~allFiles ~pos ~env ~scope
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
  | CArgument {functionContextPath; argumentLabel} -> (
    (* TODO: Also needs to support ExtractedType *)
    let labels, env =
      match
        functionContextPath
        |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos
             ~env ~exact:true ~scope
        |> completionsGetTypeEnv2 ~full ~opens ~rawOpens ~allFiles ~pos ~scope
      with
      | Some (typ, env) -> (typ |> TypeUtils.getArgs ~full ~env, env)
      | None -> ([], env)
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
  | CPatternPath {rootCtxPath; nested} -> (
    match
      rootCtxPath
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType2 ~full ~opens ~rawOpens ~allFiles ~pos
           ~scope
    with
    | Some (typ, env) -> (
      let typ =
        match typ with
        | ExtractedType typ -> Some typ
        | TypeExpr typ -> typ |> TypeUtils.extractType ~env ~package
      in
      match typ with
      | None -> []
      | Some typ -> (
        match typ |> TypeUtils.resolveNested ~env ~full ~nested with
        | Some (typ, env, _completionContext) ->
          [
            Completion.create "dummy" ~env
              ~kind:(Completion.ExtractedType (typ, `Value));
          ]
        | None -> []))
    | None -> [])

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

type completionMode = Pattern of Completable.patternMode | Expression

let rec completeTypedValue ~full ~prefix ~completionContext ~mode
    (t : SharedTypes.completionType) =
  match t with
  | Tbool env ->
    [
      Completion.create "true" ~kind:(Label "bool") ~env;
      Completion.create "false" ~kind:(Label "bool") ~env;
    ]
    |> filterItems ~prefix
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
               ^ printConstructorArgs numArgs ~asSnippet:false)
             ~insertText:
               (constructor.cname.txt
               ^ printConstructorArgs numArgs ~asSnippet:true)
             ~kind:
               (Constructor
                  (constructor, variantDecl |> Shared.declToString variantName))
             ~env ())
    |> filterItems ~prefix
  | Tpolyvariant {env; constructors; typeExpr} ->
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
    [
      Completion.create "None" ~kind:(kindFromInnerType t) ~env;
      Completion.createWithSnippet ~name:"Some(_)" ~kind:(kindFromInnerType t)
        ~env ~insertText:"Some(${1:_})" ();
    ]
    @ expandedCompletions
    |> filterItems ~prefix
  | Tuple (env, exprs, typ) ->
    let numExprs = List.length exprs in
    [
      Completion.createWithSnippet
        ~name:(printConstructorArgs numExprs ~asSnippet:false)
        ~insertText:(printConstructorArgs numExprs ~asSnippet:true)
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
      |> filterItems ~prefix
    | None ->
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
      |> filterItems ~prefix
    | None ->
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
  | Tfunction {env; typ; args} when prefix = "" && mode = Expression ->
    let prettyPrintArgTyp ?currentIndex (argTyp : Types.type_expr) =
      let indexText =
        match currentIndex with
        | None -> ""
        | Some i -> string_of_int i
      in
      match argTyp |> TypeUtils.pathFromTypeExpr with
      | None -> "v" ^ indexText
      | Some p -> (
        (* Pretty print a few common patterns. *)
        match Path.head p |> Ident.name with
        | "unit" -> "()"
        | "ReactEvent" | "JsxEvent" -> "event"
        | _ -> "v" ^ indexText)
    in
    let mkFnArgs ~asSnippet =
      match args with
      | [(Nolabel, argTyp)] when TypeUtils.typeIsUnit argTyp -> "()"
      | [(Nolabel, argTyp)] ->
        let varName = prettyPrintArgTyp argTyp in
        if asSnippet then "${1:" ^ varName ^ "}" else varName
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
                     let varName = prettyPrintArgTyp typ ~currentIndex:num in
                     if asSnippet then
                       "${" ^ string_of_int num ^ ":" ^ varName ^ "}"
                     else varName))
          |> String.concat ", "
        in
        "(" ^ argsText ^ ")"
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
    |> completionsGetTypeEnv2 ~full ~opens ~rawOpens ~allFiles ~pos ~scope
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
        |> completionsGetTypeEnv2 ~full ~opens ~rawOpens ~allFiles ~pos ~scope
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
  | Cpattern {contextPath; prefix; nested; fallback; patternMode} -> (
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
      |> completionsGetTypeEnv2 ~full ~opens ~rawOpens ~allFiles ~pos ~scope
    with
    | Some (typ, env) -> (
      match
        typ
        |> TypeUtils.extractType ~env ~package:full.package
        |> Utils.Option.flatMap (fun typ ->
               typ |> TypeUtils.resolveNested ~env ~full ~nested)
      with
      | None -> fallbackOrEmpty ()
      | Some (typ, _env, completionContext) ->
        let items =
          typ
          |> completeTypedValue ~mode:(Pattern patternMode) ~full ~prefix
               ~completionContext
        in
        fallbackOrEmpty ~items ())
    | None -> fallbackOrEmpty ())
  | Cexpression {contextPath; prefix; nested} -> (
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
      contextPath
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
           ~exact:true ~scope
      |> completionsGetCompletionType ~full
    with
    | None -> regularCompletions
    | Some (typ, env) -> (
      match typ |> TypeUtils.resolveNested ~env ~full ~nested with
      | None -> regularCompletions
      | Some (typ, _env, completionContext) -> (
        (* Wrap the insert text in braces when we're completing the root of a
           JSX prop value. *)
        let wrapInsertTextInBraces =
          if List.length nested > 0 then false
          else
            match contextPath with
            | CJsxPropValue _ -> true
            | _ -> false
        in
        let items =
          typ
          |> completeTypedValue ~mode:Expression ~full ~prefix
               ~completionContext
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
        | _ -> items)))
  | CexhaustiveSwitch {contextPath; exprLoc} ->
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
      |> getCompletionsForContextPath ~full ~opens ~rawOpens ~allFiles ~pos ~env
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
                          "| #" ^ constructor.name
                          ^
                          match constructor.args with
                          | [] -> ""
                          | _ -> "(_)"))
             | Some (Toption (_env, _typ)) ->
               withExhaustiveItem c ~cases:["Some($1)"; "None"] ~startIndex:1
             | Some (Tbool _) -> withExhaustiveItem c ~cases:["true"; "false"]
             | _ -> [c])
           | _ -> [c])
    |> List.flatten
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
