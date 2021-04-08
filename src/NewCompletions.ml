open SharedTypes

let showConstructor {cname = {txt}; args; res} =
  let open Infix in
  txt
  ^ ( match args = [] with
    | true -> ""
    | false ->
      "("
      ^ String.concat ", "
          (args |> List.map (fun (typ, _) -> typ |> Shared.typeToString))
      ^ ")" )
  ^ (res |?>> (fun typ -> "\n" ^ (typ |> Shared.typeToString)) |? "")

(* TODO: local opens *)
let resolveOpens ~env ~previous opens ~getModule =
  List.fold_left
    (fun previous path ->
      (** Finding an open, first trying to find it in previoulsly resolved opens *)
      let rec loop prev =
        match prev with
        | [] -> (
          match path with
          | Tip _ -> previous
          | Nested (name, path) -> (
            match getModule name with
            | None ->
              Log.log ("Could not get module " ^ name);
              previous (* TODO: warn? *)
            | Some file -> (
              match
                Query.resolvePath ~env:(Query.fileEnv file) ~getModule ~path
              with
              | None ->
                Log.log ("Could not resolve in " ^ name);
                previous
              | Some (env, _placeholder) -> previous @ [env] ) ) )
        | env :: rest -> (
          match Query.resolvePath ~env ~getModule ~path with
          | None -> loop rest
          | Some (env, _placeholder) -> previous @ [env] )
      in
      Log.log ("resolving open " ^ pathToString path);
      match Query.resolvePath ~env ~getModule ~path with
      | None ->
        Log.log "Not local";
        loop previous
      | Some (env, _) ->
        Log.log "Was local";
        previous @ [env])
    (* loop(previous) *)
    previous opens

let completionForDeclareds ~pos declareds prefix transformContents =
  (* Log.log("complete for declares " ++ prefix); *)
  Hashtbl.fold
    (fun _stamp declared results ->
      if
        Utils.startsWith declared.name.txt prefix
        && Utils.locationContainsFuzzy declared.scopeLoc pos
      then {declared with item = transformContents declared.item} :: results
      else
        (* Log.log("Nope doesn't count " ++ Utils.showLocation(declared.scopeLoc) ++ " " ++ m); *)
        results
    )
    declareds []

let completionForExporteds exporteds
    (stamps : (int, 'a SharedTypes.declared) Hashtbl.t) prefix transformContents
    =
  Hashtbl.fold
    (fun name stamp results ->
      (* Log.log("checking exported: " ++ name); *)
      if Utils.startsWith name prefix then
        let declared = Hashtbl.find stamps stamp in
        {declared with item = transformContents declared.item} :: results
      else results)
    exporteds []

let completionForConstructors exportedTypes
    (stamps : (int, SharedTypes.Type.t SharedTypes.declared) Hashtbl.t) prefix =
  Hashtbl.fold
    (fun _name stamp results ->
      let t = Hashtbl.find stamps stamp in
      match t.item.kind with
      | SharedTypes.Type.Variant constructors ->
        ( constructors
        |> List.filter (fun c -> Utils.startsWith c.cname.txt prefix)
        |> List.map (fun c -> (c, t)) )
        @ results
      | _ -> results)
    exportedTypes []

let completionForFields exportedTypes
    (stamps : (int, SharedTypes.Type.t SharedTypes.declared) Hashtbl.t) prefix =
  Hashtbl.fold
    (fun _name stamp results ->
      let t = Hashtbl.find stamps stamp in
      match t.item.kind with
      | Record fields ->
        ( fields
        |> List.filter (fun f -> Utils.startsWith f.fname.txt prefix)
        |> List.map (fun f -> (f, t)) )
        @ results
      | _ -> results)
    exportedTypes []

let isCapitalized name =
  if name = "" then false
  else
    let c = name.[0] in
    match c with 'A' .. 'Z' -> true | _ -> false

let determineCompletion items =
  let rec loop offset items =
    match items with
    | [] -> assert false
    | [one] -> `Normal (Tip one)
    | [one; two] when not (isCapitalized one) -> `Attribute ([one], two)
    | [one; two] -> `Normal (Nested (one, Tip two))
    | one :: rest -> (
      if isCapitalized one then
        match loop (offset + String.length one + 1) rest with
        | `Normal path -> `Normal (Nested (one, path))
        | x -> x
      else
        match loop (offset + String.length one + 1) rest with
        | `Normal path -> `AbsAttribute path
        | `Attribute (path, suffix) -> `Attribute (one :: path, suffix)
        | x -> x )
  in
  loop 0 items

(* Note: This is a hack. It will be wrong some times if you have a local thing
   that overrides an open.

   Maybe the way to fix it is to make note of what things in an open override
   locally defined things...
*)
let getEnvWithOpens ~pos ~(env : Query.queryEnv) ~getModule
    ~(opens : Query.queryEnv list) path =
  (* Query.resolvePath(~env, ~path, ~getModule) *)
  match Query.resolveFromStamps ~env ~path ~getModule ~pos with
  | Some x -> Some x
  | None ->
    let rec loop opens =
      match opens with
      | env :: rest -> (
        Log.log ("Looking for env in " ^ Uri2.toString env.Query.file.uri);
        match Query.resolvePath ~env ~getModule ~path with
        | Some x -> Some x
        | None -> loop rest )
      | [] -> (
        match path with
        | Tip _ -> None
        | Nested (top, path) -> (
          Log.log ("Getting module " ^ top);
          match getModule top with
          | None -> None
          | Some file ->
            Log.log "got it";
            let env = Query.fileEnv file in
            Query.resolvePath ~env ~getModule ~path
            |> Infix.logIfAbsent "Unable to resolve the path" ) )
    in
    loop opens

type k =
  | Module of moduleKind
  | Value of Types.type_expr
  | Type of Type.t
  | Constructor of constructor * Type.t declared
  | Field of field * Type.t declared
  | FileModule of string

let kindToInt k =
  match k with
  | Module _ -> 9
  | FileModule _ -> 9
  | Constructor (_, _) -> 4
  | Field (_, _) -> 5
  | Type _ -> 22
  | Value _ -> 12

let detail name contents =
  match contents with
  | Type {decl} -> decl |> Shared.declToString name
  | Value typ -> typ |> Shared.typeToString
  | Module _ -> "module"
  | FileModule _ -> "file module"
  | Field ({typ}, t) ->
    name ^ ": "
    ^ (typ |> Shared.typeToString)
    ^ "\n\n"
    ^ (t.item.decl |> Shared.declToString t.name.txt)
  | Constructor (c, t) ->
    showConstructor c ^ "\n\n" ^ (t.item.decl |> Shared.declToString t.name.txt)

let localValueCompletions ~pos ~(env : Query.queryEnv) suffix =
  let results = [] in
  Log.log "---------------- LOCAL VAL";
  let results =
    if suffix = "" || isCapitalized suffix then
      results
      @ completionForDeclareds ~pos env.file.stamps.modules suffix (fun m ->
            Module m)
      @ ( completionForConstructors env.exported.types env.file.stamps.types
        (* TODO declared thingsz *)
            suffix
        |> List.map (fun (c, t) ->
               {(emptyDeclared c.cname.txt) with item = Constructor (c, t)}) )
    else results
  in
  let results =
    if suffix = "" || not (isCapitalized suffix) then
      results
      @ completionForDeclareds ~pos env.file.stamps.values suffix (fun v ->
            Value v)
      @ completionForDeclareds ~pos env.file.stamps.types suffix (fun t ->
            Type t)
      @ ( completionForFields env.exported.types env.file.stamps.types suffix
        |> List.map (fun (f, t) ->
               {(emptyDeclared f.fname.txt) with item = Field (f, t)}) )
    else results
  in
  results |> List.map (fun x -> (env.file.uri, x))

let valueCompletions ~(env : Query.queryEnv) suffix =
  Log.log (" - Completing in " ^ Uri2.toString env.file.uri);
  let results = [] in
  let results =
    if suffix = "" || isCapitalized suffix then (
      (* Get rid of lowercase modules (#417) *)
      env.exported.modules
      |> Hashtbl.filter_map_inplace (fun name key ->
             match isCapitalized name with true -> Some key | false -> None);
      let moduleCompletions =
        completionForExporteds env.exported.modules env.file.stamps.modules
          suffix (fun m -> Module m)
      in
      (* Log.log(" -- capitalized " ++ string_of_int(Hashtbl.length(env.exported.types)) ++ " exported types"); *)
      (* env.exported.types |> Hashtbl.iter((name, _) => Log.log("    > " ++ name)); *)
      results @ moduleCompletions
      @ (
        (* TODO declared thingsz *)
        completionForConstructors env.exported.types env.file.stamps.types suffix
        |> List.map (fun (c, t) ->
               {(emptyDeclared c.cname.txt) with item = Constructor (c, t)}) ) )
    else results
  in
  let results =
    if suffix = "" || not (isCapitalized suffix) then (
      Log.log " -- not capitalized";
      results
      @ completionForExporteds env.exported.values env.file.stamps.values suffix
          (fun v -> Value v)
      @ completionForExporteds env.exported.types env.file.stamps.types suffix
          (fun t -> Type t)
      @ ( completionForFields env.exported.types env.file.stamps.types suffix
        |> List.map (fun (f, t) ->
               {(emptyDeclared f.fname.txt) with item = Field (f, t)}) ) )
    else results
  in
  (* Log.log("Getting value completions " ++ env.file.uri);
   Log.log(String.concat(", ", results |. Belt.List.map(x => x.name.txt))); *)
  results |> List.map (fun x -> (env.file.uri, x))

let attributeCompletions ~(env : Query.queryEnv) ~suffix =
  let results = [] in
  let results =
    if suffix = "" || isCapitalized suffix then
      results
      @ completionForExporteds env.exported.modules env.file.stamps.modules
          suffix (fun m -> Module m)
    else results
  in
  let results =
    if suffix = "" || not (isCapitalized suffix) then
      results
      @ completionForExporteds env.exported.values env.file.stamps.values suffix
          (fun v -> Value v)
      (* completionForExporteds(env.exported.types, env.file.stamps.types, suffix, t => Type(t)) @ *)
      @ ( completionForFields env.exported.types env.file.stamps.types suffix
        |> List.map (fun (f, t) ->
               {(emptyDeclared f.fname.txt) with item = Field (f, t)}) )
    else results
  in
  results |> List.map (fun x -> (env.file.uri, x))

(* TODO filter out things that are defined after the current position *)
let resolveRawOpens ~env ~getModule ~rawOpens ~package =
  (* TODO Stdlib instead of Pervasives *)
  let packageOpens = "Pervasives" :: package.TopTypes.opens in
  Log.log ("Package opens " ^ String.concat " " packageOpens);
  let opens =
    resolveOpens ~env
      ~previous:
        (List.map Query.fileEnv (packageOpens |> Utils.filterMap getModule))
      rawOpens ~getModule
  in
  opens
  [@@ocaml.doc
    "\n\nTODO filter out things that are defined after the current position\n\n"]

let getItems ~full ~package ~rawOpens ~getModule ~allModules ~pos ~parts =
  Log.log
    ( "Opens folkz > "
    ^ string_of_int (List.length rawOpens)
    ^ " "
    ^ String.concat " ... " (rawOpens |> List.map pathToString) );
  let env = Query.fileEnv full.file in
  let packageOpens = "Pervasives" :: package.TopTypes.opens in
  Log.log ("Package opens " ^ String.concat " " packageOpens);
  let resolvedOpens = resolveRawOpens ~env ~getModule ~rawOpens ~package in
  Log.log
    ( "Opens nows "
    ^ string_of_int (List.length resolvedOpens)
    ^ " "
    ^ String.concat " "
        (resolvedOpens |> List.map (fun e -> Uri2.toString e.Query.file.uri)) );
  (* Last open takes priority *)
  let opens = List.rev resolvedOpens in
  match parts with
  | [] -> []
  | [suffix] ->
    let locallyDefinedValues = localValueCompletions ~pos ~env suffix in
    let alreadyUsedIdentifiers = Hashtbl.create 10 in
    let valuesFromOpens =
      opens
      |> List.fold_left
           (fun results env ->
             let completionsFromThisOpen = valueCompletions ~env suffix in
             List.filter
               (fun (_uri, declared) ->
                 if not (Hashtbl.mem alreadyUsedIdentifiers declared.name.txt)
                 then (
                   Hashtbl.add alreadyUsedIdentifiers declared.name.txt true;
                   true )
                 else false)
               completionsFromThisOpen
             @ results)
           []
    in
    (* TODO complete the namespaced name too *)
    let localModuleNames =
      allModules
      |> Utils.filterMap (fun name ->
             match
               Utils.startsWith name suffix && not (String.contains name '-')
             with
             | true ->
               Some
                 ( env.file.uri,
                   {(emptyDeclared name) with item = FileModule name} )
             | false -> None)
    in
    locallyDefinedValues @ valuesFromOpens @ localModuleNames
  | multiple -> (
    Log.log ("Completing for " ^ String.concat "<.>" multiple);
    match determineCompletion multiple with
    | `Normal path -> (
      Log.log ("normal " ^ pathToString path);
      match getEnvWithOpens ~pos ~env ~getModule ~opens path with
      | Some (env, suffix) ->
        Log.log "Got the env";
        valueCompletions ~env suffix
      | None -> [] )
    | `Attribute (target, suffix) -> (
      Log.log ("suffix :" ^ suffix);
      match target with
      | [] -> []
      | first :: rest -> (
        Log.log ("-------------- Looking for " ^ first);
        match Query.findInScope pos first env.file.stamps.values with
        | None -> []
        | Some declared -> (
          Log.log ("Found it! " ^ declared.name.txt);
          match declared.item |> Shared.digConstructor with
          | None -> []
          | Some path -> (
            match Hover.digConstructor ~env ~getModule path with
            | None -> []
            | Some (env, typ) -> (
              match
                rest
                |> List.fold_left
                     (fun current name ->
                       match current with
                       | None -> None
                       | Some (env, typ) -> (
                         match typ.item.SharedTypes.Type.kind with
                         | Record fields -> (
                           match
                             fields
                             |> List.find_opt (fun f -> f.fname.txt = name)
                           with
                           | None -> None
                           | Some attr -> (
                             Log.log ("Found attr " ^ name);
                             match attr.typ |> Shared.digConstructor with
                             | None -> None
                             | Some path ->
                               Hover.digConstructor ~env ~getModule path ) )
                         | _ -> None ))
                     (Some (env, typ))
              with
              | None -> []
              | Some (env, typ) -> (
                match typ.item.kind with
                | Record fields ->
                  fields
                  |> Utils.filterMap (fun f ->
                         if Utils.startsWith f.fname.txt suffix then
                           Some
                             ( env.file.uri,
                               {
                                 (emptyDeclared f.fname.txt) with
                                 item = Field (f, typ);
                               } )
                         else None)
                | _ -> [] ) ) ) ) ) )
    | `AbsAttribute path -> (
      match getEnvWithOpens ~pos ~env ~getModule ~opens path with
      | None -> []
      | Some (env, suffix) ->
        attributeCompletions ~env ~suffix
        @ List.concat
            (opens |> List.map (fun env -> attributeCompletions ~env ~suffix)) )
    )

module J = JsonShort

let mkItem ~name ~kind ~detail ~deprecated ~docstring ~uri ~pos_lnum =
  let valueMessage =
    (match deprecated with None -> "" | Some s -> "Deprecated: " ^ s ^ "\n\n")
    ^ ( match docstring with
      | [] -> ""
      | _ :: _ -> (docstring |> String.concat "\n") ^ "\n\n" )
    ^ "\n" ^ Uri2.toString uri ^ ":" ^ string_of_int pos_lnum
  in
  let tags = match deprecated = None with true -> [] | false -> [J.i 1 (* deprecated *)] in
  J.o
    [
      ("label", J.s name);
      ("kind", J.i kind);
      ("tags", J.l tags);
      ("detail", detail |> J.s);
      ( "documentation",
        J.o [("kind", J.s "markdown"); ("value", J.s valueMessage)] );
    ]

let processCompletable ~findItems ~full ~package ~pos ~rawOpens
    (completable : PartialParser.completable) =
  match completable with
  | Cjsx (componentPath, prefix) ->
    let items = findItems ~exact:true (componentPath @ ["make"]) in
    let labels =
      match items with
      | (_uri, {SharedTypes.item = Value typ}) :: _ ->
        let rec getFields (texp : Types.type_expr) =
          match texp.desc with
          | Tfield (name, _, t1, t2) ->
            let fields = t2 |> getFields in
            (name, t1) :: fields
          | Tlink te -> te |> getFields
          | Tvar None -> []
          | _ -> []
        in
        let rec getLabels (t : Types.type_expr) =
          match t.desc with
          | Tlink t1 | Tsubst t1 -> getLabels t1
          | Tarrow
              ( Nolabel,
                {
                  desc =
                    ( Tconstr (* Js.t *) (_, [{desc = Tobject (tObj, _)}], _)
                    | Tobject (tObj, _) );
                },
                _,
                _ ) ->
            getFields tObj
          | _ -> []
        in
        typ |> getLabels
      | _ -> []
    in
    let mkLabel_ name typString =
      mkItem ~name ~kind:4 ~deprecated:None ~detail:typString ~docstring:[]
        ~uri:full.file.uri ~pos_lnum:(fst pos)
    in
    let mkLabel (name, typ) = mkLabel_ name (typ |> Shared.typeToString) in
    let keyLabel = mkLabel_ "key" "string" in
    if labels = [] then []
    else
      keyLabel
      :: ( labels
         |> List.filter (fun (name, _t) -> Utils.startsWith name prefix)
         |> List.map mkLabel )
  | Cpath parts ->
    let items = parts |> findItems ~exact:false in
    (* TODO(#107): figure out why we're getting duplicates. *)
    items |> Utils.dedup
    |> List.map
         (fun
           ( uri,
             {
               SharedTypes.name = {txt = name; loc = {loc_start = {pos_lnum}}};
               deprecated;
               docstring;
               item;
             } )
         ->
           mkItem ~name ~kind:(kindToInt item) ~deprecated
             ~detail:(detail name item) ~docstring ~uri ~pos_lnum)
  | Cpipe s -> (
    let getLhsType ~lhs ~partialName =
      match [lhs] |> findItems ~exact:true with
      | (_uri, {SharedTypes.item = Value t}) :: _ -> Some (t, partialName)
      | _ -> None
    in
    let lhsType =
      match Str.split (Str.regexp_string "->") s with
      | [lhs] -> getLhsType ~lhs ~partialName:""
      | [lhs; partialName] -> getLhsType ~lhs ~partialName
      | _ ->
        (* Only allow one -> *)
        None
    in
    let removePackageOpens modulePath =
      match modulePath with
      | toplevel :: rest when package.TopTypes.opens |> List.mem toplevel ->
        rest
      | _ -> modulePath
    in
    let rec removeRawOpen rawOpen modulePath =
      match (rawOpen, modulePath) with
      | Tip _, _ -> Some modulePath
      | Nested (s, inner), first :: restPath when s = first ->
        removeRawOpen inner restPath
      | _ -> None
    in
    let rec removeRawOpens rawOpens modulePath =
      match rawOpens with
      | rawOpen :: restOpens ->
        let newModulePath =
          match removeRawOpen rawOpen modulePath with
          | None -> modulePath
          | Some newModulePath -> newModulePath
        in
        removeRawOpens restOpens newModulePath
      | [] -> modulePath
    in
    match lhsType with
    | Some (t, partialName) -> (
      let getModulePath path =
        let rec loop (path : Path.t) =
          match path with
          | Pident id -> [Ident.name id]
          | Pdot (p, s, _) -> s :: loop p
          | Papply _ -> []
        in
        match loop path with _ :: rest -> List.rev rest | [] -> []
      in
      let modulePath =
        match t.desc with
        | Tconstr (path, _, _) -> getModulePath path
        | Tlink {desc = Tconstr (path, _, _)} -> getModulePath path
        | _ -> []
      in
      match modulePath with
      | _ :: _ ->
        let modulePathMinusOpens =
          modulePath |> removePackageOpens |> removeRawOpens rawOpens
          |> String.concat "."
        in
        let completionName name =
          match modulePathMinusOpens = "" with
          | true -> name
          | false -> modulePathMinusOpens ^ "." ^ name
        in
        let parts = modulePath @ [partialName] in
        let items = parts |> findItems ~exact:false in
        items
        |> List.filter (fun (_, {item}) ->
               match item with Value _ -> true | _ -> false)
        |> List.map
             (fun
               ( uri,
                 {
                   SharedTypes.name =
                     {txt = name; loc = {loc_start = {pos_lnum}}};
                   deprecated;
                   docstring;
                   item;
                 } )
             ->
               mkItem ~name:(completionName name) ~kind:(kindToInt item)
                 ~detail:(detail name item) ~deprecated ~docstring ~uri
                 ~pos_lnum)
      | _ -> [] )
    | None -> [] )
  | Cdecorator prefix ->
    let mkDecorator name =
      mkItem ~name ~kind:4 ~deprecated:None ~detail:"" ~docstring:[]
        ~uri:full.file.uri ~pos_lnum:(fst pos)
    in
    [
      "as";
      "deriving";
      "genType";
      "genType.as";
      "genType.import";
      "genType.opaque";
      "get";
      "get_index";
      "inline";
      "int";
      "meth";
      "module";
      "new";
      "obj";
      "react.component";
      "return";
      "scope";
      "send";
      "set";
      "set_index";
      "string";
      "this";
      "unboxed";
      "uncurry";
      "unwrap";
      "val";
      "variadic";
    ]
    |> List.filter (fun decorator -> Utils.startsWith decorator prefix)
    |> List.map mkDecorator
  | Clabel (funPath, prefix) ->
    let labels =
      match funPath |> findItems ~exact:true with
      | (_uri, {SharedTypes.item = Value typ}) :: _ ->
        let rec getLabels (t : Types.type_expr) =
          match t.desc with
          | Tlink t1 | Tsubst t1 -> getLabels t1
          | Tarrow ((Labelled l | Optional l), tArg, tRet, _) ->
            (l, tArg) :: getLabels tRet
          | Tarrow (Nolabel, _, tRet, _) -> getLabels tRet
          | _ -> []
        in
        typ |> getLabels
      | _ -> []
    in
    let mkLabel (name, typ) =
      mkItem ~name ~kind:4 ~deprecated:None
        ~detail:(typ |> Shared.typeToString)
        ~docstring:[] ~uri:full.file.uri ~pos_lnum:(fst pos)
    in
    labels
    |> List.filter (fun (name, _t) -> Utils.startsWith name prefix)
    |> List.map mkLabel

let computeCompletions ~full ~maybeText ~package ~pos ~state =
  let items =
    match maybeText with
    | None -> []
    | Some text -> (
      match PartialParser.positionToOffset text pos with
      | None -> []
      | Some offset -> (
        match PartialParser.findCompletable text offset with
        | None -> []
        | Some completable ->
          let rawOpens = PartialParser.findOpens text offset in
          let allModules =
            package.TopTypes.localModules @ package.dependencyModules
          in
          let findItems ~exact parts =
            let items =
              getItems ~full ~package ~rawOpens
                ~getModule:(State.fileForModule state ~package)
                ~allModules ~pos ~parts
            in
            match parts |> List.rev with
            | last :: _ when exact ->
              items
              |> List.filter (fun (_uri, {SharedTypes.name = {txt}}) ->
                     txt = last)
            | _ -> items
          in
          completable
          |> processCompletable ~findItems ~full ~package ~pos ~rawOpens ) )
  in
  if items = [] then J.null else items |> J.l
