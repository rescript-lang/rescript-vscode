open SharedTypes

let instantiateType ~typeParams ~typeArgs (t : Types.type_expr) =
  if typeParams = [] || typeArgs = [] then t
  else
    let rec applySub tp ta t =
      match (tp, ta) with
      | t1 :: tRest1, t2 :: tRest2 ->
        if t1 = t then t2 else applySub tRest1 tRest2 t
      | [], _ | _, [] -> t
    in
    let rec loop (t : Types.type_expr) =
      match t.desc with
      | Tlink t -> loop t
      | Tvar _ -> applySub typeParams typeArgs t
      | Tunivar _ -> t
      | Tconstr (path, args, memo) ->
        {t with desc = Tconstr (path, args |> List.map loop, memo)}
      | Tsubst t -> loop t
      | Tvariant rd -> {t with desc = Tvariant (rowDesc rd)}
      | Tnil -> t
      | Tarrow (lbl, t1, t2, c) ->
        {t with desc = Tarrow (lbl, loop t1, loop t2, c)}
      | Ttuple tl -> {t with desc = Ttuple (tl |> List.map loop)}
      | Tobject (t, r) -> {t with desc = Tobject (loop t, r)}
      | Tfield (n, k, t1, t2) -> {t with desc = Tfield (n, k, loop t1, loop t2)}
      | Tpoly (t, []) -> loop t
      | Tpoly (t, tl) -> {t with desc = Tpoly (loop t, tl |> List.map loop)}
      | Tpackage (p, l, tl) ->
        {t with desc = Tpackage (p, l, tl |> List.map loop)}
    and rowDesc (rd : Types.row_desc) =
      let row_fields =
        rd.row_fields |> List.map (fun (l, rf) -> (l, rowField rf))
      in
      let row_more = loop rd.row_more in
      let row_name =
        match rd.row_name with
        | None -> None
        | Some (p, tl) -> Some (p, tl |> List.map loop)
      in
      {rd with row_fields; row_more; row_name}
    and rowField (rf : Types.row_field) =
      match rf with
      | Rpresent None -> rf
      | Rpresent (Some t) -> Rpresent (Some (loop t))
      | Reither (b1, tl, b2, r) -> Reither (b1, tl |> List.map loop, b2, r)
      | Rabsent -> Rabsent
    in
    loop t

let rec extractRecordType ~env ~package (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> extractRecordType ~env ~package t1
  | Tconstr (path, typeArgs, _) -> (
    match References.digConstructor ~env ~package path with
    | Some (env, ({item = {kind = Record fields}} as typ)) ->
      let typeParams = typ.item.decl.type_params in
      let fields =
        fields
        |> List.map (fun field ->
               let fieldTyp =
                 field.typ |> instantiateType ~typeParams ~typeArgs
               in
               {field with typ = fieldTyp})
      in
      Some (env, fields, typ)
    | Some
        ( env,
          {item = {decl = {type_manifest = Some t1; type_params = typeParams}}}
        ) ->
      let t1 = t1 |> instantiateType ~typeParams ~typeArgs in
      extractRecordType ~env ~package t1
    | _ -> None)
  | _ -> None

let rec extractObjectType ~env ~package (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> extractObjectType ~env ~package t1
  | Tobject (tObj, _) -> Some (env, tObj)
  | Tconstr (path, typeArgs, _) -> (
    match References.digConstructor ~env ~package path with
    | Some
        ( env,
          {item = {decl = {type_manifest = Some t1; type_params = typeParams}}}
        ) ->
      let t1 = t1 |> instantiateType ~typeParams ~typeArgs in
      extractObjectType ~env ~package t1
    | _ -> None)
  | _ -> None

let rec extractFunctionType ~env ~package typ =
  let rec loop ~env acc (t : Types.type_expr) =
    match t.desc with
    | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> loop ~env acc t1
    | Tarrow (label, tArg, tRet, _) -> loop ~env ((label, tArg) :: acc) tRet
    | Tconstr (Pident {name = "function$"}, [t; _], _) ->
      extractFunctionType ~env ~package t
    | Tconstr (path, typeArgs, _) -> (
      match References.digConstructor ~env ~package path with
      | Some
          ( env,
            {
              item = {decl = {type_manifest = Some t1; type_params = typeParams}};
            } ) ->
        let t1 = t1 |> instantiateType ~typeParams ~typeArgs in
        loop ~env acc t1
      | _ -> (List.rev acc, t))
    | _ -> (List.rev acc, t)
  in
  loop ~env [] typ

(** Pulls out a type we can complete from a type expr. *)
let rec extractType ~env ~package (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> extractType ~env ~package t1
  | Tconstr (Path.Pident {name = "option"}, [payloadTypeExpr], _) ->
    Some (Toption (env, TypeExpr payloadTypeExpr))
  | Tconstr (Path.Pident {name = "promise"}, [payloadTypeExpr], _) ->
    Some (Tpromise (env, payloadTypeExpr))
  | Tconstr (Path.Pident {name = "array"}, [payloadTypeExpr], _) ->
    Some (Tarray (env, TypeExpr payloadTypeExpr))
  | Tconstr (Path.Pident {name = "bool"}, [], _) -> Some (Tbool env)
  | Tconstr (Path.Pident {name = "string"}, [], _) -> Some (Tstring env)
  | Tconstr (Path.Pident {name = "exn"}, [], _) -> Some (Texn env)
  | Tconstr (Pident {name = "function$"}, [t; _], _) -> (
    (* Uncurried functions. *)
    match extractFunctionType t ~env ~package with
    | args, tRet when args <> [] ->
      Some (Tfunction {env; args; typ = t; returnType = tRet; uncurried = true})
    | _args, _tRet -> None)
  | Tconstr (path, typeArgs, _) -> (
    match References.digConstructor ~env ~package path with
    | Some (_env, {item = {decl = {type_manifest = Some t1; type_params}}}) ->
      t1
      |> instantiateType ~typeParams:type_params ~typeArgs
      |> extractType ~env ~package
    | Some (_env, {name; item = {decl; kind = Type.Variant constructors}}) ->
      Some
        (Tvariant
           {
             env;
             constructors;
             variantName = name.txt;
             variantDecl = decl;
             typeArgs;
             typeParams = decl.type_params;
           })
    | Some (env, {item = {kind = Record fields}}) ->
      Some (Trecord {env; fields; definition = `TypeExpr t})
    | _ -> None)
  | Ttuple expressions -> Some (Tuple (env, expressions, t))
  | Tvariant {row_fields} ->
    let constructors =
      row_fields
      |> List.map (fun (label, field) ->
             {
               name = label;
               args =
                 (* Multiple arguments are represented as a Ttuple, while a single argument is just the type expression itself. *)
                 (match field with
                 | Types.Rpresent (Some typeExpr) -> (
                   match typeExpr.desc with
                   | Ttuple args -> args
                   | _ -> [typeExpr])
                 | _ -> []);
             })
    in
    Some (Tpolyvariant {env; constructors; typeExpr = t})
  | Tarrow _ -> (
    match extractFunctionType t ~env ~package with
    | args, tRet when args <> [] ->
      Some
        (Tfunction {env; args; typ = t; uncurried = false; returnType = tRet})
    | _args, _tRet -> None)
  | _ -> None

let findReturnTypeOfFunctionAtLoc loc ~(env : QueryEnv.t) ~full ~debug =
  match References.getLocItem ~full ~pos:(loc |> Loc.end_) ~debug with
  | Some {locType = Typed (_, typExpr, _)} -> (
    match extractFunctionType ~env ~package:full.package typExpr with
    | args, tRet when args <> [] -> Some tRet
    | _ -> None)
  | _ -> None

type builtinType =
  | Array
  | Option
  | String
  | Int
  | Float
  | Promise
  | List
  | Result
  | Lazy
  | Char

type pipeCompletionType =
  | Builtin of builtinType * Types.type_expr
  | TypExpr of Types.type_expr

let getBuiltinFromTypePath path =
  match path with
  | Path.Pident id when Ident.name id = "array" -> Some Array
  | Path.Pident id when Ident.name id = "option" -> Some Option
  | Path.Pident id when Ident.name id = "string" -> Some String
  | Path.Pident id when Ident.name id = "int" -> Some Int
  | Path.Pident id when Ident.name id = "float" -> Some Float
  | Path.Pident id when Ident.name id = "promise" -> Some Promise
  | Path.Pident id when Ident.name id = "list" -> Some List
  | Path.Pident id when Ident.name id = "result" -> Some Result
  | Path.Pident id when Ident.name id = "lazy_t" -> Some Lazy
  | Path.Pident id when Ident.name id = "char" -> Some Char
  | Pdot (Pident id, "result", _)
    when Ident.name id = "Pervasives" || Ident.name id = "PervasivesU" ->
    Some Result
  | _ -> None

let rec pathFromTypeExpr (t : Types.type_expr) =
  match t.desc with
  | Tconstr (Pident {name = "function$"}, [t; _], _) -> pathFromTypeExpr t
  | Tconstr (path, _typeArgs, _)
  | Tlink {desc = Tconstr (path, _typeArgs, _)}
  | Tsubst {desc = Tconstr (path, _typeArgs, _)}
  | Tpoly ({desc = Tconstr (path, _typeArgs, _)}, []) ->
    Some path
  | _ -> None

let rec digToRelevantTemplateNameType ~env ~package ?(suffix = "")
    (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
    digToRelevantTemplateNameType ~suffix ~env ~package t1
  | Tconstr (Path.Pident {name = "option"}, [t1], _) ->
    digToRelevantTemplateNameType ~suffix ~env ~package t1
  | Tconstr (Path.Pident {name = "array"}, [t1], _) ->
    digToRelevantTemplateNameType ~suffix:"s" ~env ~package t1
  | Tconstr (path, _, _) -> (
    match References.digConstructor ~env ~package path with
    | Some (env, {item = {decl = {type_manifest = Some typ}}}) ->
      digToRelevantTemplateNameType ~suffix ~env ~package typ
    | _ -> (t, suffix, env))
  | _ -> (t, suffix, env)

let rec resolveTypeForPipeCompletion ~env ~package ~lhsLoc ~full
    (t : Types.type_expr) =
  let builtin =
    match t |> pathFromTypeExpr with
    | Some path -> path |> getBuiltinFromTypePath
    | None -> None
  in
  match builtin with
  | Some builtin -> (env, Builtin (builtin, t))
  | None -> (
    (* If the type we're completing on is a type parameter, we won't be able to
       do completion unless we know what that type parameter is compiled as.
       This attempts to look up the compiled type for that type parameter by
       looking for compiled information at the loc of that expression. *)
    let typFromLoc =
      match t with
      | {Types.desc = Tvar _} -> (
        match findReturnTypeOfFunctionAtLoc lhsLoc ~env ~full ~debug:false with
        | None -> None
        | Some typFromLoc -> Some typFromLoc)
      | _ -> None
    in
    match typFromLoc with
    | Some typFromLoc ->
      typFromLoc |> resolveTypeForPipeCompletion ~lhsLoc ~env ~package ~full
    | None ->
      let rec digToRelevantType ~env ~package (t : Types.type_expr) =
        match t.desc with
        | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
          digToRelevantType ~env ~package t1
        (* Don't descend into types named "t". Type t is a convention in the ReScript ecosystem. *)
        | Tconstr (path, _, _) when path |> Path.last = "t" -> (env, TypExpr t)
        | Tconstr (path, _, _) -> (
          match References.digConstructor ~env ~package path with
          | Some (env, {item = {decl = {type_manifest = Some typ}}}) ->
            digToRelevantType ~env ~package typ
          | _ -> (env, TypExpr t))
        | _ -> (env, TypExpr t)
      in
      digToRelevantType ~env ~package t)

let extractTypeFromResolvedType (typ : Type.t) ~env ~full =
  match typ.kind with
  | Tuple items -> Some (Tuple (env, items, Ctype.newty (Ttuple items)))
  | Record fields ->
    Some (Trecord {env; fields; definition = `NameOnly typ.name})
  | Variant constructors ->
    Some
      (Tvariant
         {
           env;
           constructors;
           variantName = typ.name;
           variantDecl = typ.decl;
           typeParams = typ.decl.type_params;
           typeArgs = [];
         })
  | Abstract _ | Open -> (
    match typ.decl.type_manifest with
    | None -> None
    | Some t -> t |> extractType ~env ~package:full.package)

(** The context we just came from as we resolve the nested structure. *)
type ctx = Rfield of string  (** A record field of name *)

(** This moves through a nested path via a set of instructions, trying to resolve the type at the end of the path. *)
let rec resolveNested ~env ~full ~nested ?ctx (typ : completionType) =
  match nested with
  | [] ->
    Some
      ( typ,
        env,
        match ctx with
        | None -> None
        | Some (Rfield fieldName) ->
          Some (Completable.CameFromRecordField fieldName) )
  | patternPath :: nested -> (
    match (patternPath, typ) with
    | Completable.NTupleItem {itemNum}, Tuple (env, tupleItems, _) -> (
      match List.nth_opt tupleItems itemNum with
      | None -> None
      | Some typ ->
        typ
        |> extractType ~env ~package:full.package
        |> Utils.Option.flatMap (fun typ ->
               typ |> resolveNested ~env ~full ~nested))
    | ( NFollowRecordField {fieldName},
        (TinlineRecord {env; fields} | Trecord {env; fields}) ) -> (
      match
        fields
        |> List.find_opt (fun (field : field) -> field.fname.txt = fieldName)
      with
      | None -> None
      | Some {typ; optional} ->
        let typ = if optional then Utils.unwrapIfOption typ else typ in
        typ
        |> extractType ~env ~package:full.package
        |> Utils.Option.flatMap (fun typ ->
               typ |> resolveNested ~ctx:(Rfield fieldName) ~env ~full ~nested))
    | NRecordBody {seenFields}, Trecord {env; definition = `TypeExpr typeExpr}
      ->
      typeExpr
      |> extractType ~env ~package:full.package
      |> Option.map (fun typ ->
             (typ, env, Some (Completable.RecordField {seenFields})))
    | ( NRecordBody {seenFields},
        (Trecord {env; definition = `NameOnly _} as extractedType) ) ->
      Some (extractedType, env, Some (Completable.RecordField {seenFields}))
    | NRecordBody {seenFields}, TinlineRecord {env; fields} ->
      Some
        ( TinlineRecord {fields; env},
          env,
          Some (Completable.RecordField {seenFields}) )
    | ( NVariantPayload {constructorName = "Some"; itemNum = 0},
        Toption (env, ExtractedType typ) ) ->
      typ |> resolveNested ~env ~full ~nested
    | ( NVariantPayload {constructorName = "Some"; itemNum = 0},
        Toption (env, TypeExpr typ) ) ->
      typ
      |> extractType ~env ~package:full.package
      |> Utils.Option.flatMap (fun t -> t |> resolveNested ~env ~full ~nested)
    | ( NVariantPayload {constructorName; itemNum},
        Tvariant {env; constructors; typeParams; typeArgs} ) -> (
      match
        constructors
        |> List.find_opt (fun (c : Constructor.t) ->
               c.cname.txt = constructorName)
      with
      | Some {args = Args args} -> (
        match List.nth_opt args itemNum with
        | None -> None
        | Some (typ, _) ->
          typ
          |> instantiateType ~typeParams ~typeArgs
          |> extractType ~env ~package:full.package
          |> Utils.Option.flatMap (fun typ ->
                 typ |> resolveNested ~env ~full ~nested))
      | Some {args = InlineRecord fields} when itemNum = 0 ->
        TinlineRecord {env; fields} |> resolveNested ~env ~full ~nested
      | _ -> None)
    | ( NPolyvariantPayload {constructorName; itemNum},
        Tpolyvariant {env; constructors} ) -> (
      match
        constructors
        |> List.find_opt (fun (c : polyVariantConstructor) ->
               c.name = constructorName)
      with
      | None -> None
      | Some constructor -> (
        match List.nth_opt constructor.args itemNum with
        | None -> None
        | Some typ ->
          typ
          |> extractType ~env ~package:full.package
          |> Utils.Option.flatMap (fun typ ->
                 typ |> resolveNested ~env ~full ~nested)))
    | NArray, Tarray (env, ExtractedType typ) ->
      typ |> resolveNested ~env ~full ~nested
    | NArray, Tarray (env, TypeExpr typ) ->
      typ
      |> extractType ~env ~package:full.package
      |> Utils.Option.flatMap (fun typ ->
             typ |> resolveNested ~env ~full ~nested)
    | _ -> None)

let findTypeOfRecordField fields ~fieldName =
  match
    fields |> List.find_opt (fun (field : field) -> field.fname.txt = fieldName)
  with
  | None -> None
  | Some {typ; optional} ->
    let typ = if optional then Utils.unwrapIfOption typ else typ in
    Some typ

let findTypeOfConstructorArg constructors ~constructorName ~payloadNum ~env =
  match
    constructors
    |> List.find_opt (fun (c : Constructor.t) -> c.cname.txt = constructorName)
  with
  | Some {args = Args args} -> (
    match List.nth_opt args payloadNum with
    | None -> None
    | Some (typ, _) -> Some (TypeExpr typ))
  | Some {args = InlineRecord fields} when payloadNum = 0 ->
    Some (ExtractedType (TinlineRecord {env; fields}))
  | _ -> None

let findTypeOfPolyvariantArg constructors ~constructorName ~payloadNum =
  match
    constructors
    |> List.find_opt (fun (c : polyVariantConstructor) ->
           c.name = constructorName)
  with
  | Some {args} -> (
    match List.nth_opt args payloadNum with
    | None -> None
    | Some typ -> Some typ)
  | None -> None

let rec resolveNestedPatternPath (typ : innerType) ~env ~full ~nested =
  let t =
    match typ with
    | TypeExpr t -> t |> extractType ~env ~package:full.package
    | ExtractedType t -> Some t
  in
  match nested with
  | [] -> None
  | [finalPatternPath] -> (
    match t with
    | None -> None
    | Some completionType -> (
      match (finalPatternPath, completionType) with
      | ( Completable.NFollowRecordField {fieldName},
          (TinlineRecord {fields} | Trecord {fields}) ) -> (
        match fields |> findTypeOfRecordField ~fieldName with
        | None -> None
        | Some typ -> Some (TypeExpr typ, env))
      | NTupleItem {itemNum}, Tuple (env, tupleItems, _) -> (
        match List.nth_opt tupleItems itemNum with
        | None -> None
        | Some typ -> Some (TypeExpr typ, env))
      | NVariantPayload {constructorName; itemNum}, Tvariant {env; constructors}
        -> (
        match
          constructors
          |> findTypeOfConstructorArg ~constructorName ~payloadNum:itemNum ~env
        with
        | Some typ -> Some (typ, env)
        | None -> None)
      | ( NPolyvariantPayload {constructorName; itemNum},
          Tpolyvariant {env; constructors} ) -> (
        match
          constructors
          |> findTypeOfPolyvariantArg ~constructorName ~payloadNum:itemNum
        with
        | Some typ -> Some (TypeExpr typ, env)
        | None -> None)
      | ( NVariantPayload {constructorName = "Some"; itemNum = 0},
          Toption (env, typ) ) ->
        Some (typ, env)
      | NArray, Tarray (env, typ) -> Some (typ, env)
      | _ -> None))
  | patternPath :: nested -> (
    match t with
    | None -> None
    | Some completionType -> (
      match (patternPath, completionType) with
      | ( Completable.NFollowRecordField {fieldName},
          (TinlineRecord {env; fields} | Trecord {env; fields}) ) -> (
        match fields |> findTypeOfRecordField ~fieldName with
        | None -> None
        | Some typ ->
          typ
          |> extractType ~env ~package:full.package
          |> Utils.Option.flatMap (fun typ ->
                 ExtractedType typ
                 |> resolveNestedPatternPath ~env ~full ~nested))
      | NTupleItem {itemNum}, Tuple (env, tupleItems, _) -> (
        match List.nth_opt tupleItems itemNum with
        | None -> None
        | Some typ ->
          typ
          |> extractType ~env ~package:full.package
          |> Utils.Option.flatMap (fun typ ->
                 ExtractedType typ
                 |> resolveNestedPatternPath ~env ~full ~nested))
      | NVariantPayload {constructorName; itemNum}, Tvariant {env; constructors}
        -> (
        match
          constructors
          |> findTypeOfConstructorArg ~constructorName ~payloadNum:itemNum ~env
        with
        | Some typ -> typ |> resolveNestedPatternPath ~env ~full ~nested
        | None -> None)
      | ( NPolyvariantPayload {constructorName; itemNum},
          Tpolyvariant {env; constructors} ) -> (
        match
          constructors
          |> findTypeOfPolyvariantArg ~constructorName ~payloadNum:itemNum
        with
        | Some typ ->
          TypeExpr typ |> resolveNestedPatternPath ~env ~full ~nested
        | None -> None)
      | ( NVariantPayload {constructorName = "Some"; itemNum = 0},
          Toption (env, typ) ) ->
        typ |> resolveNestedPatternPath ~env ~full ~nested
      | NArray, Tarray (env, typ) ->
        typ |> resolveNestedPatternPath ~env ~full ~nested
      | _ -> None))

let getArgs ~env (t : Types.type_expr) ~full =
  let rec getArgsLoop ~env (t : Types.type_expr) ~full ~currentArgumentPosition
      =
    match t.desc with
    | Tlink t1
    | Tsubst t1
    | Tpoly (t1, [])
    | Tconstr (Pident {name = "function$"}, [t1; _], _) ->
      getArgsLoop ~full ~env ~currentArgumentPosition t1
    | Tarrow (Labelled l, tArg, tRet, _) ->
      (SharedTypes.Completable.Labelled l, tArg)
      :: getArgsLoop ~full ~env ~currentArgumentPosition tRet
    | Tarrow (Optional l, tArg, tRet, _) ->
      (Optional l, tArg) :: getArgsLoop ~full ~env ~currentArgumentPosition tRet
    | Tarrow (Nolabel, tArg, tRet, _) ->
      (Unlabelled {argumentPosition = currentArgumentPosition}, tArg)
      :: getArgsLoop ~full ~env
           ~currentArgumentPosition:(currentArgumentPosition + 1)
           tRet
    | Tconstr (path, typeArgs, _) -> (
      match References.digConstructor ~env ~package:full.package path with
      | Some
          ( env,
            {
              item = {decl = {type_manifest = Some t1; type_params = typeParams}};
            } ) ->
        let t1 = t1 |> instantiateType ~typeParams ~typeArgs in
        getArgsLoop ~full ~env ~currentArgumentPosition t1
      | _ -> [])
    | _ -> []
  in
  t |> getArgsLoop ~env ~full ~currentArgumentPosition:0

let typeIsUnit (typ : Types.type_expr) =
  match typ.desc with
  | Tconstr (Pident id, _typeArgs, _)
  | Tlink {desc = Tconstr (Pident id, _typeArgs, _)}
  | Tsubst {desc = Tconstr (Pident id, _typeArgs, _)}
  | Tpoly ({desc = Tconstr (Pident id, _typeArgs, _)}, [])
    when Ident.name id = "unit" ->
    true
  | _ -> false

let rec contextPathFromCoreType (coreType : Parsetree.core_type) =
  match coreType.ptyp_desc with
  | Ptyp_constr ({txt = Lident "option"}, [innerTyp]) ->
    innerTyp |> contextPathFromCoreType
    |> Option.map (fun innerTyp -> Completable.CPOption innerTyp)
  | Ptyp_constr ({txt = Lident "array"}, [innerTyp]) ->
    Some (Completable.CPArray (innerTyp |> contextPathFromCoreType))
  | Ptyp_constr (lid, _) ->
    Some (CPId (lid.txt |> Utils.flattenLongIdent, Type))
  | _ -> None

let printRecordFromFields ?name (fields : field list) =
  (match name with
  | None -> ""
  | Some name -> "type " ^ name ^ " = ")
  ^ "{"
  ^ (fields
    |> List.map (fun f -> f.fname.txt ^ ": " ^ Shared.typeToString f.typ)
    |> String.concat ", ")
  ^ "}"

let rec extractedTypeToString ?(inner = false) = function
  | Tuple (_, _, typ)
  | Tpolyvariant {typeExpr = typ}
  | Tfunction {typ}
  | Trecord {definition = `TypeExpr typ} ->
    if inner then
      match pathFromTypeExpr typ with
      | None -> "record" (* Won't happen *)
      | Some p -> p |> SharedTypes.pathIdentToString
    else Shared.typeToString typ
  | Tbool _ -> "bool"
  | Tstring _ -> "string"
  | Tarray (_, TypeExpr innerTyp) ->
    "array<" ^ Shared.typeToString innerTyp ^ ">"
  | Tarray (_, ExtractedType innerTyp) ->
    "array<" ^ extractedTypeToString ~inner:true innerTyp ^ ">"
  | Toption (_, TypeExpr innerTyp) ->
    "option<" ^ Shared.typeToString innerTyp ^ ">"
  | Toption (_, ExtractedType innerTyp) ->
    "option<" ^ extractedTypeToString ~inner:true innerTyp ^ ">"
  | Tpromise (_, innerTyp) -> "promise<" ^ Shared.typeToString innerTyp ^ ">"
  | Tvariant {variantDecl; variantName} ->
    if inner then variantName else Shared.declToString variantName variantDecl
  | Trecord {definition = `NameOnly name; fields} ->
    if inner then name else printRecordFromFields ~name fields
  | TinlineRecord {fields} -> printRecordFromFields fields
  | Texn _ -> "exn"

let unwrapCompletionTypeIfOption (t : SharedTypes.completionType) =
  match t with
  | Toption (_, ExtractedType unwrapped) -> unwrapped
  | _ -> t

module Codegen = struct
  let mkFailWithExp () =
    Ast_helper.Exp.apply
      (Ast_helper.Exp.ident {txt = Lident "failwith"; loc = Location.none})
      [(Nolabel, Ast_helper.Exp.constant (Pconst_string ("TODO", None)))]

  let mkConstructPat ?payload name =
    Ast_helper.Pat.construct
      {Asttypes.txt = Longident.Lident name; loc = Location.none}
      payload

  let mkTagPat ?payload name = Ast_helper.Pat.variant name payload

  let any () = Ast_helper.Pat.any ()

  let rec extractedTypeToExhaustivePatterns ~env ~full extractedType =
    match extractedType with
    | Tvariant v ->
      Some
        (v.constructors
        |> List.map (fun (c : SharedTypes.Constructor.t) ->
               mkConstructPat
                 ?payload:
                   (match c.args with
                   | Args [] -> None
                   | _ -> Some (any ()))
                 c.cname.txt))
    | Tpolyvariant v ->
      Some
        (v.constructors
        |> List.map (fun (c : SharedTypes.polyVariantConstructor) ->
               mkTagPat
                 ?payload:
                   (match c.args with
                   | [] -> None
                   | _ -> Some (any ()))
                 c.name))
    | Toption (_, innerType) ->
      let extractedType =
        match innerType with
        | ExtractedType t -> Some t
        | TypeExpr t -> extractType t ~env ~package:full.package
      in
      let expandedBranches =
        match extractedType with
        | None -> []
        | Some extractedType -> (
          match extractedTypeToExhaustivePatterns ~env ~full extractedType with
          | None -> []
          | Some patterns -> patterns)
      in
      Some
        ([
           mkConstructPat "None";
           mkConstructPat ~payload:(Ast_helper.Pat.any ()) "Some";
         ]
        @ (expandedBranches
          |> List.map (fun (pat : Parsetree.pattern) ->
                 mkConstructPat ~payload:pat "Some")))
    | Tbool _ -> Some [mkConstructPat "true"; mkConstructPat "false"]
    | _ -> None

  let extractedTypeToExhaustiveCases ~env ~full extractedType =
    let patterns = extractedTypeToExhaustivePatterns ~env ~full extractedType in

    match patterns with
    | None -> None
    | Some patterns ->
      Some
        (patterns
        |> List.map (fun (pat : Parsetree.pattern) ->
               Ast_helper.Exp.case pat (mkFailWithExp ())))
end
