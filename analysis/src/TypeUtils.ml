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

let extractFunctionType ~env ~package typ =
  let rec loop ~env acc (t : Types.type_expr) =
    match t.desc with
    | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> loop ~env acc t1
    | Tarrow (label, tArg, tRet, _) -> loop ~env ((label, tArg) :: acc) tRet
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
    Some (Completable.Toption (env, payloadTypeExpr))
  | Tconstr (Path.Pident {name = "array"}, [payloadTypeExpr], _) ->
    Some (Tarray (env, payloadTypeExpr))
  | Tconstr (Path.Pident {name = "bool"}, [], _) -> Some (Tbool env)
  | Tconstr (Path.Pident {name = "string"}, [], _) -> Some (Tstring env)
  | Tconstr (path, _, _) -> (
    match References.digConstructor ~env ~package path with
    | Some (env, {item = {decl = {type_manifest = Some t1}}}) ->
      extractType ~env ~package t1
    | Some (env, {name; item = {decl; kind = Type.Variant constructors}}) ->
      Some
        (Tvariant
           {env; constructors; variantName = name.txt; variantDecl = decl})
    | Some (env, {item = {kind = Record fields}}) ->
      Some (Trecord {env; fields; typeExpr = t})
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
  | _ -> None

let rec resolveTypeForPipeCompletion ~env ~package (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
    resolveTypeForPipeCompletion ~env ~package t1
  (* Don't descend into types named "t". Type t is a convention in the ReScript ecosystem. *)
  | Tconstr (path, _, _) when path |> Path.last = "t" -> (env, t)
  | Tconstr (path, _, _) -> (
    match References.digConstructor ~env ~package path with
    | Some (env, {item = {decl = {type_manifest = Some typ}}}) ->
      resolveTypeForPipeCompletion ~env ~package typ
    | _ -> (env, t))
  | _ -> (env, t)

(** This moves through a nested path via a set of instructions, trying to resolve the type at the end of the path. *)
let rec resolveNested (typ : completionType) ~env ~package ~nested =
  match nested with
  | [] -> Some (typ, env, None)
  | patternPath :: nested -> (
    let extractedType =
      match typ with
      | TypeExpr typ -> typ |> extractType ~env ~package
      | InlineRecord fields -> Some (TinlineRecord {env; fields})
    in
    match (patternPath, extractedType) with
    | Completable.NTupleItem {itemNum}, Some (Tuple (env, tupleItems, _)) -> (
      match List.nth_opt tupleItems itemNum with
      | None -> None
      | Some typ -> TypeExpr typ |> resolveNested ~env ~package ~nested)
    | ( NFollowRecordField {fieldName},
        Some (TinlineRecord {env; fields} | Trecord {env; fields}) ) -> (
      match
        fields
        |> List.find_opt (fun (field : field) -> field.fname.txt = fieldName)
      with
      | None -> None
      | Some {typ; optional} ->
        let typ = if optional then Utils.unwrapIfOption typ else typ in
        TypeExpr typ |> resolveNested ~env ~package ~nested)
    | NRecordBody {seenFields}, Some (Trecord {env; typeExpr}) ->
      Some (TypeExpr typeExpr, env, Some (Completable.RecordField {seenFields}))
    | NRecordBody {seenFields}, Some (TinlineRecord {env; fields}) ->
      Some
        (InlineRecord fields, env, Some (Completable.RecordField {seenFields}))
    | ( NVariantPayload {constructorName = "Some"; itemNum = 0},
        Some (Toption (env, typ)) ) ->
      TypeExpr typ |> resolveNested ~env ~package ~nested
    | ( NVariantPayload {constructorName; itemNum},
        Some (Tvariant {env; constructors}) ) -> (
      match
        constructors
        |> List.find_opt (fun (c : Constructor.t) ->
               c.cname.txt = constructorName)
      with
      | Some {args = Args args} -> (
        match List.nth_opt args itemNum with
        | None -> None
        | Some (typ, _) -> TypeExpr typ |> resolveNested ~env ~package ~nested)
      | Some {args = InlineRecord fields} when itemNum = 0 ->
        InlineRecord fields |> resolveNested ~env ~package ~nested
      | _ -> None)
    | ( NPolyvariantPayload {constructorName; itemNum},
        Some (Tpolyvariant {env; constructors}) ) -> (
      match
        constructors
        |> List.find_opt (fun (c : polyVariantConstructor) ->
               c.name = constructorName)
      with
      | None -> None
      | Some constructor -> (
        match List.nth_opt constructor.args itemNum with
        | None -> None
        | Some typ -> TypeExpr typ |> resolveNested ~env ~package ~nested))
    | NArray, Some (Tarray (env, typ)) ->
      TypeExpr typ |> resolveNested ~env ~package ~nested
    | _ -> None)

let findReturnTypeOfFunctionAtLoc loc ~(env : QueryEnv.t) ~full ~debug =
  match References.getLocItem ~full ~pos:(loc |> Loc.end_) ~debug with
  | Some {locType = Typed (_, typExpr, _)} -> (
    match extractFunctionType ~env ~package:full.package typExpr with
    | args, tRet when args <> [] -> Some tRet
    | _ -> None)
  | _ -> None

let getArgs ~env (t : Types.type_expr) ~full =
  let rec getArgsLoop ~env (t : Types.type_expr) ~full ~currentArgumentPosition
      =
    match t.desc with
    | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
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
