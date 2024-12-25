let filterRecordFields ~env ~recordAsString ~prefix ~exact fields =
  fields
  |> Utils.filterMap (fun (field : SharedTypes.field) ->
         if Utils.checkName field.fname.txt ~prefix ~exact then
           Some
             (SharedTypes.Completion.create field.fname.txt ~env
                ?deprecated:field.deprecated ~docstring:field.docstring
                ~kind:(SharedTypes.Completion.Field (field, recordAsString)))
         else None)

let fieldCompletionsForDotCompletion typ ~env ~package ~prefix ~fieldNameLoc
    ~exact =
  let asObject = typ |> TypeUtils.extractObjectType ~env ~package in
  match asObject with
  | Some (objEnv, obj) ->
    (* Handle obj completion via dot *)
    if Debug.verbose () then
      Printf.printf "[dot_completion]--> Obj type found:\n";
    obj |> TypeUtils.getObjFields
    |> Utils.filterMap (fun (field, _typ) ->
           if Utils.checkName field ~prefix ~exact then
             let fullObjFieldName = Printf.sprintf "[\"%s\"]" field in
             Some
               (SharedTypes.Completion.create fullObjFieldName ~synthetic:true
                  ~range:fieldNameLoc ~insertText:fullObjFieldName ~env:objEnv
                  ~kind:(SharedTypes.Completion.ObjLabel typ))
           else None)
  | None -> (
    match typ |> TypeUtils.extractRecordType ~env ~package with
    | Some (env, fields, typDecl, _path, _attributes) ->
      fields
      |> filterRecordFields ~env ~prefix ~exact
           ~recordAsString:
             (typDecl.item.decl |> Shared.declToString typDecl.name.txt)
    | None -> [])
