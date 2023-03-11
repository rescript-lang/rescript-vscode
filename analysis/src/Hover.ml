open SharedTypes

let showModuleTopLevel ~docstring ~isType ~name (topLevel : Module.item list) =
  let contents =
    topLevel
    |> List.map (fun item ->
           match item.Module.kind with
           (* TODO pretty print module contents *)
           | Type ({decl}, recStatus) ->
             "  " ^ (decl |> Shared.declToString ~recStatus item.name)
           | Module _ -> "  module " ^ item.name
           | Value typ ->
             "  let " ^ item.name ^ ": " ^ (typ |> Shared.typeToString))
    (* TODO indent *)
    |> String.concat "\n"
  in
  let full =
    Markdown.codeBlock
      ("module "
      ^ (if isType then "type " ^ name ^ " = " else name ^ ": ")
      ^ "{" ^ "\n" ^ contents ^ "\n}")
  in
  let doc =
    match docstring with
    | [] -> ""
    | _ :: _ -> "\n" ^ (docstring |> String.concat "\n") ^ "\n"
  in
  Some (doc ^ full)

let rec showModule ~docstring ~(file : File.t) ~name
    (declared : Module.t Declared.t option) =
  match declared with
  | None ->
    showModuleTopLevel ~docstring ~isType:false ~name file.structure.items
  | Some {item = Structure {items}; modulePath} ->
    let isType =
      match modulePath with
      | ExportedModule {isType} -> isType
      | _ -> false
    in
    showModuleTopLevel ~docstring ~isType ~name items
  | Some ({item = Constraint (_moduleItem, moduleTypeItem)} as declared) ->
    (* show the interface *)
    showModule ~docstring ~file ~name
      (Some {declared with item = moduleTypeItem})
  | Some {item = Ident path} ->
    Some ("Unable to resolve module reference " ^ Path.name path)

type extractedType = {
  name: string;
  path: Path.t;
  decl: Types.type_declaration;
  env: SharedTypes.QueryEnv.t;
  loc: Warnings.loc;
}

let findRelevantTypesFromType ~file ~package typ =
  (* Expand definitions of types mentioned in typ.
     If typ itself is a record or variant, search its body *)
  let env = QueryEnv.fromFile file in
  let envToSearch, typesToSearch =
    match typ |> Shared.digConstructor with
    | Some path -> (
      let labelDeclarationsTypes lds =
        lds |> List.map (fun (ld : Types.label_declaration) -> ld.ld_type)
      in
      match References.digConstructor ~env ~package path with
      | None -> (env, [typ])
      | Some (env1, {item = {decl}}) -> (
        match decl.type_kind with
        | Type_record (lds, _) -> (env1, typ :: (lds |> labelDeclarationsTypes))
        | Type_variant cds ->
          ( env1,
            cds
            |> List.map (fun (cd : Types.constructor_declaration) ->
                   let fromArgs =
                     match cd.cd_args with
                     | Cstr_tuple ts -> ts
                     | Cstr_record lds -> lds |> labelDeclarationsTypes
                   in
                   typ
                   ::
                   (match cd.cd_res with
                   | None -> fromArgs
                   | Some t -> t :: fromArgs))
            |> List.flatten )
        | _ -> (env, [typ])))
    | None -> (env, [typ])
  in
  let fromConstructorPath ~env path =
    match References.digConstructor ~env ~package path with
    | None -> None
    | Some (env, {name = {txt}; extentLoc; item = {decl}}) ->
      if Utils.isUncurriedInternal path then None
      else Some {name = txt; env; loc = extentLoc; decl; path}
  in
  let constructors = Shared.findTypeConstructors typesToSearch in
  constructors |> List.filter_map (fromConstructorPath ~env:envToSearch)

(* Produces a hover with relevant types expanded in the main type being hovered. *)
let hoverWithExpandedTypes ~file ~package ~supportsMarkdownLinks typ =
  let typeString = Markdown.codeBlock (typ |> Shared.typeToString) in
  let types = findRelevantTypesFromType typ ~file ~package in
  let typeDefinitions =
    types
    |> List.map (fun {decl; env; loc; path} ->
           let linkToTypeDefinitionStr =
             if supportsMarkdownLinks then
               Markdown.goToDefinitionText ~env ~pos:loc.Warnings.loc_start
             else ""
           in
           Markdown.divider
           ^ (if supportsMarkdownLinks then Markdown.spacing else "")
           ^ Markdown.codeBlock
               (decl
               |> Shared.declToString ~printNameAsIs:true
                    (SharedTypes.pathIdentToString path))
           ^ linkToTypeDefinitionStr ^ "\n")
  in
  typeString :: typeDefinitions |> String.concat "\n"

(* Leverages autocomplete functionality to produce a hover for a position. This
   makes it (most often) work with unsaved content. *)
let getHoverViaCompletions ~debug ~path ~pos ~currentFile ~forHover
    ~supportsMarkdownLinks =
  match Completions.getCompletions ~debug ~path ~pos ~currentFile ~forHover with
  | None -> None
  | Some (completions, ({file; package} as full), scope) -> (
    let rawOpens = Scope.getRawOpens scope in
    let allFiles =
      FileSet.union package.projectFiles package.dependenciesFiles
    in
    match completions with
    | {kind = Label typString; docstring} :: _ ->
      let parts =
        (if typString = "" then [] else [Markdown.codeBlock typString])
        @ docstring
      in
      Some (Protocol.stringifyHover (String.concat "\n\n" parts))
    | {kind = Field _; env; docstring} :: _ -> (
      let opens = CompletionBackEnd.getOpens ~debug ~rawOpens ~package ~env in
      match
        CompletionBackEnd.completionsGetTypeEnv2 ~debug ~full ~rawOpens ~opens
          ~allFiles ~pos ~scope completions
      with
      | Some (typ, _env) ->
        let typeString =
          hoverWithExpandedTypes ~file ~package ~supportsMarkdownLinks typ
        in
        let parts = typeString :: docstring in
        Some (Protocol.stringifyHover (String.concat "\n\n" parts))
      | None -> None)
    | {env} :: _ -> (
      let opens = CompletionBackEnd.getOpens ~debug ~rawOpens ~package ~env in
      match
        CompletionBackEnd.completionsGetTypeEnv2 ~debug ~full ~rawOpens ~opens
          ~allFiles ~pos ~scope completions
      with
      | Some (typ, _env) ->
        let typeString =
          hoverWithExpandedTypes ~file ~package ~supportsMarkdownLinks typ
        in
        Some (Protocol.stringifyHover typeString)
      | None -> None)
    | _ -> None)

let newHover ~full:{file; package} ~supportsMarkdownLinks locItem =
  match locItem.locType with
  | TypeDefinition (name, decl, _stamp) ->
    let typeDef = Shared.declToString name decl in
    Some (Markdown.codeBlock typeDef)
  | LModule (Definition (stamp, _tip)) | LModule (LocalReference (stamp, _tip))
    -> (
    match Stamps.findModule file.stamps stamp with
    | None -> None
    | Some md -> (
      match References.resolveModuleReference ~file ~package md with
      | None -> None
      | Some (file, declared) ->
        let name, docstring =
          match declared with
          | Some d -> (d.name.txt, d.docstring)
          | None -> (file.moduleName, file.structure.docstring)
        in
        showModule ~docstring ~name ~file declared))
  | LModule (GlobalReference (moduleName, path, tip)) -> (
    match ProcessCmt.fileForModule ~package moduleName with
    | None -> None
    | Some file -> (
      let env = QueryEnv.fromFile file in
      match References.exportedForTip ~env ~path ~package ~tip with
      | None -> None
      | Some (_env, _name, stamp) -> (
        match Stamps.findModule file.stamps stamp with
        | None -> None
        | Some md -> (
          match References.resolveModuleReference ~file ~package md with
          | None -> None
          | Some (file, declared) ->
            let name, docstring =
              match declared with
              | Some d -> (d.name.txt, d.docstring)
              | None -> (file.moduleName, file.structure.docstring)
            in
            showModule ~docstring ~name ~file declared))))
  | LModule NotFound -> None
  | TopLevelModule name -> (
    match ProcessCmt.fileForModule ~package name with
    | None -> None
    | Some file ->
      showModule ~docstring:file.structure.docstring ~name:file.moduleName ~file
        None)
  | Typed (_, _, Definition (_, (Field _ | Constructor _))) -> None
  | Constant t ->
    Some
      (Markdown.codeBlock
         (match t with
         | Const_int _ -> "int"
         | Const_char _ -> "char"
         | Const_string _ -> "string"
         | Const_float _ -> "float"
         | Const_int32 _ -> "int32"
         | Const_int64 _ -> "int64"
         | Const_nativeint _ -> "int"))
  | Typed (_, t, locKind) ->
    let fromType ~docstring typ =
      ( hoverWithExpandedTypes ~file ~package ~supportsMarkdownLinks typ,
        docstring )
    in
    let parts =
      match References.definedForLoc ~file ~package locKind with
      | None ->
        let typeString, docstring = t |> fromType ~docstring:[] in
        typeString :: docstring
      | Some (docstring, res) -> (
        match res with
        | `Declared ->
          let typeString, docstring = t |> fromType ~docstring in
          typeString :: docstring
        | `Constructor {cname = {txt}; args; docstring} ->
          let typeString, docstring = t |> fromType ~docstring in
          let argsString =
            match args with
            | InlineRecord _ | Args [] -> ""
            | Args args ->
              args
              |> List.map (fun (t, _) -> Shared.typeToString t)
              |> String.concat ", " |> Printf.sprintf "(%s)"
          in
          (Markdown.codeBlock (txt ^ argsString) :: docstring) @ [typeString]
        | `Field ->
          let typeString, docstring = t |> fromType ~docstring in
          typeString :: docstring)
    in
    Some (String.concat "\n\n" parts)
