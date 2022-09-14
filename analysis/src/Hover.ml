open SharedTypes

let codeBlock code = Printf.sprintf "```rescript\n%s\n```" code

let encodeURIComponent text =
  let ln = String.length text in
  let buf = Buffer.create ln in
  let rec loop i =
    if i < ln then (
      (match text.[i] with
      | '"' -> Buffer.add_string buf "%22"
      | ':' -> Buffer.add_string buf "%3A"
      | '/' -> Buffer.add_string buf "%2F"
      | '\\' -> Buffer.add_string buf "%5C"
      | ',' -> Buffer.add_string buf "%2C"
      | '&' -> Buffer.add_string buf "%26"
      | '[' -> Buffer.add_string buf "%5B"
      | ']' -> Buffer.add_string buf "%5D"
      | c -> Buffer.add_char buf c);
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

type link = {startPos: Protocol.position; file: string; label: string}

let linkToCommandArgs link =
  Printf.sprintf "[\"%s\",%i,%i]" link.file link.startPos.line
    link.startPos.character

let makeGotoCommand link =
  Printf.sprintf "[%s](command:rescript-vscode.go_to_location?%s)" link.label
    (encodeURIComponent (linkToCommandArgs link))

let showModuleTopLevel ~docstring ~name (topLevel : Module.item list) =
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
  let full = codeBlock ("module " ^ name ^ " = {" ^ "\n" ^ contents ^ "\n}") in
  let doc =
    match docstring with
    | [] -> ""
    | _ :: _ -> "\n" ^ (docstring |> String.concat "\n") ^ "\n"
  in
  Some (doc ^ full)

let rec showModule ~docstring ~(file : File.t) ~name
    (declared : Module.t Declared.t option) =
  match declared with
  | None -> showModuleTopLevel ~docstring ~name file.structure.items
  | Some {item = Structure {items}} -> showModuleTopLevel ~docstring ~name items
  | Some ({item = Constraint (_moduleItem, moduleTypeItem)} as declared) ->
    (* show the interface *)
    showModule ~docstring ~file ~name
      (Some {declared with item = moduleTypeItem})
  | Some {item = Ident path} ->
    Some ("Unable to resolve module reference " ^ Path.name path)

let newHover ~full:{file; package} locItem =
  match locItem.locType with
  | TypeDefinition (name, decl, _stamp) ->
    let typeDef = Shared.declToString name decl in
    Some (codeBlock typeDef)
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
      match ResolvePath.resolvePath ~env ~path ~package with
      | None -> None
      | Some (env, name) -> (
        match References.exportedForTip ~env name tip with
        | None -> None
        | Some stamp -> (
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
              showModule ~docstring ~name ~file declared)))))
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
      (codeBlock
         (match t with
         | Const_int _ -> "int"
         | Const_char _ -> "char"
         | Const_string _ -> "string"
         | Const_float _ -> "float"
         | Const_int32 _ -> "int32"
         | Const_int64 _ -> "int64"
         | Const_nativeint _ -> "int"))
  | Typed (_, t, locKind) ->
    let fromConstructorPath ~env path =
      match References.digConstructor ~env ~package path with
      | None -> None
      | Some (env, {extentLoc; item = {decl}}) ->
        if Utils.isUncurriedInternal path then None
        else
          Some
            ( decl
              |> Shared.declToString ~printNameAsIs:true
                   (SharedTypes.pathIdentToString path),
              extentLoc,
              env )
    in
    let fromType ~docstring typ =
      let typeString = codeBlock (typ |> Shared.typeToString) in
      let typeDefinitions =
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
              | Type_record (lds, _) ->
                (env1, typ :: (lds |> labelDeclarationsTypes))
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
        let constructors = Shared.findTypeConstructors typesToSearch in
        constructors
        |> List.filter_map (fun constructorPath ->
               match
                 constructorPath |> fromConstructorPath ~env:envToSearch
               with
               | None -> None
               | Some (typString, extentLoc, env) ->
                 let startLine, startCol = Pos.ofLexing extentLoc.loc_start in
                 Some
                   (Shared.markdownSpacing ^ codeBlock typString ^ "\n"
                  ^ "Go to: "
                   ^ makeGotoCommand
                       {
                         label = "Type definition";
                         file = Uri.toString env.file.uri;
                         startPos = {line = startLine; character = startCol};
                       }
                   ^ "\n\n---\n"))
      in
      let typeString = typeString :: typeDefinitions |> String.concat "\n\n" in
      (typeString, docstring)
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
          typeString :: codeBlock (txt ^ argsString) :: docstring
        | `Field ->
          let typeString, docstring = t |> fromType ~docstring in
          typeString :: docstring)
    in
    Some (String.concat "\n\n" parts)
