type linkableType = {
  name: string;
  path: Path.t;
  env: SharedTypes.QueryEnv.t;
  loc: Location.t;
}

type fieldDoc = {fieldName: string; docstrings: string list; signature: string}

type constructorDoc = {
  constructorName: string;
  docstrings: string list;
  signature: string;
}

type docItemDetail =
  | Record of {fieldDocs: fieldDoc list}
  | Variant of {constructorDocs: constructorDoc list}
type docItem =
  | Value of {
      id: string;
      docstring: string list;
      signature: string;
      name: string;
      linkables: linkableType list;
          (** Relevant types to link to, found in relation to this value. *)
    }
  | Type of {
      id: string;
      docstring: string list;
      signature: string;
      name: string;
      detail: docItemDetail option;
          (** Additional documentation for constructors and record fields, if available. *)
      linkables: linkableType list;
          (** Relevant types to link to, found in relation to this type. *)
    }
  | Module of docsForModule
and docsForModule = {
  id: string;
  docstring: string list;
  name: string;
  items: docItem list;
}

let formatCode content =
  let {Res_driver.parsetree = signature; comments} =
    Res_driver.parseInterfaceFromSource ~forPrinter:true
      ~displayFilename:"<missing-file>" ~source:content
  in
  Res_printer.printInterface ~width:!Res_cli.ResClflags.width ~comments
    signature
  |> String.trim

module Linkables = struct
  (* TODO: Extend this by going into function arguments, tuples etc... *)
  let labelDeclarationsTypes lds =
    lds |> List.map (fun (ld : Types.label_declaration) -> ld.ld_type)

  let rec linkablesFromDecl (decl : Types.type_declaration) ~env ~full =
    match decl.type_kind with
    | Type_record (lds, _) -> (env, lds |> labelDeclarationsTypes)
    | Type_variant cds ->
      ( env,
        cds
        |> List.map (fun (cd : Types.constructor_declaration) ->
               let fromArgs =
                 match cd.cd_args with
                 | Cstr_tuple ts -> ts
                 | Cstr_record lds -> lds |> labelDeclarationsTypes
               in
               match cd.cd_res with
               | None -> fromArgs
               | Some t -> t :: fromArgs)
        |> List.flatten )
    | _ -> (
      match decl.type_manifest with
      | None -> (env, [])
      | Some typ -> linkablesFromTyp typ ~env ~full)

  and linkablesFromTyp ~env ~(full : SharedTypes.full) typ =
    match typ |> Shared.digConstructor with
    | Some path -> (
      match References.digConstructor ~env ~package:full.package path with
      | None -> (env, [typ])
      | Some (env1, {item = {decl}}) -> linkablesFromDecl decl ~env:env1 ~full)
    | None -> (env, [typ])

  type linkableSource =
    | Typ of SharedTypes.Type.t
    | TypeExpr of Types.type_expr

  let findLinkables ~env ~(full : SharedTypes.full) (typ : linkableSource) =
    (* Expand definitions of types mentioned in typ.
       If typ itself is a record or variant, search its body *)
    let envToSearch, typesToSearch =
      match typ with
      | Typ t -> linkablesFromDecl ~env t.decl ~full
      | TypeExpr t -> linkablesFromTyp t ~env ~full
    in
    let fromConstructorPath ~env path =
      match References.digConstructor ~env ~package:full.package path with
      | None -> None
      | Some (env, {name = {txt}; extentLoc}) ->
        if Utils.isUncurriedInternal path then None
        else Some {name = txt; env; loc = extentLoc; path}
    in
    let constructors = Shared.findTypeConstructors typesToSearch in
    constructors |> List.filter_map (fromConstructorPath ~env:envToSearch)
end

let stringifyDocstrings docstrings =
  let open Protocol in
  docstrings
  |> List.map (fun docstring ->
         docstring |> String.trim |> Json.escape |> wrapInQuotes)
  |> array

let stringifyLinkables ?(indentation = 0)
    ~(originalEnv : SharedTypes.QueryEnv.t) (linkables : linkableType list) =
  let open Protocol in
  linkables
  |> List.map (fun l ->
         let isExternal = originalEnv.file.uri <> l.env.file.uri in
         stringifyObject ~indentation:(indentation + 1)
           [
             ( "linkId",
               Some
                 ((if isExternal then "" else l.env.file.moduleName ^ ".")
                  ^ (l.path |> SharedTypes.pathIdentToString)
                 |> Json.escape |> wrapInQuotes) );
             ( "path",
               Some
                 (l.path |> SharedTypes.pathIdentToString |> Json.escape
                |> wrapInQuotes) );
             ("moduleName", Some (l.env.file.moduleName |> wrapInQuotes));
             ("external", Some (Printf.sprintf "%b" isExternal));
           ])
  |> array

let stringifyDetail ?(indentation = 0) (detail : docItemDetail) =
  let open Protocol in
  match detail with
  | Record {fieldDocs} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("kind", Some (wrapInQuotes "record"));
        ( "fieldDocs",
          Some
            (fieldDocs
            |> List.map (fun fieldDoc ->
                   stringifyObject ~indentation:(indentation + 1)
                     [
                       ("fieldName", Some (wrapInQuotes fieldDoc.fieldName));
                       ( "docstrings",
                         Some (stringifyDocstrings fieldDoc.docstrings) );
                       ("signature", Some (wrapInQuotes fieldDoc.signature));
                     ])
            |> array) );
      ]
  | Variant {constructorDocs} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("kind", Some (wrapInQuotes "variant"));
        ( "fieldDocs",
          Some
            (constructorDocs
            |> List.map (fun constructorDoc ->
                   stringifyObject ~startOnNewline:true
                     ~indentation:(indentation + 1)
                     [
                       ( "constructorName",
                         Some (wrapInQuotes constructorDoc.constructorName) );
                       ( "docstrings",
                         Some (stringifyDocstrings constructorDoc.docstrings) );
                       ( "signature",
                         Some (wrapInQuotes constructorDoc.signature) );
                     ])
            |> array) );
      ]

let rec stringifyDocItem ?(indentation = 0) ~originalEnv (item : docItem) =
  let open Protocol in
  match item with
  | Value {id; docstring; signature; name; linkables} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes id));
        ("kind", Some (wrapInQuotes "value"));
        ("name", Some (name |> Json.escape |> wrapInQuotes));
        ( "signature",
          Some (signature |> String.trim |> Json.escape |> wrapInQuotes) );
        ("docstrings", Some (stringifyDocstrings docstring));
        ( "linkables",
          Some
            (stringifyLinkables ~originalEnv ~indentation:(indentation + 1)
               linkables) );
      ]
  | Type {id; docstring; signature; name; detail; linkables} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes id));
        ("kind", Some (wrapInQuotes "type"));
        ("name", Some (name |> Json.escape |> wrapInQuotes));
        ("signature", Some (signature |> Json.escape |> wrapInQuotes));
        ("docstrings", Some (stringifyDocstrings docstring));
        ( "linkables",
          Some
            (stringifyLinkables ~originalEnv ~indentation:(indentation + 1)
               linkables) );
        ( "detail",
          match detail with
          | None -> None
          | Some detail ->
            Some (stringifyDetail ~indentation:(indentation + 1) detail) );
      ]
  | Module m ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes m.id));
        ("kind", Some (wrapInQuotes "module"));
        ( "item",
          Some
            (stringifyDocsForModule ~originalEnv ~indentation:(indentation + 1)
               m) );
      ]

and stringifyDocsForModule ?(indentation = 0) ~originalEnv (d : docsForModule) =
  let open Protocol in
  stringifyObject ~startOnNewline:true ~indentation
    [
      ("name", Some (wrapInQuotes d.name));
      ("docstrings", Some (stringifyDocstrings d.docstring));
      ( "items",
        Some
          (d.items
          |> List.map
               (stringifyDocItem ~originalEnv ~indentation:(indentation + 1))
          |> array) );
    ]

exception Invalid_file_type

let makeId modulePath ~identifier =
  identifier :: modulePath |> List.rev |> SharedTypes.ident

let extractDocs ~path ~debug =
  if debug then Printf.printf "extracting docs for %s\n" path;
  if
    FindFiles.isImplementation path = false
    && FindFiles.isInterface path = false
  then raise Invalid_file_type;
  let path =
    if FindFiles.isImplementation path then
      let pathAsResi =
        (path |> Filename.dirname) ^ "/"
        ^ (path |> Filename.basename |> Filename.chop_extension)
        ^ ".resi"
      in
      if Sys.file_exists pathAsResi then (
        if debug then
          Printf.printf "preferring found resi file for impl: %s\n" pathAsResi;
        pathAsResi)
      else path
    else path
  in
  match Cmt.loadFullCmtFromPath ~path with
  | None -> ()
  | Some full ->
    let file = full.file in
    let structure = file.structure in
    let open SharedTypes in
    let env = QueryEnv.fromFile file in
    let rec extractDocs ?(modulePath = [env.file.moduleName])
        (structure : Module.structure) =
      {
        id = modulePath |> ident;
        docstring = structure.docstring |> List.map String.trim;
        name = structure.name;
        items =
          structure.items
          |> List.filter_map (fun (item : Module.item) ->
                 match item.kind with
                 | Value typ ->
                   Some
                     (Value
                        {
                          id = modulePath |> makeId ~identifier:item.name;
                          docstring = item.docstring |> List.map String.trim;
                          signature =
                            "let " ^ item.name ^ ": " ^ Shared.typeToString typ
                            |> formatCode;
                          name = item.name;
                          linkables =
                            TypeExpr typ |> Linkables.findLinkables ~env ~full;
                        })
                 | Type (typ, _) ->
                   Some
                     (Type
                        {
                          id = modulePath |> makeId ~identifier:item.name;
                          linkables =
                            Typ typ |> Linkables.findLinkables ~env ~full;
                          docstring = item.docstring |> List.map String.trim;
                          signature =
                            typ.decl
                            |> Shared.declToString item.name
                            |> formatCode;
                          name = item.name;
                          detail =
                            (match
                               TypeUtils.extractTypeFromResolvedType ~env ~full
                                 typ
                             with
                            | Some (Trecord {fields}) ->
                              Some
                                (Record
                                   {
                                     fieldDocs =
                                       fields
                                       |> List.map (fun (field : field) ->
                                              {
                                                fieldName = field.fname.txt;
                                                docstrings = field.docstring;
                                                signature =
                                                  Shared.typeToString field.typ;
                                              });
                                   })
                            | Some (Tvariant {constructors}) ->
                              Some
                                (Variant
                                   {
                                     constructorDocs =
                                       constructors
                                       |> List.map (fun (c : Constructor.t) ->
                                              {
                                                constructorName = c.cname.txt;
                                                docstrings = c.docstring;
                                                signature =
                                                  CompletionBackEnd
                                                  .showConstructor c;
                                              });
                                   })
                            | _ -> None);
                        })
                 | Module (Structure m) ->
                   (* module Whatever = {} in res or module Whatever: {} in resi. *)
                   Some
                     (Module (extractDocs ~modulePath:(m.name :: modulePath) m))
                 | Module (Constraint (Structure _impl, Structure interface)) ->
                   (* module Whatever: { <interface> } = { <impl> }. Prefer the interface. *)
                   Some
                     (Module
                        (extractDocs
                           ~modulePath:(interface.name :: modulePath)
                           interface))
                 | _ -> None);
      }
    in
    let docs = extractDocs structure in
    print_endline (stringifyDocsForModule ~originalEnv:env docs)
