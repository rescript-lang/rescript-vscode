type fieldDoc = {
  fieldName: string;
  docstrings: string list;
  signature: string;
  optional: bool;
  deprecated: string option;
}

type constructorPayload = InlineRecord of {fieldDocs: fieldDoc list}

type constructorDoc = {
  constructorName: string;
  docstrings: string list;
  signature: string;
  deprecated: string option;
  items: constructorPayload option;
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
      deprecated: string option;
    }
  | Type of {
      id: string;
      docstring: string list;
      signature: string;
      name: string;
      deprecated: string option;
      detail: docItemDetail option;
          (** Additional documentation for constructors and record fields, if available. *)
    }
  | Module of docsForModule
  | ModuleAlias of {
      id: string;
      docstring: string list;
      name: string;
      items: docItem list;
    }
and docsForModule = {
  id: string;
  docstring: string list;
  deprecated: string option;
  name: string;
  items: docItem list;
}

let stringifyDocstrings docstrings =
  let open Protocol in
  docstrings
  |> List.map (fun docstring -> docstring |> String.trim |> wrapInQuotes)
  |> array

let stringifyFieldDoc ~indentation (fieldDoc : fieldDoc) =
  let open Protocol in
  stringifyObject ~indentation:(indentation + 1)
    [
      ("name", Some (wrapInQuotes fieldDoc.fieldName));
      ( "deprecated",
        match fieldDoc.deprecated with
        | Some d -> Some (wrapInQuotes d)
        | None -> None );
      ("optional", Some (string_of_bool fieldDoc.optional));
      ("docstrings", Some (stringifyDocstrings fieldDoc.docstrings));
      ("signature", Some (wrapInQuotes fieldDoc.signature));
    ]

let stringifyConstructorPayload ~indentation
    (constructorPayload : constructorPayload) =
  let open Protocol in
  match constructorPayload with
  | InlineRecord {fieldDocs} ->
    stringifyObject ~indentation:(indentation + 1)
      [
        ("kind", Some (wrapInQuotes "inlineRecord"));
        ( "fields",
          Some
            (fieldDocs
            |> List.map (stringifyFieldDoc ~indentation:(indentation + 1))
            |> array) );
      ]

let stringifyDetail ?(indentation = 0) (detail : docItemDetail) =
  let open Protocol in
  match detail with
  | Record {fieldDocs} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("kind", Some (wrapInQuotes "record"));
        ( "items",
          Some (fieldDocs |> List.map (stringifyFieldDoc ~indentation) |> array)
        );
      ]
  | Variant {constructorDocs} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("kind", Some (wrapInQuotes "variant"));
        ( "items",
          Some
            (constructorDocs
            |> List.map (fun constructorDoc ->
                   stringifyObject ~startOnNewline:true
                     ~indentation:(indentation + 1)
                     [
                       ( "name",
                         Some (wrapInQuotes constructorDoc.constructorName) );
                       ( "deprecated",
                         match constructorDoc.deprecated with
                         | Some d -> Some (wrapInQuotes d)
                         | None -> None );
                       ( "docstrings",
                         Some (stringifyDocstrings constructorDoc.docstrings) );
                       ( "signature",
                         Some (wrapInQuotes constructorDoc.signature) );
                       ( "payload",
                         match constructorDoc.items with
                         | None -> None
                         | Some constructorPayload ->
                           Some
                             (stringifyConstructorPayload
                                ~indentation:(indentation + 1)
                                constructorPayload) );
                     ])
            |> array) );
      ]

let rec stringifyDocItem ?(indentation = 0) ~originalEnv (item : docItem) =
  let open Protocol in
  match item with
  | Value {id; docstring; signature; name; deprecated} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes id));
        ("kind", Some (wrapInQuotes "value"));
        ("name", Some (name |> wrapInQuotes));
        ( "deprecated",
          match deprecated with
          | Some d -> Some (wrapInQuotes d)
          | None -> None );
        ( "signature",
          Some (signature |> String.trim |> wrapInQuotes) );
        ("docstrings", Some (stringifyDocstrings docstring));
      ]
  | Type {id; docstring; signature; name; deprecated; detail} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes id));
        ("kind", Some (wrapInQuotes "type"));
        ("name", Some (name |> wrapInQuotes));
        ( "deprecated",
          match deprecated with
          | Some d -> Some (wrapInQuotes d)
          | None -> None );
        ("signature", Some (signature |> wrapInQuotes));
        ("docstrings", Some (stringifyDocstrings docstring));
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
        ("name", Some (wrapInQuotes m.name));
        ("kind", Some (wrapInQuotes "module"));
        ("docstrings", Some (stringifyDocstrings m.docstring));
        ( "items",
          Some
            (m.items
            |> List.map
                 (stringifyDocItem ~originalEnv ~indentation:(indentation + 1))
            |> array) );
      ]
  | ModuleAlias m ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes m.id));
        ("kind", Some (wrapInQuotes "moduleAlias"));
        ("name", Some (wrapInQuotes m.name));
        ("docstrings", Some (stringifyDocstrings m.docstring));
        ( "items",
          Some
            (m.items
            |> List.map
                 (stringifyDocItem ~originalEnv ~indentation:(indentation + 1))
            |> array) );
      ]

and stringifyDocsForModule ?(indentation = 0) ~originalEnv (d : docsForModule) =
  let open Protocol in
  stringifyObject ~startOnNewline:true ~indentation
    [
      ("name", Some (wrapInQuotes d.name));
      ( "deprecated",
        match d.deprecated with
        | Some d -> Some (wrapInQuotes d)
        | None -> None );
      ("docstrings", Some (stringifyDocstrings d.docstring));
      ( "items",
        Some
          (d.items
          |> List.map
               (stringifyDocItem ~originalEnv ~indentation:(indentation + 1))
          |> array) );
    ]

let fieldToFieldDoc (field : SharedTypes.field) : fieldDoc =
  {
    fieldName = field.fname.txt;
    docstrings = field.docstring;
    optional = field.optional;
    signature = Shared.typeToString field.typ;
    deprecated = field.deprecated;
  }

let typeDetail typ ~env ~full =
  let open SharedTypes in
  match TypeUtils.extractTypeFromResolvedType ~env ~full typ with
  | Some (Trecord {fields}) ->
    Some (Record {fieldDocs = fields |> List.map fieldToFieldDoc})
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
                      signature = CompletionBackEnd.showConstructor c;
                      deprecated = c.deprecated;
                      items =
                        (match c.args with
                        | InlineRecord fields ->
                          Some
                            (InlineRecord
                               {fieldDocs = fields |> List.map fieldToFieldDoc})
                        | _ -> None);
                    });
         })
  | _ -> None

let makeId modulePath ~identifier =
  identifier :: modulePath |> List.rev |> SharedTypes.ident

let extractDocs ~path ~debug =
  if debug then Printf.printf "extracting docs for %s\n" path;
  if
    FindFiles.isImplementation path = false
    && FindFiles.isInterface path = false
  then (
    Printf.eprintf "error: failed to read %s, expected an .res or .resi file\n"
      path;
    exit 1);
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
  | None ->
    Printf.eprintf
      "error: failed to generate doc for %s, try to build the project\n" path;
    exit 1
  | Some full ->
    let file = full.file in
    let structure = file.structure in
    let open SharedTypes in
    let env = QueryEnv.fromFile file in
    let rec extractDocsForModule ?(modulePath = [env.file.moduleName])
        (structure : Module.structure) =
      {
        id = modulePath |> List.rev |> ident;
        docstring = structure.docstring |> List.map String.trim;
        name = structure.name;
        deprecated = structure.deprecated;
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
                            "let " ^ item.name ^ ": " ^ Shared.typeToString typ;
                          name = item.name;
                          deprecated = item.deprecated;
                        })
                 | Type (typ, _) ->
                   Some
                     (Type
                        {
                          id = modulePath |> makeId ~identifier:item.name;
                          docstring = item.docstring |> List.map String.trim;
                          signature = typ.decl |> Shared.declToString item.name;
                          name = item.name;
                          deprecated = item.deprecated;
                          detail = typeDetail typ ~full ~env;
                        })
                 | Module (Ident p) ->
                   (* module Whatever = OtherModule *)
                   let aliasToModule = p |> pathIdentToString in
                   let id =
                     (modulePath |> List.rev |> List.hd) ^ "." ^ item.name
                   in
                   let items, internalDocstrings =
                     match
                       ProcessCmt.fileForModule ~package:full.package
                         aliasToModule
                     with
                     | None -> ([], [])
                     | Some file ->
                       let docs =
                         extractDocsForModule ~modulePath:[id] file.structure
                       in
                       (docs.items, docs.docstring)
                   in
                   Some
                     (ModuleAlias
                        {
                          id;
                          name = item.name;
                          items;
                          docstring =
                            item.docstring @ internalDocstrings
                            |> List.map String.trim;
                        })
                 | Module (Structure m) ->
                   (* module Whatever = {} in res or module Whatever: {} in resi. *)
                   Some
                     (Module
                        (extractDocsForModule ~modulePath:(m.name :: modulePath)
                           m))
                 | Module (Constraint (Structure _impl, Structure interface)) ->
                   (* module Whatever: { <interface> } = { <impl> }. Prefer the interface. *)
                   Some
                     (Module
                        (extractDocsForModule
                           ~modulePath:(interface.name :: modulePath)
                           interface))
                 | _ -> None);
      }
    in
    let docs = extractDocsForModule structure in
    print_endline (stringifyDocsForModule ~originalEnv:env docs)
