type fieldDoc = {
  fieldName: string;
  docstrings: string list;
  signature: string;
  optional: bool;
  deprecated: string option;
}

type constructorDoc = {
  constructorName: string;
  docstrings: string list;
  signature: string;
  deprecated: string option;
}

type docsForModuleAlias = {
  id: string;
  docstring: string list;
  name: string;
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
      loc: Warnings.loc;
      deprecated: string option;
    }
  | Type of {
      id: string;
      docstring: string list;
      signature: string;
      name: string;
      loc: Warnings.loc;
      deprecated: string option;
      detail: docItemDetail option;
          (** Additional documentation for constructors and record fields, if available. *)
    }
  | Module of docsForModule
  | ModuleAlias of docsForModuleAlias
and docsForModule = {
  id: string;
  docstring: string list;
  deprecated: string option;
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

let stringifyDocstrings docstrings =
  let open Protocol in
  docstrings
  |> List.map (fun docstring -> docstring |> String.trim |> wrapInQuotes)
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
                       ( "deprecated",
                         match fieldDoc.deprecated with
                         | Some d -> Some (wrapInQuotes d)
                         | None -> None );
                       ("optional", Some (string_of_bool fieldDoc.optional));
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
        ( "constructorDocs",
          Some
            (constructorDocs
            |> List.map (fun constructorDoc ->
                   stringifyObject ~startOnNewline:true
                     ~indentation:(indentation + 1)
                     [
                       ( "constructorName",
                         Some (wrapInQuotes constructorDoc.constructorName) );
                       ( "deprecated",
                         match constructorDoc.deprecated with
                         | Some d -> Some (wrapInQuotes d)
                         | None -> None );
                       ( "docstrings",
                         Some (stringifyDocstrings constructorDoc.docstrings) );
                       ( "signature",
                         Some (wrapInQuotes constructorDoc.signature) );
                     ])
            |> array) );
      ]

let stringifyLoc loc ~indentation =
  let open Protocol in
  let line, col = Loc.start loc in
  let path = loc.loc_start.pos_fname in
  stringifyObject ~startOnNewline:false ~indentation:(indentation + 1)
    [
      ("path", Some (wrapInQuotes path));
      ("line", Some (string_of_int (line + 1)));
      ("col", Some (string_of_int (col + 1)));
    ]

let rec stringifyDocItem ?(indentation = 0) ~originalEnv (item : docItem) =
  let open Protocol in
  match item with
  | Value {id; docstring; signature; name; deprecated} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes id));
        ("kind", Some (wrapInQuotes "value"));
        ("name", Some (name |> Json.escape |> wrapInQuotes));
        ( "deprecated",
          match deprecated with
          | Some d -> Some (wrapInQuotes d)
          | None -> None );
        (* ("location", Some (stringifyLoc loc ~indentation)); *)
        ( "signature",
          Some (signature |> String.trim |> Json.escape |> wrapInQuotes) );
        ("docstrings", Some (stringifyDocstrings docstring));
      ]
  | Type {id; docstring; signature; name; deprecated; detail} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes id));
        ("kind", Some (wrapInQuotes "type"));
        ("name", Some (name |> Json.escape |> wrapInQuotes));
        ( "deprecated",
          match deprecated with
          | Some d -> Some (wrapInQuotes d)
          | None -> None );
        (* ("location", Some (stringifyLoc loc ~indentation)); *)
        ("signature", Some (signature |> Json.escape |> wrapInQuotes));
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
        ("kind", Some (wrapInQuotes "module"));
        ( "item",
          Some
            (stringifyDocsForModule ~originalEnv ~indentation:(indentation + 1)
               m) );
      ]
  | ModuleAlias m ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes m.id));
        ("kind", Some (wrapInQuotes "moduleAlias"));
        ("docstrings", Some (stringifyDocstrings m.docstring));
        ("signature", Some (m.signature |> Json.escape |> wrapInQuotes));
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

let typeDetail typ ~env ~full =
  let open SharedTypes in
  match TypeUtils.extractTypeFromResolvedType ~env ~full typ with
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
                      optional = field.optional;
                      signature = Shared.typeToString field.typ;
                      deprecated = field.deprecated;
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
                      signature = CompletionBackEnd.showConstructor c;
                      deprecated = c.deprecated;
                    });
         })
  | _ -> None

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
    let rec extractDocsForModule ?(modulePath = [env.file.moduleName])
        (structure : Module.structure) =
      {
        id = modulePath |> ident;
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
                            "let " ^ item.name ^ ": " ^ Shared.typeToString typ
                            |> formatCode;
                          name = item.name;
                          loc = item.loc;
                          deprecated = item.deprecated;
                        })
                 | Type (typ, _) ->
                   Some
                     (Type
                        {
                          id = modulePath |> makeId ~identifier:item.name;
                          docstring = item.docstring |> List.map String.trim;
                          signature =
                            typ.decl
                            |> Shared.declToString item.name
                            |> formatCode;
                          name = item.name;
                          loc = item.loc;
                          deprecated = item.deprecated;
                          detail = typeDetail typ ~full ~env;
                        })
                 | Module (Ident p) ->
                   (* module Whatever = OtherModule *)
                   let aliasToModule = p |> pathIdentToString in
                   let modulePath = aliasToModule :: modulePath in
                   Some
                     (ModuleAlias
                        {
                          id = modulePath |> List.rev |> SharedTypes.ident;
                          signature =
                            Printf.sprintf "module %s = %s" item.name
                              aliasToModule;
                          name = item.name;
                          docstring = item.docstring |> List.map String.trim;
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
