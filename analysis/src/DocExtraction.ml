let formatCode content =
  let {Res_driver.parsetree = signature; comments} =
    Res_driver.parseInterfaceFromSource ~forPrinter:true
      ~displayFilename:"<missing-file>" ~source:content
  in
  Res_printer.printInterface ~width:!Res_cli.ResClflags.width ~comments
    signature
  |> String.trim

type docItemDetail =
  | Record of {fieldDocs: (string * string list) list}
  | Variant of {constructorDocs: (string * string list) list}
type docItem =
  | Value of {docstring: string list; signature: string; name: string}
  | Type of {
      docstring: string list;
      signature: string;
      name: string;
      detail: docItemDetail option;
    }
  | Module of docsForModule
and docsForModule = {docstring: string list; name: string; items: docItem list}

let stringifyDocstrings docstrings =
  let open Protocol in
  docstrings
  |> List.map (fun docstring ->
         docstring |> String.trim |> Json.escape |> wrapInQuotes)
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
            |> List.map (fun (fieldName, docstrings) ->
                   stringifyObject ~indentation:(indentation + 1)
                     [
                       ("fieldName", Some (wrapInQuotes fieldName));
                       ("docstrings", Some (stringifyDocstrings docstrings));
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
            |> List.map (fun (constructorName, docstrings) ->
                   stringifyObject ~startOnNewline:true
                     ~indentation:(indentation + 1)
                     [
                       ("constructorName", Some (wrapInQuotes constructorName));
                       ("docstrings", Some (stringifyDocstrings docstrings));
                     ])
            |> array) );
      ]

let rec stringifyDocItem ?(indentation = 0) (item : docItem) =
  let open Protocol in
  match item with
  | Value {docstring; signature; name} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("kind", Some (wrapInQuotes "value"));
        ("name", Some (name |> Json.escape |> wrapInQuotes));
        ( "signature",
          Some (signature |> String.trim |> Json.escape |> wrapInQuotes) );
        ("docstrings", Some (stringifyDocstrings docstring));
      ]
  | Type {docstring; signature; name; detail} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("kind", Some (wrapInQuotes "type"));
        ("name", Some (name |> Json.escape |> wrapInQuotes));
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
        ("kind", Some (wrapInQuotes "module"));
        ("item", Some (stringifyDocsForModule ~indentation:(indentation + 1) m));
      ]

and stringifyDocsForModule ?(indentation = 0) (d : docsForModule) =
  let open Protocol in
  stringifyObject ~startOnNewline:true ~indentation
    [
      ("name", Some (wrapInQuotes d.name));
      ("docstrings", Some (stringifyDocstrings d.docstring));
      ( "items",
        Some
          (d.items
          |> List.map (stringifyDocItem ~indentation:(indentation + 1))
          |> array) );
    ]

let extractDocs ~path ~debug =
  if debug then Printf.printf "extracting docs for %s\n" path;
  match Cmt.loadFullCmtFromPath ~path with
  | None -> ()
  | Some full ->
    let file = full.file in
    let structure = file.structure in
    let env = SharedTypes.QueryEnv.fromFile file in
    let rec extractDocs (structure : SharedTypes.Module.structure) =
      {
        docstring = structure.docstring |> List.map String.trim;
        name = structure.name;
        items =
          structure.items
          |> List.filter_map (fun (item : SharedTypes.Module.item) ->
                 match item.kind with
                 | Value typ ->
                   Some
                     (Value
                        {
                          docstring = item.docstring |> List.map String.trim;
                          signature =
                            "let " ^ item.name ^ ": " ^ Shared.typeToString typ
                            |> formatCode;
                          name = item.name;
                        })
                 | Type (typ, _) ->
                   Some
                     (Type
                        {
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
                                       |> List.map
                                            (fun (field : SharedTypes.field) ->
                                              (field.fname.txt, field.docstring));
                                   })
                            | Some (Tvariant {constructors}) ->
                              Some
                                (Variant
                                   {
                                     constructorDocs =
                                       constructors
                                       |> List.map
                                            (fun (c : SharedTypes.Constructor.t)
                                            -> (c.cname.txt, c.docstring));
                                   })
                            | _ -> None);
                        })
                 | Module (Structure m) -> Some (Module (extractDocs m))
                 | _ -> None);
      }
    in
    let docs = extractDocs structure in
    print_endline (stringifyDocsForModule docs)
