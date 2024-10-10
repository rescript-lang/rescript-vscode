open Prettier_printer
open DSL
open Analysis

module Transform = struct
  let mk_bool (b : bool) : oak = if b then Ident "true" else Ident "false"

  let mk_string_option (o : string option) : oak =
    match o with
    | None -> Ident "None"
    | Some s -> Application ("Some", String s)

  let mk_string_list (items : string list) : oak =
    List (items |> List.map (fun s -> String s))

  let path_to_string path =
    let buf = Buffer.create 64 in
    let rec aux = function
      | Path.Pident id -> Buffer.add_string buf (Ident.name id)
      | Path.Pdot (p, s, _) ->
        aux p;
        Buffer.add_char buf '.';
        Buffer.add_string buf s
      | Path.Papply (p1, p2) ->
        aux p1;
        Buffer.add_char buf '(';
        aux p2;
        Buffer.add_char buf ')'
    in
    aux path;
    Buffer.contents buf

  let mk_row_field (row_field : Types.row_field) : oak =
    match row_field with
    | Rpresent _ -> Ident "row_field.Rpresent"
    | Reither _ -> Ident "row_field.Reither"
    | Rabsent -> Ident "row_field.Rabsent"

  let mk_field_kind = function
    | Types.Fvar _ -> Ident "field_kind.Fvar"
    | Types.Fpresent -> Ident "field_kind.Fpresent"
    | Types.Fabsent -> Ident "field_kind.Fabsent"

  let rec mk_type_desc (desc : Types.type_desc) : oak =
    match desc with
    | Tlink {desc} -> Application ("type_desc.Tlink", mk_type_desc desc)
    | Tvar var -> (
      match var with
      | None -> Application ("type_desc.Tvar", Ident "None")
      | Some s -> Application ("type_desc.Tvar", Ident s))
    | Tconstr (path, ts, _) ->
      let ts =
        ts |> List.map (fun (t : Types.type_expr) -> mk_type_desc t.desc)
      in
      Application
        ( "type_desc.Tconstr",
          Tuple
            [
              {name = "path"; value = Ident (path_to_string path)};
              {name = "ts"; value = List ts};
            ] )
    | Tarrow (_, t1, t2, _) ->
      Application
        ( "type_desc.Tarrow",
          Tuple
            [
              {name = "t1"; value = mk_type_desc t1.desc};
              {name = "t2"; value = mk_type_desc t2.desc};
            ] )
    | Ttuple ts ->
      let ts =
        ts |> List.map (fun (t : Types.type_expr) -> mk_type_desc t.desc)
      in
      Application ("type_desc.Ttuple", List ts)
    | Tobject (t, r) -> (
      match !r with
      | None -> Application ("type_desc.Tobject", mk_type_desc t.desc)
      | Some (path, ts) ->
        Application
          ( "type_desc.Tobject",
            Tuple
              [
                {name = "type_expr"; value = mk_type_desc t.desc};
                {name = "path"; value = Ident (path_to_string path)};
                {
                  name = "ts";
                  value =
                    List
                      (ts
                      |> List.map (fun (t : Types.type_expr) ->
                             mk_type_desc t.desc));
                };
              ] ))
    | Tfield (field, fk, t1, t2) ->
      Application
        ( "type_desc.Tfield",
          Tuple
            [
              {name = "name"; value = String field};
              {name = "field_kind"; value = mk_field_kind fk};
              {name = "t1"; value = mk_type_desc t1.desc};
              {name = "t2"; value = mk_type_desc t2.desc};
            ] )
    | Tnil -> Ident "type_desc.Tnil"
    | Tsubst t -> Application ("type_desc.Tsubst", mk_type_desc t.desc)
    | Tvariant row_descr ->
      Application ("type_desc.Tvariant", mk_row_desc row_descr)
    | Tunivar so -> Application ("type_desc.Tunivar", mk_string_option so)
    | Tpoly (t, ts) ->
      let ts =
        ts |> List.map (fun (t : Types.type_expr) -> mk_type_desc t.desc)
      in
      Application
        ( "type_desc.Tpoly",
          Tuple
            [
              {name = "t"; value = mk_type_desc t.desc};
              {name = "ts"; value = List ts};
            ] )
    | Tpackage (path, lids, ts) ->
      let lids =
        lids
        |> List.map (fun (lid : Longident.t) ->
               List
                 (Longident.flatten lid |> List.map (fun ident -> String ident)))
      in
      let ts =
        ts |> List.map (fun (t : Types.type_expr) -> mk_type_desc t.desc)
      in
      Application
        ( "type_desc.Tpackage",
          Tuple
            [
              {name = "path"; value = Ident (path_to_string path)};
              {name = "lids"; value = List lids};
              {name = "ts"; value = List ts};
            ] )

  and mk_row_desc (row_desc : Types.row_desc) : oak =
    let fields =
      [
        {
          name = "row_fields";
          value =
            ( row_desc.row_fields
            |> List.map (fun (label, row_field) ->
                   Tuple
                     [
                       {name = "label"; value = Ident label};
                       {name = "row_field"; value = mk_row_field row_field};
                     ])
            |> fun ts -> List ts );
        };
        {name = "row_more"; value = mk_type_desc row_desc.row_more.desc};
        {name = "row_closed"; value = mk_bool row_desc.row_closed};
        {name = "row_fixed"; value = mk_bool row_desc.row_fixed};
      ]
    in
    match row_desc.row_name with
    | None -> Record fields
    | Some (path, ts) ->
      Record
        ({
           name = "row_name";
           value =
             Tuple
               [
                 {name = "Path.t"; value = Ident (path_to_string path)};
                 {
                   name = "fields";
                   value =
                     List
                       (ts
                       |> List.map (fun (t : Types.type_expr) ->
                              mk_type_desc t.desc));
                 };
               ];
         }
        :: fields)

  let mk_FileSet (fileSet : SharedTypes.FileSet.t) : oak =
    List (fileSet |> SharedTypes.FileSet.to_list |> List.map (fun s -> String s))

  let mk_builtInCompletionModules
      (builtInCompletionModules : SharedTypes.builtInCompletionModules) : oak =
    Record
      [
        {
          name = "arrayModulePath";
          value = mk_string_list builtInCompletionModules.arrayModulePath;
        };
        {
          name = "optionModulePath";
          value = mk_string_list builtInCompletionModules.optionModulePath;
        };
        {
          name = "stringModulePath";
          value = mk_string_list builtInCompletionModules.stringModulePath;
        };
        {
          name = "intModulePath";
          value = mk_string_list builtInCompletionModules.intModulePath;
        };
        {
          name = "floatModulePath";
          value = mk_string_list builtInCompletionModules.floatModulePath;
        };
        {
          name = "promiseModulePath";
          value = mk_string_list builtInCompletionModules.promiseModulePath;
        };
        {
          name = "listModulePath";
          value = mk_string_list builtInCompletionModules.listModulePath;
        };
        {
          name = "resultModulePath";
          value = mk_string_list builtInCompletionModules.resultModulePath;
        };
        {
          name = "exnModulePath";
          value = mk_string_list builtInCompletionModules.exnModulePath;
        };
        {
          name = "regexpModulePath";
          value = mk_string_list builtInCompletionModules.regexpModulePath;
        };
      ]

  let mk_package (package : SharedTypes.package) : oak =
    Record
      [
        {
          name = "genericJsxModule";
          value = mk_string_option package.genericJsxModule;
        };
        {name = "suffix"; value = String package.suffix};
        {name = "rootPath"; value = String package.rootPath};
        {name = "projectFiles"; value = mk_FileSet package.projectFiles};
        {
          name = "dependenciesFiles";
          value = mk_FileSet package.dependenciesFiles;
        };
        {name = "namespace"; value = mk_string_option package.namespace};
        {
          name = "builtInCompletionModules";
          value = mk_builtInCompletionModules package.builtInCompletionModules;
        };
        {name = "opens"; value = mk_string_list (List.concat package.opens)};
        {name = "uncurried"; value = mk_bool package.uncurried};
        {
          name = "rescriptVersion";
          value =
            (let major, minor = package.rescriptVersion in
             Tuple
               [
                 {name = "major"; value = String (string_of_int major)};
                 {name = "minor"; value = String (string_of_int minor)};
               ]);
        };
      ]

  let mk_Uri (uri : Uri.t) : oak = String (Uri.toString uri)

  let mk_item (item : SharedTypes.Module.item) : oak =
    let kind =
      match item.kind with
      | SharedTypes.Module.Value v ->
        Application ("SharedTypes.Module.Value", mk_type_desc v.desc)
      | SharedTypes.Module.Type _ -> Ident "Type"
      | SharedTypes.Module.Module _ -> Ident "Module"
    in
    Record
      [
        {name = "name"; value = String item.name};
        {name = "kind"; value = kind};
        {name = "docstring"; value = mk_string_list item.docstring};
        {name = "deprecated"; value = mk_string_option item.deprecated};
      ]

  let mk_structure (structure : SharedTypes.Module.structure) : oak =
    Record
      [
        {name = "name"; value = String structure.name};
        {name = "docstring"; value = mk_string_list structure.docstring};
        {name = "items"; value = List (List.map mk_item structure.items)};
        {name = "deprecated"; value = mk_string_option structure.deprecated};
      ]

  let mk_file (file : SharedTypes.File.t) : oak =
    Record
      [
        {name = "uri"; value = mk_Uri file.uri};
        {name = "moduleName"; value = String file.moduleName};
        {name = "structure"; value = mk_structure file.structure};
      ]

  let mk_full (full : SharedTypes.full) : oak =
    Record
      [
        {name = "package"; value = mk_package full.package};
        {name = "file"; value = mk_file full.file};
      ]
end

let print_type_expr (typ : Types.type_expr) : string =
  CodePrinter.genOak (Transform.mk_type_desc typ.desc) CodePrinter.emptyContext
  |> CodePrinter.dump

let print_full (full : SharedTypes.full) : string =
  CodePrinter.genOak (Transform.mk_full full) CodePrinter.emptyContext
  |> CodePrinter.dump
