open Prettier_printer
open DSL
open Analysis

module Transform = struct
  let mk_bool (b : bool) : oak = if b then Ident "true" else Ident "false"

  let mk_option f o =
    match o with
    | None -> Ident "None"
    | Some x -> Application ("Some", f x)

  let mk_string_option (o : string option) : oak =
    mk_option (fun s -> String s) o

  let mk_list f l = List (List.map f l)

  let mk_string_list (items : string list) : oak =
    mk_list (fun s -> String s) items

  let mk_int_list (items : int list) : oak =
    mk_list (fun i -> Ident (string_of_int i)) items

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
    | Tvar var -> Application ("type_desc.Tvar", mk_string_option var)
    | Tconstr (path, ts, _) ->
      Application
        ( "type_desc.Tconstr",
          Tuple
            [
              {name = "path"; value = Ident (path_to_string path)};
              {name = "ts"; value = mk_type_expr_list ts};
            ] )
    | Tarrow (_, t1, t2, _) ->
      Application
        ( "type_desc.Tarrow",
          Tuple
            [
              {name = "t1"; value = mk_type_desc t1.desc};
              {name = "t2"; value = mk_type_desc t2.desc};
            ] )
    | Ttuple ts -> Application ("type_desc.Ttuple", mk_type_expr_list ts)
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
      Application
        ( "type_desc.Tpoly",
          Tuple
            [
              {name = "t"; value = mk_type_desc t.desc};
              {name = "ts"; value = mk_type_expr_list ts};
            ] )
    | Tpackage (path, lids, ts) ->
      let lids =
        lids
        |> List.map (fun (lid : Longident.t) ->
               List
                 (Longident.flatten lid |> List.map (fun ident -> String ident)))
      in
      Application
        ( "type_desc.Tpackage",
          Tuple
            [
              {name = "path"; value = Ident (path_to_string path)};
              {name = "lids"; value = List lids};
              {name = "ts"; value = mk_type_expr_list ts};
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

  and mk_type_expr_list ts =
    List (List.map (fun (t : Types.type_expr) -> mk_type_desc t.desc) ts)

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

  let mk_rec_status = function
    | Types.Trec_not -> Ident "Trec_not"
    | Types.Trec_first -> Ident "Trec_first"
    | Types.Trec_next -> Ident "Trec_next"

  let mk_field (field : SharedTypes.field) : oak =
    Record
      [
        {name = "stamp"; value = Ident (string_of_int field.stamp)};
        {name = "fname"; value = String field.fname.txt};
        {name = "typ"; value = mk_type_desc field.typ.desc};
        {name = "optional"; value = mk_bool field.optional};
        {name = "docstring"; value = mk_string_list field.docstring};
        {name = "deprecated"; value = mk_string_option field.deprecated};
      ]

  let mk_pos (pos : Lexing.position) : oak =
    Record
      [
        {name = "pos_fname"; value = String pos.pos_fname};
        {name = "pos_lnum"; value = Ident (string_of_int pos.pos_lnum)};
        {name = "pos_bol"; value = Ident (string_of_int pos.pos_bol)};
        {name = "pos_cnum"; value = Ident (string_of_int pos.pos_cnum)};
      ]

  let mk_location (loc : Location.t) =
    Record
      [
        {name = "loc_start"; value = mk_pos loc.loc_start};
        {name = "loc_end"; value = mk_pos loc.loc_end};
        {name = "loc_ghost"; value = mk_bool loc.loc_ghost};
      ]

  let mk_string_loc (loc : string Location.loc) : oak =
    Record
      [
        {name = "txt"; value = String loc.txt};
        {name = "loc"; value = mk_location loc.loc};
      ]

  let mk_constructor_args (args : SharedTypes.constructorArgs) : oak =
    match args with
    | SharedTypes.InlineRecord fields ->
      Application
        ("constructorArgs.InlineRecord", List (fields |> List.map mk_field))
    | SharedTypes.Args ts ->
      let ts =
        ts
        |> List.map (fun ((t : Types.type_expr), loc) ->
               Tuple
                 [
                   {name = "type"; value = mk_type_desc t.desc};
                   {name = "loc"; value = mk_location loc};
                 ])
      in
      Application ("constructorArgs.Tuple", List ts)

  let mk_constructor (ctor : SharedTypes.Constructor.t) : oak =
    Record
      [
        {name = "stamp"; value = Ident (string_of_int ctor.stamp)};
        {
          name = "cname";
          value =
            Record
              [
                {name = "txt"; value = String ctor.cname.txt};
                {name = "loc"; value = mk_location ctor.cname.loc};
              ];
        };
        {name = "args"; value = mk_constructor_args ctor.args};
        {name = "docstring"; value = mk_string_list ctor.docstring};
        {name = "deprecated"; value = mk_string_option ctor.deprecated};
      ]
  let mk_attribute_payload (payload : Parsetree.payload) : oak =
    match payload with
    | PStr _ -> Ident "payload.PStr"
    | PSig _ -> Ident "payload.PSig"
    | PTyp _ -> Ident "payload.PTyp"
    | PPat _ -> Ident "payload.PPat"

  let mk_attribute (attribute : Parsetree.attribute) : oak =
    let loc, payload = attribute in
    Tuple
      [
        {name = "loc"; value = mk_string_loc loc};
        {name = "payload"; value = mk_attribute_payload payload};
      ]

  let mk_attribute_list (attributes : Parsetree.attributes) =
    List (attributes |> List.map mk_attribute)

  let mk_type_kind (kind : SharedTypes.Type.kind) : oak =
    match kind with
    | SharedTypes.Type.Abstract _ -> Ident "Type.kind.Abstract"
    | SharedTypes.Type.Open -> Ident "Type.kind.Open"
    | SharedTypes.Type.Tuple ts ->
      Application ("Type.kind.Tuple", mk_type_expr_list ts)
    | SharedTypes.Type.Record fields ->
      let fields = List.map mk_field fields in
      Application ("Type.kind.Record", List fields)
    | SharedTypes.Type.Variant ctors ->
      Application ("Type.kind.Variant", List (ctors |> List.map mk_constructor))

  let mk_type_declaration_type_kind (type_kind : Types.type_kind) : oak =
    match type_kind with
    | Type_abstract -> Ident "type_kind.Type_abstract"
    | Type_variant _ -> Ident "type_kind.Type_variant"
    | Type_record _ -> Ident "type_kind.Type_record"
    | Type_open -> Ident "type_kind.Type_open"

  let mk_private_flag = function
    | Asttypes.Private -> Ident "Private"
    | Asttypes.Public -> Ident "Public"

  let mk_unboxed_status (status : Types.unboxed_status) : oak =
    Record
      [
        {name = "unboxed"; value = mk_bool status.unboxed};
        {name = "default"; value = mk_bool status.default};
      ]

  let mk_type_declaration (td : Types.type_declaration) : oak =
    Record
      [
        {name = "type_params"; value = mk_type_expr_list td.type_params};
        {name = "type_arity"; value = Ident (string_of_int td.type_arity)};
        {name = "type_kind"; value = mk_type_declaration_type_kind td.type_kind};
        {name = "type_private"; value = mk_private_flag td.type_private};
        {
          name = "type_manifest";
          value =
            mk_option
              (fun (te : Types.type_expr) -> mk_type_desc te.desc)
              td.type_manifest;
        };
        {
          name = "type_newtype_level";
          value =
            mk_option
              (fun (i1, i2) ->
                Tuple
                  [
                    {name = "i1"; value = Ident (string_of_int i1)};
                    {name = "i2"; value = Ident (string_of_int i2)};
                  ])
              td.type_newtype_level;
        };
        {name = "type_loc"; value = mk_location td.type_loc};
        {name = "type_attributes"; value = mk_attribute_list td.type_attributes};
        {name = "type_immediate"; value = mk_bool td.type_immediate};
        {name = "type_unboxed"; value = mk_unboxed_status td.type_unboxed};
      ]

  let mk_type (type_ : SharedTypes.Type.t) : oak =
    Record
      [
        {name = "kind"; value = mk_type_kind type_.kind};
        {name = "decl"; value = mk_type_declaration type_.decl};
        {name = "name"; value = String type_.name};
        {name = "attributes"; value = mk_attribute_list type_.attributes};
      ]

  let mk_item (item : SharedTypes.Module.item) : oak =
    let kind =
      match item.kind with
      | SharedTypes.Module.Value v ->
        Application ("SharedTypes.Module.Value", mk_type_desc v.desc)
      | SharedTypes.Module.Type (t, rec_status) ->
        Application
          ( "Type",
            Tuple
              [
                {name = "type"; value = mk_type t};
                {name = "rec_status"; value = mk_rec_status rec_status};
              ] )
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
