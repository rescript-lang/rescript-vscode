open Analysis

(** Transform the AST types to the more generic Oak format *)
module Oak = struct
  type application = {name: string; argument: oak}

  and namedField = {name: string; value: oak}

  and oak =
    | Application of application
    | Record of namedField list
    | Ident of string
    | Tuple of namedField list
    | List of oak list
    | String of string
  let mk_bool (b : bool) : oak = if b then Ident "true" else Ident "false"

  let mk_string_option (o : string option) : oak =
    match o with
    | None -> Ident "None"
    | Some s -> Application {name = "Some"; argument = String s}

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

  let name_of_type_desc (desc : Types.type_desc) =
    match desc with
    | Tvar _ -> "Tvar"
    | Tarrow _ -> "Tarrow"
    | Ttuple _ -> "Ttuple"
    | Tconstr _ -> "Tconstr"
    | Tobject _ -> "Tobject"
    | Tfield _ -> "Tfield"
    | Tnil -> "Tnil"
    | Tlink _ -> "Tlink"
    | Tsubst _ -> "Tsubst"
    | Tvariant _ -> "Tvariant"
    | Tunivar _ -> "Tunivar"
    | Tpoly _ -> "Tpoly"
    | Tpackage _ -> "Tpackage"

  let rec mk_type_desc (desc : Types.type_desc) : oak =
    Printf.printf "entering mk_type_desc for %s\n" (name_of_type_desc desc);
    match desc with
    | Tlink {desc} ->
      Application {name = "type_desc.Tlink"; argument = mk_type_desc desc}
    | Tvar var -> (
      match var with
      | None -> Application {name = "type_desc.Tvar"; argument = Ident "None"}
      | Some s -> Application {name = "type_desc.Tvar"; argument = Ident s})
    | Tconstr (path, ts, _) ->
      let ts =
        ts |> List.map (fun (t : Types.type_expr) -> mk_type_desc t.desc)
      in
      Application
        {
          name = "type_desc.Tconstr";
          argument =
            Tuple
              [
                {name = "path"; value = Ident (path_to_string path)};
                {name = "ts"; value = List ts};
              ];
        }
    | Tarrow (_, t1, t2, _) ->
      Application
        {
          name = "type_desc.Tarrow";
          argument =
            Tuple
              [
                {name = "t1"; value = mk_type_desc t1.desc};
                {name = "t2"; value = mk_type_desc t2.desc};
              ];
        }
    | Ttuple _ -> Ident "type_desc.Ttuple"
    | Tobject _ -> Ident "type_desc.Tobject"
    | Tfield _ -> Ident "type_desc.Tfield"
    | Tnil -> Ident "type_desc.Tnil"
    | Tsubst _ -> Ident "type_desc.Tsubst"
    | Tvariant row_descr ->
      Application
        {name = "type_desc.Tvariant"; argument = mk_row_desc row_descr}
    | Tunivar _ -> Ident "type_desc.Tunivar"
    | Tpoly _ -> Ident "type_desc.Tpoly"
    | Tpackage _ -> Ident "type_desc.Tpackage"

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

  let mk_package (package : SharedTypes.package) : oak =
    Record
      [
        {
          name = "genericJsxModule";
          value = mk_string_option package.genericJsxModule;
        };
      ]

  let mk_Uri (uri : Uri.t) : oak = String (Uri.toString uri)

  let mk_item (item : SharedTypes.Module.item) : oak =
    let kind =
      match item.kind with
      | SharedTypes.Module.Value v ->
        Application
          {name = "SharedTypes.Module.Value"; argument = mk_type_desc v.desc}
      | SharedTypes.Module.Type _ -> Ident "Type"
      | SharedTypes.Module.Module _ -> Ident "Module"
    in
    Record
      [
        {name = "kind"; value = kind};
        {name = "name"; value = String item.name};
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

(** Transform the Oak types to string *)
module CodePrinter = struct
  (** 
    The idea is that we capture events in a context type.
    Doing this allows us to reason about the current state of the writer
    and whether the next expression fits on the current line or not.

  *)

  type writerEvents =
    | Write of string
    | WriteLine
    | IndentBy of int
    | UnindentBy of int

  type context = {
    indent_size: int;
    max_line_length: int;
    current_indent: int;
    current_line_column: int;
    events: writerEvents list;
  }

  let countLines (ctx : context) =
    ctx.events
    |> List.filter (fun event ->
           match event with
           | WriteLine -> true
           | _ -> false)
    |> List.length

  type appendEvents = context -> context

  let emptyContext =
    {
      indent_size = 2;
      max_line_length = 80;
      current_indent = 0;
      current_line_column = 0;
      events = [];
    }

  (* Type representing the writer context during code printing

      - [indent_size] is the configured indentation size, typically 2
      - [current_indent] is the current indentation size
      - [current_line_column] is the characters written on the current line
      - [events] is the write events in reverse order, head event is last written
  *)

  let id x = x

  (** add a write event to the context *)
  let ( !- ) str ctx =
    {
      ctx with
      events = Write str :: ctx.events;
      current_line_column = ctx.current_line_column + String.length str;
    }

  (** compose two context transforming functions *)
  let ( +> ) f g ctx = g (f ctx)

  let sepNln ctx =
    {ctx with events = WriteLine :: ctx.events; current_line_column = 0}
  let sepSpace ctx = !-" " ctx
  let sepComma ctx = !-", " ctx
  let sepSemi ctx = !-"; " ctx
  let sepOpenT ctx = !-"(" ctx
  let sepCloseT ctx = !-")" ctx
  let sepOpenR ctx = !-"{" ctx
  let sepCloseR ctx = !-"}" ctx
  let sepOpenL ctx = !-"[" ctx
  let sepCloseL ctx = !-"]" ctx
  let sepEq ctx = !-" = " ctx
  let wrapInParentheses f = sepOpenT +> f +> sepCloseT
  let indent ctx =
    let nextIdent = ctx.current_indent + ctx.indent_size in
    {
      ctx with
      current_indent = nextIdent;
      current_line_column = nextIdent;
      events = IndentBy ctx.indent_size :: ctx.events;
    }
  let unindent ctx =
    let nextIdent = ctx.current_indent - ctx.indent_size in
    {
      ctx with
      current_indent = nextIdent;
      current_line_column = nextIdent;
      events = UnindentBy ctx.indent_size :: ctx.events;
    }

  let indentAndNln f = indent +> sepNln +> f +> unindent

  let col (f : 't -> appendEvents) (intertwine : appendEvents) items ctx =
    let rec visit items ctx =
      match items with
      | [] -> ctx
      | [item] -> f item ctx
      | item :: rest ->
        let ctx' = (f item +> intertwine) ctx in
        visit rest ctx'
    in
    visit items ctx

  let expressionFitsOnRestOfLine (f : appendEvents) (fallback : appendEvents)
      (ctx : context) =
    let current_line_count = countLines ctx in
    let shortCtx = f ctx in
    let nextLineCount = countLines shortCtx in
    if
      current_line_count == nextLineCount
      && shortCtx.current_line_column <= ctx.max_line_length
    then shortCtx
    else fallback ctx

  (** Fold all the events in context into text *)
  let dump (ctx : context) =
    let buf = Buffer.create 1024 in
    let addSpaces n = Buffer.add_string buf (String.make n ' ') in

    List.fold_right
      (fun event current_indent ->
        match event with
        | Write str ->
          Buffer.add_string buf str;
          current_indent
        | WriteLine ->
          Buffer.add_char buf '\n';
          addSpaces current_indent;
          current_indent
        | IndentBy n -> current_indent + n
        | UnindentBy n -> current_indent - n)
      ctx.events ctx.current_indent
    |> ignore;
    Buffer.contents buf

  let rec genOak (oak : Oak.oak) : appendEvents =
    match oak with
    | Oak.Application application -> genApplication application
    | Oak.Record record -> genRecord record
    | Oak.Ident ident -> genIdent ident
    | Oak.String str -> !-(Format.sprintf "\"%s\"" str)
    | Oak.Tuple ts -> genTuple ts
    | Oak.List xs -> genList xs

  and genApplication (application : Oak.application) : appendEvents =
    let short =
      !-(application.name) +> sepOpenT
      +> genOak application.argument
      +> sepCloseT
    in
    let long =
      !-(application.name) +> sepOpenT
      +> (match application.argument with
         | Oak.List _ | Oak.Record _ -> genOak application.argument
         | _ -> indentAndNln (genOak application.argument) +> sepNln)
      +> sepCloseT
    in
    expressionFitsOnRestOfLine short long

  and genRecord (recordFields : Oak.namedField list) : appendEvents =
    let short =
      match recordFields with
      | [] -> sepOpenR +> sepCloseR
      | fields ->
        sepOpenR +> sepSpace
        +> col genNamedField sepSemi fields
        +> sepSpace +> sepCloseR
    in
    let long =
      sepOpenR
      +> indentAndNln (col genNamedField sepNln recordFields)
      +> sepNln +> sepCloseR
    in
    expressionFitsOnRestOfLine short long

  and genTuple (oaks : Oak.namedField list) : appendEvents =
    let short = col genNamedField sepComma oaks in
    let long = col genNamedField sepNln oaks in
    expressionFitsOnRestOfLine short long

  and genIdent (ident : string) : appendEvents = !-ident

  and genNamedField (field : Oak.namedField) : appendEvents =
    let short = !-(field.name) +> sepEq +> genOak field.value in
    let long =
      !-(field.name) +> sepEq
      +>
      match field.value with
      | Oak.List _ | Oak.Record _ -> genOak field.value
      | _ -> indentAndNln (genOak field.value)
    in
    expressionFitsOnRestOfLine short long

  and genList (items : Oak.oak list) : appendEvents =
    let genItem = function
      | Oak.Tuple _ as item -> wrapInParentheses (genOak item)
      | item -> genOak item
    in
    let short =
      match items with
      | [] -> sepOpenL +> sepCloseL
      | _ ->
        sepOpenL +> sepSpace +> col genItem sepSemi items +> sepSpace
        +> sepCloseL
    in
    let long =
      sepOpenL +> indentAndNln (col genItem sepNln items) +> sepNln +> sepCloseL
    in
    expressionFitsOnRestOfLine short long
end

let print_type_expr (typ : Types.type_expr) : string =
  CodePrinter.genOak (Oak.mk_type_desc typ.desc) CodePrinter.emptyContext
  |> CodePrinter.dump

let print_full (full : SharedTypes.full) : string =
  CodePrinter.genOak (Oak.mk_full full) CodePrinter.emptyContext
  |> CodePrinter.dump
