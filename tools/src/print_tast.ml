(*  *)

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

  let rec path_to_string = function
    | Path.Pident id -> Ident.name id
    | Path.Pdot (p, s, _) -> path_to_string p ^ "." ^ s
    | Path.Papply (p1, p2) -> path_to_string p1 ^ "(" ^ path_to_string p2 ^ ")"

  let rec mk_type_desc (desc : Types.type_desc) : oak =
    match desc with
    | Tvar _ -> Ident "type_desc.Tvar"
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
    | Tobject _ -> Ident "type_desc.Tobject"
    | Tfield _ -> Ident "type_desc.Tfield"
    | Tnil -> Ident "type_desc.Tnil"
    | Tlink {desc} -> Ident "type_desc.Tlink"
    | Tsubst _ -> Ident "type_desc.Tsubst"
    | Tvariant row_descr -> Ident "type_desc.Tvariant"
    | Tunivar _ -> Ident "type_desc.Tunivar"
    | Tpoly _ -> Ident "type_desc.Tpoly"
    | Tpackage _ -> Ident "type_desc.Tpackage"
end

(** Transform the Oak types to string *)
module CodePrinter = struct
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

  let sepComma ctx = !-", " ctx
  let sepSemi ctx = !-"; " ctx
  let sepOpenT ctx = !-"(" ctx
  let sepCloseT ctx = !-")" ctx
  let sepOpenR ctx = !-"{" ctx
  let sepCloseR ctx = !-"}" ctx
  let sepOpenL ctx = !-"[" ctx
  let sepCloseL ctx = !-"]" ctx
  let sepEq ctx = !-" = " ctx
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
    let addSpaces n = String.make n ' ' in

    List.fold_right
      (fun event (acc, current_indent) ->
        match event with
        | Write str -> (acc ^ str, current_indent)
        | WriteLine -> (acc ^ "\n" ^ addSpaces current_indent, current_indent)
        | IndentBy n -> (acc, current_indent + n)
        | UnindentBy n -> (acc, current_indent - n))
      ctx.events ("", 0)
    |> fst

  let rec genOak (oak : Oak.oak) : appendEvents =
    match oak with
    | Oak.Application application -> genApplication application
    | Oak.Record record -> genRecord record
    | Oak.Ident ident -> genIdent ident
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
      +> indentAndNln (genOak application.argument)
      +> sepNln +> sepCloseT
    in
    expressionFitsOnRestOfLine short long

  and genRecord (recordFields : Oak.namedField list) : appendEvents =
    let short =
      sepOpenR +> col genNamedField sepSemi recordFields +> sepCloseR
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
      | Oak.List _ -> genOak field.value
      | _ -> indentAndNln (genOak field.value)
    in
    expressionFitsOnRestOfLine short long

  and genList (items : Oak.oak list) : appendEvents =
    let short = sepOpenL +> col genOak sepSemi items +> sepCloseL in
    let long =
      sepOpenL +> indentAndNln (col genOak sepNln items) +> sepNln +> sepCloseL
    in
    expressionFitsOnRestOfLine short long
end

let print_type_expr (typ : Types.type_expr) : string =
  CodePrinter.genOak (Oak.mk_type_desc typ.desc) CodePrinter.emptyContext
  |> CodePrinter.dump

(* let oak =
   Oak.Application
     {
       Oak.name = "foo";
       argument =
         Oak.Tuple [{Oak.name = "foo"; value = Oak.Ident "baaaaaaaaaaaaaaaaar"}];
     } *)
(* Oak.Record
   [
     {Oak.name = "foo"; value = Oak.Ident "baaaaaaaaaaaaaaaaar"};
     {Oak.name = "member"; value = Oak.Ident "Zigbar"};
   ] *)

(* let _ =
   CodePrinter.genOak oak CodePrinter.emptyContext
   |> CodePrinter.dump |> Format.printf "%s\n" *)

(*
    Interpret using  ocaml /home/nojaf/projects/rescript-vscode/tools/src/print_tast.ml
*)
