(* open Analysis *)

module DSL = struct
  type application = {name: string; argument: oak}

  and namedField = {name: string; value: oak}

  and oak =
    | Application of application
    | Record of namedField list
    | Ident of string
    | Tuple of namedField list
    | List of oak list
    | String of string
end

(** Transform the Oak types to string *)
module CodePrinter = struct
  open DSL

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
    line_count: int;
    nesting_level: int;
  }

  type appendEvents = context -> context

  let emptyContext =
    {
      indent_size = 2;
      max_line_length = 80;
      current_indent = 0;
      current_line_column = 0;
      events = [];
      line_count = 0;
      nesting_level = 0;
    }

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

  let debug_context (ctx : context) =
    Format.printf "Current indent: %d, Current line: %d, Events: %d\n"
      ctx.current_indent ctx.line_count (List.length ctx.events);
    ctx

  let increase_nesting ctx = {ctx with nesting_level = ctx.nesting_level + 1}

  let decrease_nesting ctx =
    {ctx with nesting_level = max 0 (ctx.nesting_level - 1)}

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
    {
      ctx with
      events = WriteLine :: ctx.events;
      current_line_column = ctx.current_indent;
      line_count = ctx.line_count + 1;
    }
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
    (* create a short context and check if the expression fits on the current line *)
    let shortCtx = f ctx in
    if
      ctx.line_count == shortCtx.line_count
      && shortCtx.current_line_column <= ctx.max_line_length
    then shortCtx
    else fallback ctx

  let rec genOak (oak : oak) : appendEvents =
    match oak with
    | Application application -> genApplication application
    | Record record -> genRecord record
    | Ident ident -> genIdent ident
    | String str -> !-(Format.sprintf "\"%s\"" str)
    | Tuple ts -> genTuple ts
    | List xs -> genList xs

  and genApplication (application : application) : appendEvents =
    let short =
      !-(application.name) +> sepOpenT
      +> genOak application.argument
      +> sepCloseT
    in
    let long =
      !-(application.name) +> sepOpenT
      +> (match application.argument with
         | List _ | Record _ -> genOak application.argument
         | _ -> indentAndNln (genOak application.argument) +> sepNln)
      +> sepCloseT
    in
    expressionFitsOnRestOfLine short long

  and genRecord (recordFields : namedField list) : appendEvents =
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

  and genTuple (oaks : namedField list) : appendEvents =
    let short = col genNamedField sepComma oaks in
    let long = col genNamedField sepNln oaks in
    expressionFitsOnRestOfLine short long

  and genIdent (ident : string) : appendEvents = !-ident

  and genNamedField (field : namedField) : appendEvents =
    let short = !-(field.name) +> sepEq +> genOak field.value in
    let long =
      !-(field.name) +> sepEq
      +>
      match field.value with
      | List _ | Record _ -> genOak field.value
      | _ -> indentAndNln (genOak field.value)
    in
    expressionFitsOnRestOfLine short long

  and genList (items : oak list) : appendEvents =
    let genItem = function
      | Tuple _ as item -> wrapInParentheses (genOak item)
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

open DSL

let oak =
  DSL.Record
    [
      {
        name = "zig";
        value =
          DSL.Record
            [
              {name = "foo"; value = Ident "baaaaaaaaaaaaaaaaar"};
              {name = "member"; value = Ident "Zigbaaaaaaaaar"};
            ];
      };
      {
        name = "roxas";
        value =
          List
            [
              Ident "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj";
              Ident "meeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee";
              DSL.Record
                [
                  {name = "foo"; value = Ident "baaaaaaaaaaaaaaaaar"};
                  {name = "member"; value = Ident "Zigbaaaaaaaaar"};
                ];
            ];
      };
      {name = "foo"; value = Ident "baaaaaaaaaaaaaaaaar"};
    ]

(* let _ =
   CodePrinter.genOak oak {CodePrinter.emptyContext with max_line_length = 20}
   |> CodePrinter.dump |> Format.printf "%s\n" *)

(*
    Interpret using  ocaml /home/nojaf/projects/rescript-vscode/tools/src/prettier_printer.ml
*)
