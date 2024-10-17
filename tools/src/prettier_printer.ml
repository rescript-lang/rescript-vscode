module DSL = struct
  type namedField = {name: string; value: oak}

  and oak =
    | Application of string * oak
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

  type writerMode = Standard | TrySingleLine | ConfirmedMultiline

  (* Type representing the writer context during code printing

     - [indent_size] is the configured indentation size, typically 2
     - [max_line_length] is the maximum line length before we break the line
     - [current_indent] is the current indentation size
     - [current_line_column] is the characters written on the current line
     - [line_count] is the number of lines written
     - [events] is the write events in reverse order, head event is last written
     - [mode] is the current writer mode (Standard or SingleLine)
  *)
  type context = {
    indent_size: int;
    max_line_length: int;
    current_indent: int;
    current_line_column: int;
    line_count: int;
    events: writerEvents list;
    mode: writerMode;
  }

  type appendEvents = context -> context

  let emptyContext =
    {
      indent_size = 2;
      max_line_length = 120;
      current_indent = 0;
      current_line_column = 0;
      line_count = 0;
      events = [];
      mode = Standard;
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
    let mode =
      match ctx.mode with
      | Standard -> "Standard"
      | TrySingleLine -> "TrySingleLine"
      | ConfirmedMultiline -> "ConfirmedMultiline"
    in
    Format.printf
      "Current indent: %d, Current column: %d, # Lines: %d Events: %d, Mode: %s\n"
      ctx.current_indent ctx.current_line_column ctx.line_count
      (List.length ctx.events) mode;
    ctx

  let updateMode (newlineWasAdded : bool) (ctx : context) =
    match ctx.mode with
    | Standard -> ctx
    | ConfirmedMultiline -> ctx
    | TrySingleLine ->
      {
        ctx with
        mode =
          (if newlineWasAdded || ctx.current_line_column > ctx.max_line_length
           then ConfirmedMultiline
           else TrySingleLine);
      }

  let id x = x

  (** add a write event to the context *)
  let write str ctx =
    {
      ctx with
      events = Write str :: ctx.events;
      current_line_column = ctx.current_line_column + String.length str;
    }
    |> updateMode false

  (** compose two context transforming functions *)
  let compose_aux f g ctx =
    let fCtx = f ctx in
    match fCtx.mode with
    | ConfirmedMultiline -> fCtx
    | _ -> g fCtx

  let compose (fs : appendEvents list) ctx =
    let rec visit fs =
      match fs with
      | [] -> id
      | [f] -> f
      | f :: g :: rest -> visit (compose_aux f g :: rest)
    in
    visit fs ctx

  let sepNln ctx =
    {
      ctx with
      events = WriteLine :: ctx.events;
      current_line_column = ctx.current_indent;
      line_count = ctx.line_count + 1;
    }
    |> updateMode true

  let sepSpace ctx = write " " ctx
  let sepComma ctx = write ", " ctx
  let sepSemi ctx = write "; " ctx
  let sepOpenT ctx = write "(" ctx
  let sepCloseT ctx = write ")" ctx
  let sepOpenR ctx = write "{" ctx
  let sepCloseR ctx = write "}" ctx
  let sepOpenL ctx = write "[" ctx
  let sepCloseL ctx = write "]" ctx
  let sepEq ctx = write " = " ctx
  let wrapInParentheses f = compose [sepOpenT; f; sepCloseT]
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

  let indentAndNln f = compose [indent; sepNln; f; unindent]

  let col (f : 't -> appendEvents) (intertwine : appendEvents) items ctx =
    let rec visit items ctx =
      match items with
      | [] -> ctx
      | [item] -> f item ctx
      | item :: rest ->
        let ctx' = compose [f item; intertwine] ctx in
        visit rest ctx'
    in
    visit items ctx

  let expressionFitsOnRestOfLine (f : appendEvents) (fallback : appendEvents)
      (ctx : context) =
    match ctx.mode with
    | ConfirmedMultiline -> ctx
    | _ -> (
      let shortCtx =
        match ctx.mode with
        | Standard -> {ctx with mode = TrySingleLine}
        | _ -> ctx
      in
      let resultCtx = f shortCtx in
      match resultCtx.mode with
      | ConfirmedMultiline -> fallback ctx
      | TrySingleLine -> {resultCtx with mode = ctx.mode}
      | Standard ->
        failwith "Unexpected Standard mode after trying SingleLine mode")

  let rec genOak (oak : oak) : appendEvents =
    match oak with
    | Application (name, argument) -> genApplication name argument
    | Record record -> genRecord record
    | Ident ident -> genIdent ident
    | String str -> write (Format.sprintf "\"%s\"" str)
    | Tuple ts -> genTuple ts
    | List xs -> genList xs

  and genApplication (name : string) (argument : oak) : appendEvents =
    let short = compose [write name; sepOpenT; genOak argument; sepCloseT] in
    let long =
      compose
        [
          write name;
          sepOpenT;
          (match argument with
          | List _ | Record _ -> genOak argument
          | _ -> compose [indentAndNln (genOak argument); sepNln]);
          sepCloseT;
        ]
    in
    expressionFitsOnRestOfLine short long

  and genRecord (recordFields : namedField list) : appendEvents =
    let short =
      match recordFields with
      | [] -> compose [sepOpenR; sepCloseR]
      | fields ->
        compose
          [
            sepOpenR;
            sepSpace;
            col genNamedField sepSemi fields;
            sepSpace;
            sepCloseR;
          ]
    in
    let long =
      compose
        [
          sepOpenR;
          indentAndNln (col genNamedField sepNln recordFields);
          sepNln;
          sepCloseR;
        ]
    in
    expressionFitsOnRestOfLine short long

  and genTuple (oaks : namedField list) : appendEvents =
    let short = col genNamedField sepComma oaks in
    let long = col genNamedField sepNln oaks in
    expressionFitsOnRestOfLine short long

  and genIdent (ident : string) : appendEvents = write ident

  and genNamedField (field : namedField) : appendEvents =
    let genValue =
      match field.value with
      | Tuple _ -> compose [sepOpenT; genOak field.value; sepCloseT]
      | _ -> genOak field.value
    in
    let short = compose [write field.name; sepEq; genValue] in
    let long =
      compose
        [
          write field.name;
          sepEq;
          (match field.value with
          | List _ | Record _ -> genOak field.value
          | _ -> indentAndNln genValue);
        ]
    in
    expressionFitsOnRestOfLine short long

  and genList (items : oak list) : appendEvents =
    let genItem = function
      | Tuple _ as item -> wrapInParentheses (genOak item)
      | item -> genOak item
    in
    let short =
      match items with
      | [] -> compose [sepOpenL; sepCloseL]
      | _ ->
        compose
          [sepOpenL; sepSpace; col genItem sepSemi items; sepSpace; sepCloseL]
    in
    let long =
      compose
        [sepOpenL; indentAndNln (col genItem sepNln items); sepNln; sepCloseL]
    in
    expressionFitsOnRestOfLine short long
end
