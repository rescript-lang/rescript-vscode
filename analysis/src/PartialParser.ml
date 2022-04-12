let rec findBack text char i =
  if i < 0 then i
  else if text.[i] = char && (i = 0 || text.[i - 1] <> '/') then i - 1
  else findBack text char (i - 1)

let rec findOpenComment text i =
  if i < 1 then 0
  else if text.[i] = '*' && text.[i - 1] = '/' then i - 2
  else findOpenComment text (i - 1)

let rec findBackSkippingCommentsAndStrings text char pair i level =
  let loop = findBackSkippingCommentsAndStrings text char pair in
  if i < 0 then 0
  else if text.[i] = char then
    if level = 0 then i - 1 else loop (i - 1) (level - 1)
  else if text.[i] = pair then loop (i - 1) (level + 1)
  else
    match text.[i] with
    | '"' -> loop (findBack text '"' (i - 1)) level
    | '/' when i >= 1 && text.[i - 1] = '*' ->
      loop (findOpenComment text (i - 2)) level
    | _ -> loop (i - 1) level

let rec findLineComment text offset =
  if offset <= 0 || text.[offset] = '\n' then None
  else if offset > 0 && text.[offset] = '/' && text.[offset - 1] = '/' then
    Some (offset - 1)
  else findLineComment text (offset - 1)

(* Check if the position is inside a `//` comment *)
let insideLineComment text offset = findLineComment text offset <> None

let skipLineComment text offset =
  match findLineComment text offset with None -> offset | Some n -> n - 1

let rec skipWhite text i =
  if i < 0 then 0
  else
    match text.[i] with
    | ' ' | '\n' | '\r' | '\t' -> skipWhite text (i - 1)
    | _ -> i

let rec startOfLident text i =
  if i < 0 then 0
  else
    match text.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '.' | '_' | '0' .. '9' ->
      startOfLident text (i - 1)
    | _ -> i + 1

(* foo(... ~arg) from ~arg find foo *)
let findCallFromArgument text offset =
  let none = ([], []) in
  let rec loop identsSeen i =
    let i = skipLineComment text i in
    let i = skipWhite text i in
    if i > 0 then
      match text.[i] with
      | '}' ->
        let i1 = findBackSkippingCommentsAndStrings text '{' '}' (i - 1) 0 in
        if i1 > 0 then loop identsSeen i1 else none
      | ')' ->
        let i1 = findBackSkippingCommentsAndStrings text '(' ')' (i - 1) 0 in
        if i1 > 0 then loop identsSeen i1 else none
      | ']' ->
        let i1 = findBackSkippingCommentsAndStrings text '[' ']' (i - 1) 0 in
        if i1 > 0 then loop identsSeen i1 else none
      | '"' ->
        let i1 = findBack text '"' (i - 1) in
        if i1 > 0 then loop identsSeen i1 else none
      | '\'' ->
        let i1 = findBack text '\'' (i - 1) in
        if i1 > 0 then loop identsSeen i1 else none
      | '`' ->
        let i1 = findBack text '`' (i - 1) in
        if i1 > 0 then loop identsSeen i1 else none
      | '(' ->
        let i1 = skipWhite text (i - 1) in
        let i0 = startOfLident text i1 in
        let funLident = String.sub text i0 (i1 - i0 + 1) in
        (Str.split (Str.regexp_string ".") funLident, identsSeen)
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' ->
        let i1 = startOfLident text i in
        let ident = String.sub text i1 (i - i1 + 1) in
        if i1 - 1 > 0 then
          match text.[i1 - 1] with
          | '~' -> loop (ident :: identsSeen) (i1 - 2)
          | _ -> loop identsSeen (i1 - 1)
        else none
      | _ -> loop identsSeen (i - 1)
    else none
  in
  loop [] offset

(* skip A or #A or %A if present *)
let skipOptVariantExtension text i =
  if i > 0 then
    match text.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' ->
      let i = startOfLident text i - 1 in
      let i =
        if i > 0 then match text.[i] with '#' | '%' -> i - 1 | _ -> i else i
      in
      i
    | _ -> i
  else i

(* Figure out whether id should be autocompleted as component prop.
   Find JSX context ctx for component M to autocomplete id (already parsed) as a prop.
   ctx ::= <M args id
   arg ::= id | id = [?] atomicExpr
   atomicExpr ::= id | #id | "abc" | 'a' | 42 | `...` | optVariant {...} | optVariant (...) | <...> | [...]
   optVariant ::= id | #id | %id |  _nothing_
*)
let findJsxContext text offset =
  let rec loop identsSeen i =
    let i = skipWhite text i in
    if i > 0 then
      match text.[i] with
      | '}' ->
        let i1 = findBackSkippingCommentsAndStrings text '{' '}' (i - 1) 0 in
        if i1 > 0 then beforeParen identsSeen i1 else None
      | ')' ->
        let i1 = findBackSkippingCommentsAndStrings text '(' ')' (i - 1) 0 in
        if i1 > 0 then beforeParen identsSeen i1 else None
      | '>' ->
        let i1 = findBackSkippingCommentsAndStrings text '<' '>' (i - 1) 0 in
        if i1 > 0 then beforeValue identsSeen i1 else None
      | ']' ->
        let i1 = findBackSkippingCommentsAndStrings text '[' ']' (i - 1) 0 in
        if i1 > 0 then beforeValue identsSeen i1 else None
      | '"' ->
        let i1 = findBack text '"' (i - 1) in
        if i1 > 0 then beforeValue identsSeen i1 else None
      | '\'' ->
        let i1 = findBack text '\'' (i - 1) in
        if i1 > 0 then beforeValue identsSeen i1 else None
      | '`' ->
        let i1 = findBack text '`' (i - 1) in
        if i1 > 0 then beforeValue identsSeen i1 else None
      | _ ->
        let i1 = startOfLident text i in
        let ident = String.sub text i1 (i - i1 + 1) in
        if i1 >= 1 && ident <> "" then
          match ident.[0] with
          | ('a' .. 'z' | 'A' .. 'Z') when i1 >= 1 && text.[i1 - 1] = '<' ->
            Some (ident, identsSeen)
          | _ ->
            if i1 >= 1 && text.[i1 - 1] = '#' then
              beforeValue identsSeen (i1 - 2)
            else beforeIdent ~ident identsSeen (i1 - 1)
        else None
    else None
  and beforeIdent ~ident identsSeen i =
    let i = skipWhite text i in
    if i > 0 then
      match text.[i] with
      | '?' -> fromEquals identsSeen (i - 1)
      | '=' -> fromEquals identsSeen i
      | _ -> (* punning *) loop (ident :: identsSeen) i
    else None
  and beforeParen identsSeen i =
    let i = skipWhite text i in
    beforeValue identsSeen (skipOptVariantExtension text i)
  and beforeValue identsSeen i =
    let i = skipWhite text i in
    if i > 0 then
      match text.[i] with
      | '?' -> fromEquals identsSeen (i - 1)
      | _ -> fromEquals identsSeen i
    else None
  and fromEquals identsSeen i =
    let i = skipWhite text i in
    if i > 0 then
      match text.[i] with
      | '=' -> (
        let i = skipWhite text (i - 1) in
        let i1 = startOfLident text i in
        let ident = String.sub text i1 (i - i1 + 1) in
        match ident with "" -> None | _ -> loop (ident :: identsSeen) (i1 - 1))
      | _ -> None
    else None
  in
  loop [] offset

type pipe = PipeId of string | PipeArray | PipeString

type completable =
  | Cdecorator of string  (** e.g. @module *)
  | Clabel of string list * string * string list
      (** e.g. (["M", "foo"], "label", ["l1", "l2"]) for M.foo(...~l1...~l2...~label...) *)
  | Cdotpath of string list  (** e.g. ["M", "foo"] for M.foo *)
  | Cjsx of string list * string * string list
      (** E.g. (["M", "Comp"], "id", ["id1", "id2"]) for <M.Comp id1=... id2=... ... id *)
  | Cobj of string list * string list * string
      (** e.g. (["M", "foo"], ["a", "b"], "bar") for M.foo["a"]["b"]["bar" *)
  | Cpipe of pipe * string  (** E.g. ("x", "foo") for "x->foo" *)

let completableToString =
  let str s = if s = "" then "\"\"" else s in
  let list l = "[" ^ (l |> List.map str |> String.concat ", ") ^ "]" in
  function
  | Cdecorator s -> "Cdecorator(" ^ str s ^ ")"
  | Clabel (sl1, s, sl2) ->
    "Clabel(" ^ (sl1 |> list) ^ ", " ^ str s ^ ", " ^ (sl2 |> list) ^ ")"
  | Cdotpath sl -> "Cdotpath(" ^ (sl |> list) ^ ")"
  | Cjsx (sl1, s, sl2) ->
    "Cjsx(" ^ (sl1 |> list) ^ ", " ^ str s ^ ", " ^ (sl2 |> list) ^ ")"
  | Cobj (sl1, sl2, s) ->
    "Cobj(" ^ (sl1 |> list) ^ ", " ^ (sl2 |> list) ^ ", " ^ str s ^ ")"
  | Cpipe (pipe, s) ->
    "Cpipe("
    ^ (match pipe with
      | PipeId s -> str s
      | PipeArray -> "PipeArray"
      | PipeString -> "PipeString")
    ^ ", " ^ str s ^ ")"

let isLowercaseIdent id =
  let rec loop i =
    if i < 0 then true
    else
      match id.[i] with
      | ('a' .. 'z' | '_') when i = 0 -> true
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') when i > 0 -> loop (i - 1)
      | _ -> false
  in
  loop (String.length id - 1)

let findCompletable text offset =
  let mkPath s =
    let len = String.length s in
    let dotpath = Str.split (Str.regexp_string ".") s in
    let dotpath =
      match s.[len - 1] with '.' -> dotpath @ [""] | _ -> dotpath
    in
    match dotpath with
    | [id] when String.lowercase_ascii id = id -> (
      match findJsxContext text (offset - len - 1) with
      | None -> Cdotpath dotpath
      | Some (componentName, identsSeen) ->
        Cjsx (Str.split (Str.regexp_string ".") componentName, id, identsSeen))
    | _ -> Cdotpath dotpath
  in
  let mkPipe off partialName =
    let off = skipWhite text off in
    let rec loop i =
      if i < 0 then Some (PipeId (String.sub text 0 (i - 1)))
      else
        match text.[i] with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' -> loop (i - 1)
        | '"' when i == off -> Some PipeString
        | ']' when i == off -> Some PipeArray
        | _ -> Some (PipeId (String.sub text (i + 1) (off - i)))
    in
    match loop off with
    | None -> None
    | Some lhs -> Some (Cpipe (lhs, partialName))
  in
  let mkObj ~off ~partialName =
    let off = skipWhite text off in
    let rec loop off path i =
      if i < 0 then
        let id = String.sub text 0 (i - 1) in
        Some ([], [id])
      else
        match text.[i] with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' ->
          loop off path (i - 1)
        | ']' when i > 1 && text.[i - 1] = '"' ->
          let i0 = i - 2 in
          let i1 = startOfLident text i0 in
          let ident = String.sub text i1 (i0 - i1 + 1) in
          if ident <> "" && i1 > 1 && text.[i1 - 1] = '"' && text.[i1 - 2] = '['
          then loop (off - i + i1 - 3) (ident :: path) (i1 - 3)
          else None
        | _ ->
          let id = String.sub text (i + 1) (off - i) in
          Some (path, Str.split (Str.regexp_string ".") id)
    in
    match loop off [] off with
    | None -> None
    | Some (path, lhs) -> Some (Cobj (lhs, path, partialName))
  in

  let suffix i = String.sub text (i + 1) (offset - (i + 1)) in
  let rec loop i =
    if i < 0 then Some (mkPath (suffix i))
    else
      match text.[i] with
      | '>' when i > 0 && text.[i - 1] = '-' ->
        let rest = suffix i in
        if isLowercaseIdent rest then mkPipe (i - 2) rest
        else Some (mkPath rest)
      | '~' ->
        let labelPrefix = suffix i in
        let funPath, identsSeen = findCallFromArgument text (i - 1) in
        Some (Clabel (funPath, labelPrefix, identsSeen))
      | '@' -> Some (Cdecorator (suffix i))
      | '"' when i > 0 && text.[i - 1] = '[' ->
        let partialName = suffix i in
        mkObj ~off:(i - 2) ~partialName
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' -> loop (i - 1)
      | ' ' when i = offset - 1 -> (
        (* autocomplete with no id: check if inside JSX *)
        match findJsxContext text (offset - 1) with
        | None -> None
        | Some (componentName, identsSeen) ->
          Some
            (Cjsx
               (Str.split (Str.regexp_string ".") componentName, "", identsSeen))
        )
      | _ -> if i = offset - 1 then None else Some (mkPath (suffix i))
  in
  if offset > String.length text || offset = 0 then None else loop (offset - 1)

let findOpens text offset =
  let opens = ref [] in
  let pathOfModuleOpen o =
    let rec loop items =
      match items with
      | [] -> ["place holder"]
      | one :: rest -> one :: loop rest
    in
    loop (o |> Str.split (Str.regexp_string "."))
  in
  let add o = opens := (o |> pathOfModuleOpen) :: !opens in
  let maybeOpen i0 =
    let rec loop i =
      if i < 4 then 0
      else
        match text.[i] with
        | 'a' .. 'z' | 'A' .. 'Z' | '.' | '_' | '0' .. '9' -> loop (i - 1)
        | ' ' | '!' ->
          let at = skipWhite text (i - 1) in
          let at =
            if at >= 0 && text.[at] = '!' then
              (* handle open! *)
              skipWhite text (at - 1)
            else at
          in
          if
            at >= 3
            && text.[at - 3] = 'o'
            && text.[at - 2] = 'p'
            && text.[at - 1] = 'e'
            && text.[at] = 'n'
            && not (insideLineComment text (at - 4))
          then (
            add (String.sub text (i + 1) (i0 + 1 - (i + 1)));
            at - 4)
          else at
        | _ -> i
    in
    loop (i0 - 1)
  in
  let rec loop i =
    if i > 1 then
      match text.[i] with
      | '}' -> loop (findBackSkippingCommentsAndStrings text '{' '}' (i - 1) 0)
      | ']' -> loop (findBackSkippingCommentsAndStrings text '[' ']' (i - 1) 0)
      | ')' -> loop (findBackSkippingCommentsAndStrings text '(' ')' (i - 1) 0)
      | '"' -> loop (findBack text '"' (i - 1))
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> loop (maybeOpen i)
      | '(' when text.[i - 1] = '.' -> (
        match text.[i - 2] with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' ->
          let i0 = startOfLident text (i - 3) in
          add (String.sub text i0 (i - i0 - 1))
        | _ -> loop (i - 1))
      | _ ->
        if i > 1 && text.[i] = '/' && text.[i - 1] = '*' then
          loop (findOpenComment text (i - 2))
        else loop (i - 1)
  in
  loop (offset - 1) |> ignore;
  !opens

let offsetOfLine text line =
  let ln = String.length text in
  let rec loop i lno =
    if i >= ln then None
    else
      match text.[i] with
      | '\n' -> if lno = line - 1 then Some (i + 1) else loop (i + 1) (lno + 1)
      | _ -> loop (i + 1) lno
  in
  match line with 0 -> Some 0 | _ -> loop 0 0

let positionToOffset text (line, character) =
  match offsetOfLine text line with
  | None -> None
  | Some bol -> Some (bol + character)
