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

let rec skipWhite text i =
  if i < 0 then 0
  else
    match text.[i] with ' ' | '\n' | '\t' -> skipWhite text (i - 1) | _ -> i

let rec startOfLident text i =
  if i < 0 then 0
  else
    match text.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '.' | '_' | '0' .. '9' ->
      startOfLident text (i - 1)
    | _ -> i + 1

(* foo(... ~arg) from ~arg find foo *)
let findCallFromArgument text offset =
  let rec loop ~i ~nClosed =
    if i > 0 then
      match text.[i] with
      | '(' when nClosed > 0 -> loop ~i:(i - 1) ~nClosed:(nClosed - 1)
      | '(' ->
        let i1 = skipWhite text (i - 1) in
        let i0 = startOfLident text i1 in
        let funLident = String.sub text i0 (i1 - i0 + 1) in
        Str.split (Str.regexp_string ".") funLident
      | ')' -> loop ~i:(i - 1) ~nClosed:(nClosed + 1)
      | _ -> loop ~i:(i - 1) ~nClosed
    else []
  in
  loop ~i:offset ~nClosed:0

(* skip A or #A if present *)
let skipOptVariant text i =
  prerr_endline "skipOptIdent";
  if i > 0 then
    match text.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' ->
      let i = startOfLident text i - 1 in
      let i =
        if i > 0 then match text.[i] with '#' -> i - 1 | _ -> i else i
      in
      i
    | _ -> i
  else i

(* Figure out whether id should be autocompleted as component prop.
   Find JSX context ctx for component M to autocomplete id (already parsed) as a prop.
   ctx ::= <M args id
   arg ::= id | id = [?] atomicExpr
   atomicExpr ::= id | "abc" | 'a' | 42 | `...` | optVariant {...} | optVariant (...) | <...> | [...]
   optVariant ::= a | A | #a |  #A |  _nothing_
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
          | 'A' .. 'Z' when i1 >= 1 && text.[i1 - 1] = '<' ->
            Some (ident, identsSeen)
          | _ -> beforeIdent identsSeen (i1 - 1)
        else None
    else None
  and beforeIdent identsSeen i =
    let i = skipWhite text i in
    if i > 0 then
      match text.[i] with
      | '?' -> fromEquals identsSeen (i - 1)
      | '=' -> fromEquals identsSeen i
      | _ -> loop identsSeen (i - 1)
    else None
  and beforeParen identsSeen i =
    let i = skipWhite text i in
    beforeValue identsSeen (skipOptVariant text i)
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
  | Clabel of string list * string
      (** e.g. (["M", "foo"], "label") for M.foo(...~label...) *)
  | Cpath of string list  (** e.g. ["M", "foo"] for M.foo *)
  | Cjsx of string list * string * string list
      (** E.g. (["M", "Comp"], "id", ["id1", "id2"]) for <M.Comp id1=... id2=... ... id *)
  | Cpipe of pipe * string  (** E.g. ("x", "foo") for "x->foo" *)

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
    let parts = Str.split (Str.regexp_string ".") s in
    let parts = match s.[len - 1] with '.' -> parts @ [""] | _ -> parts in
    match parts with
    | [id] when String.lowercase_ascii id = id -> (
      match findJsxContext text (offset - len - 1) with
      | None -> Cpath parts
      | Some (componentName, identsSeen) ->
        Cjsx (Str.split (Str.regexp_string ".") componentName, id, identsSeen))
    | _ -> Cpath parts
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
        let funPath = findCallFromArgument text (i - 1) in
        Some (Clabel (funPath, labelPrefix))
      | '@' -> Some (Cdecorator (suffix i))
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

(* Check if the position is inside a `//` comment *)
let rec insideLineComment text offset =
  if offset <= 0 || text.[offset] = '\n' then false
  else if offset > 0 && text.[offset] = '/' && text.[offset - 1] = '/' then true
  else insideLineComment text (offset - 1)

let findOpens text offset =
  let opens = ref [] in
  let pathOfModuleOpen o =
    let rec loop items =
      match items with
      | [] -> SharedTypes.Tip "place holder"
      | one :: rest -> Nested (one, loop rest)
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
