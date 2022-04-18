type pipe = PipeId of string list | PipeArray | PipeString

(* Completion context *)
type completionContext = Type | Value | Module | Field

type contextPath =
  | CPId of string list * completionContext
  | CPField of contextPath * string
  | CPObj of contextPath * string

type completable =
  | Cdecorator of string  (** e.g. @module *)
  | Clabel of string list * string * string list
      (** e.g. (["M", "foo"], "label", ["l1", "l2"]) for M.foo(...~l1...~l2...~label...) *)
  | Cpath of contextPath
  | Cjsx of string list * string * string list
      (** E.g. (["M", "Comp"], "id", ["id1", "id2"]) for <M.Comp id1=... id2=... ... id *)
  | Cobj of string list * string list * string
      (** e.g. (["M", "foo"], ["a", "b"], "bar") for M.foo["a"]["b"]["bar" *)
  | Cpipe of pipe * string  (** E.g. ("x", "foo") for "x->foo" *)

let completableToString =
  let str s = if s = "" then "\"\"" else s in
  let list l = "[" ^ (l |> List.map str |> String.concat ", ") ^ "]" in
  let completionContextToString = function
    | Value -> "Value"
    | Type -> "Type"
    | Module -> "Module"
    | Field -> "Field"
  in
  let rec contextPathToString = function
    | CPId (sl, completionContext) ->
      completionContextToString completionContext ^ list sl
    | CPField (cp, s) -> contextPathToString cp ^ "." ^ str s
    | CPObj (cp, s) -> contextPathToString cp ^ "[\"" ^ s ^ "\"]"
  in
  function
  | Cpath cp -> "Cpath " ^ contextPathToString cp
  | Cdecorator s -> "Cdecorator(" ^ str s ^ ")"
  | Clabel (sl1, s, sl2) ->
    "Clabel(" ^ (sl1 |> list) ^ ", " ^ str s ^ ", " ^ (sl2 |> list) ^ ")"
  | Cjsx (sl1, s, sl2) ->
    "Cjsx(" ^ (sl1 |> list) ^ ", " ^ str s ^ ", " ^ (sl2 |> list) ^ ")"
  | Cobj (sl1, sl2, s) ->
    "Cobj(" ^ (sl1 |> list) ^ ", " ^ (sl2 |> list) ^ ", " ^ str s ^ ")"
  | Cpipe (pipe, s) ->
    "Cpipe("
    ^ (match pipe with
      | PipeId sl -> sl |> list
      | PipeArray -> "PipeArray"
      | PipeString -> "PipeString")
    ^ ", " ^ str s ^ ")"

let rec skipWhite text i =
  if i < 0 then 0
  else
    match text.[i] with
    | ' ' | '\n' | '\r' | '\t' -> skipWhite text (i - 1)
    | _ -> i

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
