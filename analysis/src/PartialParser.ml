type pipe = PipeId of string list | PipeArray | PipeString

(* Completion context *)
type completionContext = Type | Value | Module | Field

type contextPath =
  | CPId of string list * completionContext
  | CPField of contextPath * string
  | CPObj of contextPath * string

type completion =
  | QualifiedRecordAccess of SharedTypes.path (* e.g. _.A.B.field where _ indicates a path ending in a lowercase id *)
  | RecordAccess of SharedTypes.path * SharedTypes.path * string (* e.g. A.B.var .f1.f2 .f3 *)
  | Path of SharedTypes.path
(* e.g. A.B.var or A.B *)

type completable =
  | Cdecorator of string  (** e.g. @module *)
  | Clabel of string list * string * string list
      (** e.g. (["M", "foo"], "label", ["l1", "l2"]) for M.foo(...~l1...~l2...~label...) *)
  | Cpath of contextPath
  | Cdotpath of string list * completionContext
      (** e.g. ["M", "foo"] for M.foo *)
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
  | Cdotpath (sl, k) ->
    "Cdotpath(" ^ (sl |> list) ^ "," ^ completionContextToString k ^ ")"
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

let determineCompletion (dotpath : SharedTypes.path) =
  let rec loop dotpath =
    match dotpath with
    | [] -> assert false
    | [one] -> Path [one]
    | [one; two] ->
      if Utils.isCapitalized one then Path [one; two]
      else RecordAccess ([one], [], two)
    | one :: rest -> (
      if Utils.isCapitalized one then
        match loop rest with
        | Path path -> Path (one :: path)
        | RecordAccess (valuePath, middleFields, lastField) ->
          RecordAccess (one :: valuePath, middleFields, lastField)
        | QualifiedRecordAccess _ as completion ->
          (* A. _.B.field  -> _.B.field *)
          completion
      else
        match loop rest with
        | Path path ->
          (* x. B.field -> _.B.field *)
          QualifiedRecordAccess path
        | RecordAccess ([name], middleFields, lastField) ->
          RecordAccess ([one], name :: middleFields, lastField)
        | RecordAccess (valuePath, middleFields, lastField) ->
          (* x.A.B.v.f1.f2.f3 --> .A.B.v.f1.f2.f3 *)
          QualifiedRecordAccess (valuePath @ middleFields @ [lastField])
        | QualifiedRecordAccess _ as completion ->
          (* x. _.A.f -> _.A.f *)
          completion)
  in
  loop dotpath

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
