(**
 * `startsWith(string, prefix)`
 * true if the string starts with the prefix
 *)
let startsWith s prefix =
  if prefix = "" then true
  else
    let p = String.length prefix in
    p <= String.length s && String.sub s 0 p = prefix

let endsWith s suffix =
  if suffix = "" then true
  else
    let p = String.length suffix in
    let l = String.length s in
    p <= String.length s && String.sub s (l - p) p = suffix

let cmtPosToPosition {Lexing.pos_lnum; pos_cnum; pos_bol} =
  Protocol.{line = pos_lnum - 1; character = pos_cnum - pos_bol}

let cmtLocToRange {Location.loc_start; loc_end} =
  Protocol.{start = cmtPosToPosition loc_start; end_ = cmtPosToPosition loc_end}

let endOfLocation loc length =
  let open Location in
  {
    loc with
    loc_start = {loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - length};
  }

let chopLocationEnd loc length =
  let open Location in
  {
    loc with
    loc_end = {loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - length};
  }

(** An optional List.find *)
let rec find fn items =
  match items with
  | [] -> None
  | one :: rest -> (
    match fn one with
    | None -> find fn rest
    | Some x -> Some x)

let filterMap f =
  let rec aux accu = function
    | [] -> List.rev accu
    | x :: l -> (
      match f x with
      | None -> aux accu l
      | Some v -> aux (v :: accu) l)
  in
  aux []

let dumpPath path = Str.global_replace (Str.regexp_string "\\") "/" path
let isUncurriedInternal path = startsWith (Path.name path) "Js.Fn.arity"

let flattenLongIdent ?(jsx = false) ?(cutAtOffset = None) lid =
  let rec loop lid =
    match lid with
    | Longident.Lident txt -> ([txt], String.length txt)
    | Ldot (lid, txt) ->
      let path, offset = loop lid in
      if Some offset = cutAtOffset then ("" :: path, offset + 1)
      else if jsx && txt = "createElement" then (path, offset)
      else if txt = "_" then ("" :: path, offset + 1)
      else (txt :: path, offset + 1 + String.length txt)
    | Lapply _ -> ([], 0)
  in
  let path, _ = loop lid in
  List.rev path
