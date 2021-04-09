(*
  steal from OCaml stdlib
  https://github.com/ocaml/ocaml/blob/7c9c210884e1b46f21af5bb4dfab995bb3336cf7/stdlib/string.ml#L205-L214
*)
let split_on_char sep s =
  let open String in
  let r = ref [] in
  let j = ref (length s) in
  for i = length s - 1 downto 0 do
    if unsafe_get s i = sep then (
      r := sub s (i + 1) (!j - i - 1) :: !r;
      j := i)
  done;
  sub s 0 !j :: !r

let topLoc fname =
  {
    Location.loc_start =
      {Lexing.pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
    Location.loc_end =
      {Lexing.pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
    loc_ghost = false;
  }

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

let cmtLocFromVscode (line, col) = (line + 1, col)

let locWithinLoc inner outer =
  let open Location in
  inner.loc_start.pos_cnum >= outer.loc_start.pos_cnum
  && inner.loc_end.pos_cnum <= outer.loc_end.pos_cnum

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
    match fn one with None -> find fn rest | Some x -> Some x)

let dedup items =
  let m = Hashtbl.create (List.length items) in
  items
  |> List.filter (fun a ->
         if Hashtbl.mem m a then false
         else (
           Hashtbl.add m a ();
           true))

let tupleOfLexing {Lexing.pos_lnum; pos_cnum; pos_bol} =
  (pos_lnum - 1, pos_cnum - pos_bol)

(**
  Check if pos is within the location, but be fuzzy about when the location ends.
  If it's within 5 lines, go with it.
*)
let locationContainsFuzzy {Location.loc_start; loc_end} (l, c) =
  tupleOfLexing loc_start <= (l, c) && tupleOfLexing loc_end >= (l - 5, c)

let filterMap f =
  let rec aux accu = function
    | [] -> List.rev accu
    | x :: l -> (
      match f x with None -> aux accu l | Some v -> aux (v :: accu) l)
  in
  aux []

let filterMapIndex f =
  let rec aux accu i = function
    | [] -> List.rev accu
    | x :: l -> (
      match f i x with
      | None -> aux accu i l
      | Some v -> aux (v :: accu) (i + 1) l)
  in
  aux [] 0
