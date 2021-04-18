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

let protocolLineColToCmtLoc ~line ~col = (line + 1, col)

let cmtPosToPosition {Lexing.pos_lnum; pos_cnum; pos_bol} = Protocol.{
  line = pos_lnum - 1;
  character = pos_cnum - pos_bol;
}

let cmtLocToRange {Location.loc_start; loc_end} = Protocol.{
  start = cmtPosToPosition loc_start;
  end_ = cmtPosToPosition loc_end;
}

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
