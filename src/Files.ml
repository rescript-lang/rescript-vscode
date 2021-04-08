let split str string = Str.split (Str.regexp_string str) string

let removeExtraDots path =
  Str.global_replace (Str.regexp_string "/./") "/" path
  |> Str.global_replace (Str.regexp {|^\./\.\./|}) "../"

(* Win32 & MacOS are case-insensitive *)
let pathEq =
  match Sys.os_type = "Linux" with
  | true -> fun a b -> a = b
  | false -> fun a b -> String.lowercase_ascii a = String.lowercase_ascii b

let pathStartsWith text prefix =
  String.length prefix <= String.length text
  && pathEq (String.sub text 0 (String.length prefix)) prefix

let sliceToEnd str pos = String.sub str pos (String.length str - pos)

let relpath base path =
  if pathStartsWith path base then
    let baselen = String.length base in
    let rest = String.sub path baselen (String.length path - baselen) in
    if rest = "" then "." ^ Filename.dir_sep
    else if rest.[0] = Filename.dir_sep.[0] then
      if String.length rest > 1 && rest.[1] = '.' then sliceToEnd rest 1
      else "." ^ rest
    else if rest.[0] = '.' then rest
    else "." ^ Filename.dir_sep ^ rest
  else
    let rec loop bp pp =
      match (bp, pp) with
      | "." :: ra, _ -> loop ra pp
      | _, "." :: rb -> loop bp rb
      | a :: ra, b :: rb when pathEq a b -> loop ra rb
      | _ -> (bp, pp)
    in
    let base, path =
      loop (split Filename.dir_sep base) (split Filename.dir_sep path)
    in
    String.concat Filename.dir_sep
      ( ( match base = [] with
        | true -> ["."]
        | false -> List.map (fun _ -> "..") base )
      @ path )
    |> removeExtraDots

let maybeStat path =
  try Some (Unix.stat path) with Unix.Unix_error (Unix.ENOENT, _, _) -> None

let getMtime path =
  match maybeStat path with Some {Unix.st_mtime} -> Some st_mtime | _ -> None

let readFile path =
  match maybeStat path with
  | Some {Unix.st_kind = Unix.S_REG} ->
    let ic = open_in path in
    let try_read () =
      match input_line ic with exception End_of_file -> None | x -> Some x
    in
    let rec loop acc =
      match try_read () with
      | Some s -> loop (s :: acc)
      | None ->
        close_in ic;
        List.rev acc
    in
    let text = loop [] |> String.concat (String.make 1 '\n') in
    Some text
  | _ -> None

let readFileResult path =
  match readFile path with
  | None -> Error ("Unable to read " ^ path)
  | Some text -> Ok text

let exists path = match maybeStat path with None -> false | Some _ -> true

let ifExists path = match exists path with true -> Some path | false -> None

let readDirectory dir =
  let maybeGet handle =
    try Some (Unix.readdir handle) with End_of_file -> None
  in
  let rec loop handle =
    match maybeGet handle with
    | None ->
      Unix.closedir handle;
      []
    | Some name
      when name = Filename.current_dir_name || name = Filename.parent_dir_name
      ->
      loop handle
    | Some name -> name :: loop handle
  in
  match Unix.opendir dir with
  | exception Unix.Unix_error (Unix.ENOENT, "opendir", _dir) -> []
  | handle -> loop handle

let rec collectDirs path =
  match maybeStat path with
  | None -> []
  | Some {Unix.st_kind = Unix.S_DIR} ->
    path
    :: ( readDirectory path
       |> List.map (fun name -> collectDirs (Filename.concat path name))
       |> List.concat )
  | _ -> []

let rec collect ?(checkDir = fun _ -> true) path test =
  match maybeStat path with
  | None -> []
  | Some {Unix.st_kind = Unix.S_DIR} ->
    if checkDir path then
      readDirectory path
      |> List.map (fun name ->
          collect ~checkDir (Filename.concat path name) test)
      |> List.concat
    else []
  | _ -> ( match test path with true -> [path] | false -> [] )

let fileConcat a b =
  if
    b <> ""
    && b.[0] = '.'
    && String.length b >= 2
    && b.[1] = Filename.dir_sep.[0]
  then Filename.concat a (String.sub b 2 (String.length b - 2))
  else Filename.concat a b

let isFullPath b =
  b.[0] = '/' || (Sys.win32 && String.length b > 1 && b.[1] = ':')

let maybeConcat a b = if b <> "" && isFullPath b then b else fileConcat a b
