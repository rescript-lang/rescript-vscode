type t = {path : string; uri : string}

let stripPath = ref false (* for use in tests *)

let pathToUri path =
  if Sys.os_type = "Unix" then "file://" ^ path
  else
    "file://"
    ^ (Str.global_replace (Str.regexp_string "\\") "/" path
      |> Str.substitute_first (Str.regexp "^\\([a-zA-Z]\\):") (fun text ->
             let name = Str.matched_group 1 text in
             "/" ^ String.lowercase_ascii name ^ "%3A"))

let fromPath path = {path; uri = pathToUri path}

let isInterface {path} = Filename.check_suffix path "i"

let toPath {path} = path

let toTopLevelLoc {path} =
  let topPos =
    {Lexing.pos_fname = path; pos_lnum = 1; pos_bol = 0; pos_cnum = 0}
  in
  {Location.loc_start = topPos; Location.loc_end = topPos; loc_ghost = false}

let toString {uri} = if !stripPath then Filename.basename uri else uri
