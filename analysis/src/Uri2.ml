module Uri : sig
  type t

  val fromLocalPath : string -> t

  val fromPath : string -> t

  val isInterface : t -> bool

  val stripPath : bool ref

  val toPath : t -> string

  val toString : t -> string
end = struct
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

  let fromLocalPath localPath =
    let path = Files.maybeConcat (Unix.getcwd ()) localPath in
    fromPath path

  let isInterface {path} = Filename.check_suffix path "i"

  let toPath {path} = path

  let toString {uri} =
    if !stripPath then
      let n = Str.search_forward (Str.regexp "test") uri 0 in
      String.sub uri n (String.length uri - n)
    else uri
end

include Uri
