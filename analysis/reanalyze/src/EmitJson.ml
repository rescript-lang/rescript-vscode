let items = ref 0

let start () = Format.fprintf Format.std_formatter "["

let finish () = Format.fprintf Format.std_formatter "\n]@."

let emitClose () = Format.fprintf Format.std_formatter "\n}"

let emitItem ~issue ~kind ~file ~range ~message =
  let open Format in
  items := !items + 1;
  let ppf = std_formatter in
  let startLine, startCharacter, endLine, endCharacter = range in
  fprintf ppf "%s{\n" (if !items = 1 then "\n" else ",\n");
  fprintf ppf "  \"name\": \"%s\",\n" issue;
  fprintf ppf "  \"kind\": \"%s\",\n" kind;
  fprintf ppf "  \"file\": \"%s\",\n" file;
  fprintf ppf "  \"range\": [%d,%d,%d,%d],\n" startLine startCharacter endLine
    endCharacter;
  fprintf ppf "  \"message\": \"%s\"" message

let locToPos (loc : Location.t) =
  (loc.loc_start.pos_lnum - 1, loc.loc_start.pos_cnum - loc.loc_start.pos_bol)

let emitAnnotate ~pos ~text ~action =
  let line, character = pos in
  Format.fprintf Format.std_formatter
    ",\n\
    \  \"annotate\": { \"line\": %d, \"character\": %d, \"text\": \"%s\", \
     \"action\": \"%s\"}"
    line character text action
