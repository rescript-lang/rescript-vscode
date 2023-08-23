type lines = string array
type snippet = {uri: Uri.t; pos: Pos.t; code: string}
type t = {
  mutable definition: snippet option;
  mutable files: (string, lines) Hashtbl.t;
  mutable preamble: string;
  mutable snippets: snippet list;
}

let readFile {files} name =
  match Hashtbl.find_opt files name with
  | None -> (
    match Files.readFile name with
    | None -> None
    | Some text -> Some (text |> String.split_on_char '\n' |> Array.of_list))
  | Some lines -> Some lines

let snippetRadius (lines : lines) = min (Array.length lines) 10 / 2

let addSnippet ~isDefinition ~pos ~(uri : Uri.t) prompt =
  match readFile prompt (Uri.toPath uri) with
  | None -> () (* ignore files not found *)
  | Some lines ->
    let lineNum = fst pos in
    let radius = snippetRadius lines in
    let firstLine = max 0 (lineNum - radius) in
    let lastLine = min (Array.length lines - 1) (lineNum + radius) in
    let linesInRadius = Array.sub lines firstLine (lastLine - firstLine) in
    let code = linesInRadius |> Array.to_list |> String.concat "\n" in
    let snippet = {uri; pos; code} in
    if isDefinition then prompt.definition <- Some snippet
    else prompt.snippets <- snippet :: prompt.snippets

let printSnippet buf {uri; pos; code} =
  Buffer.add_string buf
    ("{\"file\": "
    ^ Filename.basename (Uri.toString uri)
    ^ ", \"line\": "
    ^ string_of_int (1 + fst pos)
    ^ ", \"code\":\n");
  Buffer.add_string buf code;
  Buffer.add_string buf "\"}\n"

let createForReferences name =
  let quoted = "\"" ^ name ^ "\"" in
  let backticked = "`" ^ name ^ "`" in
  let preamble =
    [
      {|A Snippet has the form: {"file": "Hello.res", "line":23, "code": "...the code..."} where the first line of code is line 23.|};
      "Find Uses of " ^ quoted
      ^ " given snippets for its Definition and its Users.";
      "";
      {|The input has form:
        Definition:
        snippet
        
        Use1:
        snippet1
        
        Use2:
        snippet2
        ...|};
      "";
      "You will produce output of the form:";
      "- `File.res` line `12`: function `foo` calls " ^ backticked ^ " ...";
      "- `File2.res` line `34`: function `bar` uses " ^ backticked ^ " to ...";
      "";
      "Ignore any code in the Snippets that is not directly relevant to using "
      ^ quoted ^ ".";
      "Add enough details to understand at high level how " ^ quoted
      ^ " is used targeted at a person who is trying to undersand the codebase.";
    ]
    |> String.concat "\n"
  in
  {definition = None; files = Hashtbl.create 1; preamble; snippets = []}

let toSegments {definition; preamble; snippets} =
  let segments = ref [] in
  let addSegment s = segments := s :: !segments in
  addSegment (preamble ^ "\n");
  (match definition with
  | None -> ()
  | Some snippet ->
    let buf = Buffer.create 1 in
    Buffer.add_string buf "Definition:\n";
    printSnippet buf snippet;
    addSegment (Buffer.contents buf));
  snippets
  |> List.iteri (fun i s ->
         let buf = Buffer.create 1 in
         Buffer.add_string buf ("Use" ^ string_of_int (i + 1) ^ ":\n");
         printSnippet buf s;
         addSegment (Buffer.contents buf));
  !segments |> List.rev
