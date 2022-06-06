open Common

module Color = struct
  let color_enabled = lazy (Unix.isatty Unix.stdout)

  let forceColor = ref false

  let get_color_enabled () = !forceColor || Lazy.force color_enabled

  type color = Red | Yellow | Magenta | Cyan

  type style = FG of color | Bold | Dim

  let code_of_style = function
    | FG Red -> "31"
    | FG Yellow -> "33"
    | FG Magenta -> "35"
    | FG Cyan -> "36"
    | Bold -> "1"
    | Dim -> "2"

  let getStringTag s = match s with Format.String_tag s -> s | _ -> ""

  let style_of_tag s =
    match s |> getStringTag with
    | "error" -> [Bold; FG Red]
    | "warning" -> [Bold; FG Magenta]
    | "info" -> [Bold; FG Yellow]
    | "dim" -> [Dim]
    | "filename" -> [FG Cyan]
    | _ -> []

  let ansi_of_tag s =
    let l = style_of_tag s in
    let s = String.concat ";" (List.map code_of_style l) in
    "\027[" ^ s ^ "m"

  let reset_lit = "\027[0m"

  let setOpenCloseTag openTag closeTag =
    {
      Format.mark_open_stag = openTag;
      mark_close_stag = closeTag;
      print_open_stag = (fun _ -> ());
      print_close_stag = (fun _ -> ());
    }

  let color_functions =
    setOpenCloseTag
      (fun s -> if get_color_enabled () then ansi_of_tag s else "")
      (fun _ -> if get_color_enabled () then reset_lit else "")

  let setup () =
    Format.pp_set_mark_tags Format.std_formatter true;
    Format.pp_set_formatter_stag_functions Format.std_formatter color_functions;
    if not (get_color_enabled ()) then Misc.Color.setup (Some Never);
    (* Print a dummy thing once in the beginning, as otherwise flushing does not work. *)
    Location.print_loc Format.str_formatter Location.none

  let error ppf s = Format.fprintf ppf "@{<error>%s@}" s

  let info ppf s = Format.fprintf ppf "@{<info>%s@}" s
end

module Loc = struct
  let print_loc ppf (loc : Location.t) =
    (* Change the range so it's on a single line.
       In this way, the line number is clickable in vscode. *)
    let startChar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
    let endChar = startChar + loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
    let line = loc.loc_start.pos_lnum in
    let processPos char (pos : Lexing.position) : Lexing.position =
      {
        pos_lnum = line;
        pos_bol = 0;
        pos_cnum = char;
        pos_fname =
          (let open Filename in
          match is_implicit pos.pos_fname with
          | _ when !Cli.ci -> basename pos.pos_fname
          | true -> concat (Sys.getcwd ()) pos.pos_fname
          | false -> pos.pos_fname);
      }
    in
    Location.print_loc ppf
      {
        loc with
        loc_start = loc.loc_start |> processPos startChar;
        loc_end = loc.loc_end |> processPos endChar;
      }

  let print ppf (loc : Location.t) = Format.fprintf ppf "@[%a@]" print_loc loc
end

let log x = Format.fprintf Format.std_formatter x

let item x =
  Format.fprintf Format.std_formatter "  ";
  Format.fprintf Format.std_formatter x

let missingRaiseInfoToText {exnTable; missingAnnotations; locFull} =
  let missingTxt =
    Format.asprintf "%a" (Exceptions.pp ~exnTable:None) missingAnnotations
  in
  if !Cli.json then
    EmitJson.emitAnnotate ~action:"Add @raises annotation"
      ~pos:(EmitJson.locToPos locFull)
      ~text:(Format.asprintf "@raises(%s)\\n" missingTxt)
  else ""

let logAdditionalInfo ~(description : description) =
  match description with
  | DeadWarning {lineInfo; shouldWriteAnnotation} ->
    if shouldWriteAnnotation then WriteDeadAnnotations.lineInfoToString lineInfo
    else ""
  | ExceptionAnalysisMissing missingRaiseInfo ->
    missingRaiseInfoToText missingRaiseInfo
  | _ -> ""

let missingRaiseInfoToMessage {exnTable; exnName; missingAnnotations; raiseSet}
    =
  let raisesTxt =
    Format.asprintf "%a" (Exceptions.pp ~exnTable:(Some exnTable)) raiseSet
  in
  let missingTxt =
    Format.asprintf "%a" (Exceptions.pp ~exnTable:None) missingAnnotations
  in
  Format.asprintf
    "@{<info>%s@} might raise %s and is not annotated with @raises(%s)" exnName
    raisesTxt missingTxt

let descriptionToMessage (description : description) =
  match description with
  | Circular {message} -> message
  | DeadModule {message} -> message
  | DeadOptional {message} -> message
  | DeadWarning {path; message} ->
    Format.asprintf "@{<info>%s@} %s" path message
  | ExceptionAnalysis {message} -> message
  | ExceptionAnalysisMissing missingRaiseInfo ->
    missingRaiseInfoToMessage missingRaiseInfo
  | Termination {message} -> message

let descriptionToName (description : description) =
  match description with
  | Circular _ -> Issues.warningDeadAnalysisCycle
  | DeadModule _ -> Issues.warningDeadModule
  | DeadOptional {name} -> name
  | DeadWarning {name} -> name
  | ExceptionAnalysis {name} -> name
  | ExceptionAnalysisMissing _ -> Issues.exceptionAnalysis
  | Termination {name} -> name

let logIssue ~(issue : issue) =
  let open Format in
  let loc = issue.loc in
  if !Cli.json then
    let file = Json.escape loc.loc_start.pos_fname in
    let startLine = loc.loc_start.pos_lnum - 1 in
    let startCharacter = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
    let endLine = loc.loc_end.pos_lnum - 1 in
    let endCharacter = loc.loc_end.pos_cnum - loc.loc_start.pos_bol in
    let message = Json.escape (descriptionToMessage issue.description) in
    Format.asprintf "%a%s%s"
      (fun ppf () ->
        EmitJson.emitItem ~ppf:Format.std_formatter ~name:issue.name
          ~kind:
            (match issue.kind with Warning -> "warning" | Error -> "error")
          ~file
          ~range:(startLine, startCharacter, endLine, endCharacter)
          ~message)
      ()
      (logAdditionalInfo ~description:issue.description)
      (if !Cli.json then EmitJson.emitClose () else "")
  else
    let color =
      match issue.kind with Warning -> Color.info | Error -> Color.error
    in
    asprintf "@.  %a@.  %a@.  %s%s@." color issue.name Loc.print issue.loc
      (descriptionToMessage issue.description)
      (logAdditionalInfo ~description:issue.description)

module Stats = struct
  let issues = ref []

  let addIssue (issue : issue) = issues := issue :: !issues

  let clear () = issues := []

  let getSortedIssues () =
    let counters2 = Hashtbl.create 1 in
    !issues
    |> List.iter (fun (issue : issue) ->
           let counter =
             match Hashtbl.find_opt counters2 issue.name with
             | Some counter -> counter
             | None ->
               let counter = ref 0 in
               Hashtbl.add counters2 issue.name counter;
               counter
           in
           incr counter);
    let issues, nIssues =
      Hashtbl.fold
        (fun name cnt (issues, nIssues) ->
          ((name, cnt) :: issues, nIssues + !cnt))
        counters2 ([], 0)
    in
    (issues |> List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2), nIssues)

  let report () =
    !issues |> List.rev
    |> List.iter (fun issue -> logIssue ~issue |> print_string);
    let sortedIssues, nIssues = getSortedIssues () in
    if not !Cli.json then (
      if sortedIssues <> [] then item "@.";
      item "Analysis reported %d issues%s@." nIssues
        (match sortedIssues with
        | [] -> ""
        | _ :: _ ->
          " ("
          ^ (sortedIssues
            |> List.map (fun (name, cnt) -> name ^ ":" ^ string_of_int !cnt)
            |> String.concat ", ")
          ^ ")"))
end

let logKind ~count ~kind ~(loc : Location.t) description =
  let name = descriptionToName description in
  if Suppress.filter loc.loc_start then
    let issue : issue = {name; kind; loc; description} in
    if count then Stats.addIssue issue

let warning ?(count = true) ~loc description =
  description |> logKind ~kind:Warning ~count ~loc

let error ~loc description = description |> logKind ~kind:Error ~count:true ~loc
