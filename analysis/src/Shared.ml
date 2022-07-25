let tryReadCmt cmt =
  if not (Files.exists cmt) then (
    Log.log ("Cmt file does not exist " ^ cmt);
    None)
  else
    match Cmt_format.read_cmt cmt with
    | exception Cmi_format.Error err ->
      Log.log
        ("Failed to load " ^ cmt ^ " as a cmt w/ ocaml version " ^ "406"
       ^ ", error: "
        ^
        (Cmi_format.report_error Format.str_formatter err;
         Format.flush_str_formatter ()));
      None
    | exception err ->
      Log.log
        ("Invalid cmt format " ^ cmt
       ^ " - probably wrong ocaml version, expected " ^ Config.version ^ " : "
       ^ Printexc.to_string err);
      None
    | x -> Some x

let tryReadCmi cmi =
  if not (Files.exists cmi) then None
  else
    match Cmt_format.read_cmi cmi with
    | exception _ ->
      Log.log ("Failed to load " ^ cmi);
      None
    | x -> Some x

(** TODO move to the Process_ stuff *)
let rec dig typ =
  match typ.Types.desc with
  | Types.Tlink inner -> dig inner
  | Types.Tsubst inner -> dig inner
  | Types.Tpoly (inner, _) -> dig inner
  | _ -> typ

let digConstructor expr =
  let expr = dig expr in
  match expr.desc with
  | Tconstr (path, _args, _memo) -> Some path
  | _ -> None

let declToString ?(recStatus = Types.Trec_not) name t =
  PrintType.printDecl ~recStatus name t

let cacheTypeToString = ref false
let typeTbl = Hashtbl.create 1

let typeToString ?lineWidth (t : Types.type_expr) =
  match
    if !cacheTypeToString then Hashtbl.find_opt typeTbl (t.id, t) else None
  with
  | None ->
    let s = PrintType.printExpr ?lineWidth t in
    Hashtbl.replace typeTbl (t.id, t) s;
    s
  | Some s -> s
