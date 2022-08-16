open SharedTypes
(* This is intended to be a debug tool. It's by no means complete. Rather, you're encouraged to extend this with printing whatever types you need printing. *)

let emptyLocDenom = "<x>"
let hasCursorDenom = "<*>"
let noCursorDenom = ""

let printLocDenominator loc ~pos =
  match loc |> CursorPosition.classifyLoc ~pos with
  | EmptyLoc -> emptyLocDenom
  | HasCursor -> hasCursorDenom
  | NoCursor -> noCursorDenom

let printLocDenominatorLoc loc ~pos =
  match loc |> CursorPosition.classifyLocationLoc ~pos with
  | CursorPosition.EmptyLoc -> emptyLocDenom
  | HasCursor -> hasCursorDenom
  | NoCursor -> noCursorDenom

let printLocDenominatorPos pos ~posStart ~posEnd =
  match CursorPosition.classifyPositions pos ~posStart ~posEnd with
  | CursorPosition.EmptyLoc -> emptyLocDenom
  | HasCursor -> hasCursorDenom
  | NoCursor -> noCursorDenom

let addIndentation indentation =
  let rec indent str indentation =
    if indentation < 1 then str else indent (str ^ "  ") (indentation - 1)
  in
  indent "" indentation

let printAttributes attributes =
  match List.length attributes with
  | 0 -> ""
  | _ ->
    "["
    ^ (attributes
      |> List.map (fun ({Location.txt}, _payload) -> "@" ^ txt)
      |> String.concat ",")
    ^ "]"

let printConstant const =
  match const with
  | Parsetree.Pconst_integer (s, _) -> "Pconst_integer(" ^ s ^ ")"
  | Pconst_char c -> "Pconst_char(" ^ String.make 1 c ^ ")"
  | Pconst_string (s, delim) ->
    let delim =
      match delim with
      | None -> ""
      | Some delim -> delim ^ " "
    in
    "Pconst_string(" ^ delim ^ s ^ delim ^ ")"
  | Pconst_float (s, _) -> "Pconst_float(" ^ s ^ ")"

let printCoreType typ ~pos =
  printAttributes typ.Parsetree.ptyp_attributes
  ^ (typ.ptyp_loc |> printLocDenominator ~pos)
  ^
  match typ.ptyp_desc with
  | Ptyp_any -> "Ptyp_any"
  | Ptyp_var name -> "Ptyp_var(" ^ Completable.str name ^ ")"
  | Ptyp_constr (loc, _types) ->
    "Ptyp_constr("
    ^ (loc |> printLocDenominatorLoc ~pos)
    ^ (Utils.flattenLongIdent loc.txt |> Completable.ident |> Completable.str)
    ^ ")"
  | _ -> "<unimplemented_ptyp_desc>"

let rec printPattern pattern ~pos ~indentation =
  printAttributes pattern.Parsetree.ppat_attributes
  ^ (pattern.ppat_loc |> printLocDenominator ~pos)
  ^
  match pattern.Parsetree.ppat_desc with
  | Ppat_or (pat1, pat2) ->
    "Ppat_or(\n"
    ^ addIndentation (indentation + 1)
    ^ printPattern pat1 ~pos ~indentation:(indentation + 2)
    ^ ",\n"
    ^ addIndentation (indentation + 1)
    ^ printPattern pat2 ~pos ~indentation:(indentation + 2)
    ^ "\n" ^ addIndentation indentation ^ ")"
  | Ppat_extension (({txt} as loc), _) ->
    "Ppat_extension(%" ^ (loc |> printLocDenominatorLoc ~pos) ^ txt ^ ")"
  | Ppat_var ({txt} as loc) ->
    "Ppat_var(" ^ (loc |> printLocDenominatorLoc ~pos) ^ txt ^ ")"
  | Ppat_constant const -> "Ppat_constant(" ^ printConstant const ^ ")"
  | Ppat_construct (({txt} as loc), maybePat) ->
    "Ppat_construct("
    ^ (loc |> printLocDenominatorLoc ~pos)
    ^ (Utils.flattenLongIdent txt |> Completable.ident |> Completable.str)
    ^ (match maybePat with
      | None -> ""
      | Some pat -> "," ^ printPattern pat ~pos ~indentation)
    ^ ")"
  | Ppat_variant (label, maybePat) ->
    "Ppat_variant(" ^ Completable.str label
    ^ (match maybePat with
      | None -> ""
      | Some pat -> "," ^ printPattern pat ~pos ~indentation)
    ^ ")"
  | Ppat_record (fields, _) ->
    "Ppat_record(\n"
    ^ addIndentation (indentation + 1)
    ^ "fields:\n"
    ^ (fields
      |> List.map (fun ((Location.{txt} as loc), pat) ->
             addIndentation (indentation + 2)
             ^ (loc |> printLocDenominatorLoc ~pos)
             ^ (Utils.flattenLongIdent txt |> Completable.ident
              |> Completable.str)
             ^ ": "
             ^ printPattern pat ~pos ~indentation:(indentation + 2))
      |> String.concat "\n")
    ^ "\n" ^ addIndentation indentation ^ ")"
  | Ppat_tuple patterns ->
    "Ppat_tuple(\n"
    ^ (patterns
      |> List.map (fun pattern ->
             addIndentation (indentation + 2)
             ^ (pattern |> printPattern ~pos ~indentation:(indentation + 2)))
      |> String.concat ",\n")
    ^ "\n" ^ addIndentation indentation ^ ")"
  | Ppat_any -> "Ppat_any"
  | Ppat_constraint (pattern, typ) ->
    "Ppat_constraint(\n"
    ^ addIndentation (indentation + 1)
    ^ printCoreType typ ~pos ^ ",\n"
    ^ addIndentation (indentation + 1)
    ^ (pattern |> printPattern ~pos ~indentation:(indentation + 1))
    ^ "\n" ^ addIndentation indentation ^ ")"
  | v -> Printf.sprintf "<unimplemented_ppat_desc: %s>" (Utils.identifyPpat v)

and printCase case ~pos ~indentation ~caseNum =
  addIndentation indentation
  ^ Printf.sprintf "case %i:\n" caseNum
  ^ addIndentation (indentation + 1)
  ^ "pattern"
  ^ (case.Parsetree.pc_lhs.ppat_loc |> printLocDenominator ~pos)
  ^ ":\n"
  ^ addIndentation (indentation + 2)
  ^ printPattern case.Parsetree.pc_lhs ~pos ~indentation
  ^ "\n"
  ^ addIndentation (indentation + 1)
  ^ "expr"
  ^ (case.Parsetree.pc_rhs.pexp_loc |> printLocDenominator ~pos)
  ^ ":\n"
  ^ printExprItem case.pc_rhs ~pos ~indentation:(indentation + 2)

and printExprItem expr ~pos ~indentation =
  addIndentation indentation
  ^ printAttributes expr.Parsetree.pexp_attributes
  ^ (expr.pexp_loc |> printLocDenominator ~pos)
  ^
  match expr.Parsetree.pexp_desc with
  | Pexp_match (matchExpr, cases) ->
    "Pexp_match("
    ^ printExprItem matchExpr ~pos ~indentation:0
    ^ ")\n"
    ^ (cases
      |> List.mapi (fun caseNum case ->
             printCase case ~pos ~caseNum:(caseNum + 1)
               ~indentation:(indentation + 1))
      |> String.concat "\n")
  | Pexp_ident {txt} ->
    "Pexp_ident:" ^ (Utils.flattenLongIdent txt |> SharedTypes.Completable.ident)
  | Pexp_apply (expr, args) ->
    let printLabel labelled ~pos =
      match labelled with
      | None -> "<unlabelled>"
      | Some labelled ->
        printLocDenominatorPos pos
          ~posStart:labelled.CompletionFrontEnd.posStart ~posEnd:labelled.posEnd
        ^ "~"
        ^ if labelled.opt then "?" else "" ^ labelled.name
    in
    let args = CompletionFrontEnd.extractExpApplyArgs ~args in
    "Pexp_apply(\n"
    ^ addIndentation (indentation + 1)
    ^ "expr:\n"
    ^ printExprItem expr ~pos ~indentation:(indentation + 2)
    ^ "\n"
    ^ addIndentation (indentation + 1)
    ^ "args:\n"
    ^ (args
      |> List.map (fun arg ->
             addIndentation (indentation + 2)
             ^ printLabel arg.CompletionFrontEnd.label ~pos
             ^ "=\n"
             ^ printExprItem arg.exp ~pos ~indentation:(indentation + 3))
      |> String.concat ",\n")
    ^ "\n" ^ addIndentation indentation ^ ")"
  | Pexp_constant constant -> "Pexp_constant(" ^ printConstant constant ^ ")"
  | Pexp_construct (({txt} as loc), maybeExpr) ->
    "Pexp_construct("
    ^ (loc |> printLocDenominatorLoc ~pos)
    ^ (Utils.flattenLongIdent txt |> Completable.ident |> Completable.str)
    ^ (match maybeExpr with
      | None -> ""
      | Some expr -> ", " ^ printExprItem expr ~pos ~indentation)
    ^ ")"
  | Pexp_variant (label, maybeExpr) ->
    "Pexp_variant(" ^ Completable.str label
    ^ (match maybeExpr with
      | None -> ""
      | Some expr -> "," ^ printExprItem expr ~pos ~indentation)
    ^ ")"
  | Pexp_fun (arg, _maybeDefaultArgExpr, pattern, nextExpr) ->
    "Pexp_fun(\n"
    ^ addIndentation (indentation + 1)
    ^ "arg: "
    ^ (match arg with
      | Nolabel -> "Nolabel"
      | Labelled name -> "Labelled(" ^ name ^ ")"
      | Optional name -> "Optional(" ^ name ^ ")")
    ^ ",\n"
    ^ addIndentation (indentation + 1)
    ^ "pattern: "
    ^ printPattern pattern ~pos ~indentation:(indentation + 2)
    ^ ",\n"
    ^ addIndentation (indentation + 1)
    ^ "next expr:\n"
    ^ printExprItem nextExpr ~pos ~indentation:(indentation + 2)
    ^ "\n" ^ addIndentation indentation ^ ")"
  | Pexp_extension (({txt} as loc), _) ->
    "Pexp_extension(%" ^ (loc |> printLocDenominatorLoc ~pos) ^ txt ^ ")"
  | v -> Printf.sprintf "<unimplemented_pexp_desc: %s>" (Utils.identifyPexp v)

let printValueBinding value ~pos ~indentation =
  printAttributes value.Parsetree.pvb_attributes
  ^ "value" ^ ":\n"
  ^ addIndentation (indentation + 1)
  ^ (value.pvb_pat |> printPattern ~pos ~indentation:(indentation + 1))
  ^ "\n" ^ addIndentation indentation ^ "expr:\n"
  ^ printExprItem value.pvb_expr ~pos ~indentation:(indentation + 1)

let printStructItem structItem ~pos ~source =
  match structItem.Parsetree.pstr_loc |> CursorPosition.classifyLoc ~pos with
  | HasCursor -> (
    let startOffset =
      match
        CompletionFrontEnd.positionToOffset source
          (structItem.pstr_loc |> Loc.start)
      with
      | None -> 0
      | Some offset -> offset
    in
    let endOffset =
      (* Include the next line of the source since that will hold the ast comment pointing to the position.
         Caveat: this only works for single line sources with a comment on the next line. Will need to be
         adapted if that's not the only use case.*)
      let line, _col = structItem.pstr_loc |> Loc.end_ in
      match CompletionFrontEnd.positionToOffset source (line + 2, 0) with
      | None -> 0
      | Some offset -> offset
    in
    print_endline
      ("\nSource:\n// "
      ^ String.sub source startOffset (endOffset - startOffset)
      ^ "\n");
    match structItem.pstr_desc with
    | Pstr_eval (expr, _attributes) ->
      "Pstr_eval(\n" ^ printExprItem expr ~pos ~indentation:1 ^ "\n)"
    | Pstr_value (recFlag, values) ->
      "Pstr_value(\n"
      ^ (match recFlag with
        | Recursive -> "  rec,\n"
        | Nonrecursive -> "")
      ^ (values
        |> List.map (fun value ->
               addIndentation 1 ^ printValueBinding value ~pos ~indentation:1)
        |> String.concat ",\n")
      ^ "\n)"
    | _ -> "<structure_item_not_implemented>")
  | _ -> ""

let dump ~currentFile ~pos =
  let {Res_driver.parsetree = structure; source} =
    Res_driver.parsingEngine.parseImplementation ~forPrinter:true
      ~filename:currentFile
  in

  print_endline
    (structure
    |> List.map (fun structItem -> printStructItem structItem ~pos ~source)
    |> String.concat "")