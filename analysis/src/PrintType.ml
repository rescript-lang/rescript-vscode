let printExpr ?(lineWidth = 60) typ =
  Printtyp.reset_names ();
  Res_doc.toString ~width:lineWidth
    (Res_outcome_printer.printOutTypeDoc (Printtyp.tree_of_typexp false typ))

let printDecl ?printNameAsIs ~recStatus name decl =
  Printtyp.reset_names ();
  Res_doc.toString ~width:60
    (Res_outcome_printer.printOutSigItemDoc ?printNameAsIs
       (Printtyp.tree_of_type_declaration (Ident.create name) decl recStatus))

let printIdentLike ~allowUident name =
  Res_doc.toString ~width:60
    (Res_outcome_printer.printIdentLike ~allowUident name)
