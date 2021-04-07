let printExpr = typ => {
  Printtyp.reset_names();
  Res_doc.toString(
    ~width=60,
    Res_outcome_printer.printOutTypeDoc(Printtyp.tree_of_typexp(false, typ)),
  );
};

let printDecl = (~recStatus, name, decl) => {
  Printtyp.reset_names();
  Res_doc.toString(
    ~width=60,
    Res_outcome_printer.printOutSigItemDoc(
      Printtyp.tree_of_type_declaration(Ident.create(name), decl, recStatus),
    ),
  );
};
