let printExpr = typ => {
  try(
    Res_outcome_printer.printOutType(
      Format.str_formatter,
      Printtyp.tree_of_typexp(false, typ),
    )
  ) {
  | _ => Format.fprintf(Format.str_formatter, "Unable to print type")
  };
  Format.flush_str_formatter();
};

let printDecl = (~recStatus, name, decl) => {
  try(
    Res_outcome_printer.printOutSigItem(
      Format.str_formatter,
      Printtyp.tree_of_type_declaration(Ident.create(name), decl, recStatus),
    )
  ) {
  | _ => Format.fprintf(Format.str_formatter, "Unable to print type")
  };
  Format.flush_str_formatter();
};
