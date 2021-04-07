let tryReadCmt = cmt =>
  if (!Files.exists(cmt)) {
    Error("Cmt file does not exist " ++ cmt);
  } else {
    switch (Cmt_format.read_cmt(cmt)) {
    | exception (Cmi_format.Error(err)) =>
      Error(
        "Failed to load "
        ++ cmt
        ++ " as a cmt w/ ocaml version "
        ++ "406"
        ++ ", error: "
        ++ {
          Cmi_format.report_error(Format.str_formatter, err);
          Format.flush_str_formatter();
        },
      )
    | exception err =>
      Error(
        "Invalid cmt format "
        ++ cmt
        ++ " - probably wrong ocaml version, expected "
        ++ Config.version
        ++ " : "
        ++ Printexc.to_string(err),
      )
    | x => Ok(x)
    };
  };

/** TODO move to the Process_ stuff */
let rec dig = typ =>
  switch (typ.Types.desc) {
  | Types.Tlink(inner) => dig(inner)
  | Types.Tsubst(inner) => dig(inner)
  | Types.Tpoly(inner, _) => dig(inner)
  | _ => typ
  };

let digConstructor = expr => {
  let expr = dig(expr);
  switch (expr.desc) {
  | Tconstr(path, _args, _memo) => Some(path)
  | _ => None
  };
};

let declToString = (~recStatus=Types.Trec_not, name, t) =>
  PrintType.printDecl(~recStatus, name, t);

let cacheTypeToString = ref(false);
let typeTbl = Hashtbl.create(1);

let typeToString = (t: Types.type_expr) => {
  switch (cacheTypeToString^ ? Hashtbl.find_opt(typeTbl, (t.id, t)) : None) {
  | None =>
    let s = PrintType.printExpr(t);
    Hashtbl.replace(typeTbl, (t.id, t), s);
    s;
  | Some(s) => s
  };
};
