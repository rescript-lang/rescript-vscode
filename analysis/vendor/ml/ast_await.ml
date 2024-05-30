let is_await : Parsetree.attribute -> bool =
  fun ({txt}, _) -> txt = "await" || txt = "res.await"

let create_await_expression (e : Parsetree.expression) =
  let loc = {e.pexp_loc with loc_ghost = true} in
  let unsafe_await =
    Ast_helper.Exp.ident ~loc
      {txt = Ldot (Ldot (Lident "Js", "Promise"), "unsafe_await"); loc}
  in
  Ast_helper.Exp.apply ~loc unsafe_await [(Nolabel, e)]

(* Transform `@res.await M` to unpack(@res.await Js.import(module(M: __M0__))) *)
let create_await_module_expression ~module_type_lid (e : Parsetree.module_expr)
    =
  let open Ast_helper in
  let remove_await_attribute =
    List.filter (fun ((loc, _) : Parsetree.attribute) -> loc.txt != "res.await")
  in
  {
    e with
    pmod_desc =
      Pmod_unpack
        (create_await_expression
           (Exp.apply ~loc:e.pmod_loc
              (Exp.ident ~loc:e.pmod_loc
                 {
                   txt = Longident.Ldot (Lident "Js", "import");
                   loc = e.pmod_loc;
                 })
              [
                ( Nolabel,
                  Exp.constraint_ ~loc:e.pmod_loc
                    (Exp.pack ~loc:e.pmod_loc
                       {
                         e with
                         pmod_attributes =
                           remove_await_attribute e.pmod_attributes;
                       })
                    (Typ.package ~loc:e.pmod_loc module_type_lid []) );
              ]));
  }
