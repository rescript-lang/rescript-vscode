let printSignature ~signature =
  let rec objectToFun typ ~rhs =
    match typ.Types.desc with
    | Tfield (name, _kind, t, obj) ->
      {typ with desc = Tarrow (Labelled name, t, objectToFun obj ~rhs, Cok)}
    | Tnil -> rhs
    | _ -> (* should not happen *) assert false
  in

  Printtyp.reset_names ();
  let sigItemToString (item : Outcometree.out_sig_item) =
    item |> Res_outcome_printer.printOutSigItemDoc
    |> Res_doc.toString ~width:!Res_cli.ResClflags.width
  in

  let buf = Buffer.create 10 in

  let rec processSignature ~indent (signature : Types.signature) : unit =
    match signature with
    | Sig_value (id1, vd1)
      :: Sig_value
           ( id2,
             ({
                val_type = {desc = Tarrow (_, {desc = Tobject (tObj, _)}, t2, _)};
              } as vd2) )
         :: rest
      when Ident.name id1 = Ident.name id2 ^ "Props"
           && (* from implementation *) vd1.val_loc.loc_ghost ->
      (* {"name": string} => React.element  ~~>  (~name:string) => React.element *)
      let funType = tObj |> objectToFun ~rhs:t2 in
      let newItemStr =
        sigItemToString
          (Printtyp.tree_of_value_description id2 {vd2 with val_type = funType})
      in
      Buffer.add_string buf (indent ^ "@react.component\n");
      Buffer.add_string buf (indent ^ newItemStr ^ "\n");
      processSignature ~indent rest
    | Sig_value (id1, vd1)
      :: Sig_value
           ( id2,
             ({
                val_type =
                  {desc = Tconstr (_, [{desc = Tobject (tObj, _)}; t2], _)};
              } as vd2) )
         :: rest
      when Ident.name id1 = Ident.name id2 ^ "Props"
           && (* from interface *) vd1.val_loc = vd2.val_loc ->
      (* React.componentLike<{"name": string}, React.element>  ~~>
         (~name:string) => React.element *)
      let funType = tObj |> objectToFun ~rhs:t2 in
      let newItemStr =
        sigItemToString
          (Printtyp.tree_of_value_description id2 {vd2 with val_type = funType})
      in
      Buffer.add_string buf (indent ^ "@react.component\n");
      Buffer.add_string buf (indent ^ newItemStr ^ "\n");
      processSignature ~indent rest
    | Sig_module (id, modDecl, recStatus) :: rest ->
      Buffer.add_string buf
        (indent
        ^ (match recStatus with
          | Trec_not -> "module "
          | Trec_first -> "module rec "
          | Trec_next -> "and ")
        ^ Ident.name id ^ ": {\n");
      processModuleType ~indent:(indent ^ "  ") modDecl.md_type;
      Buffer.add_string buf (indent ^ "}\n");
      processSignature ~indent rest
    | Sig_modtype (id, mtd) :: rest ->
      let () =
        match mtd.mtd_type with
        | None ->
          Buffer.add_string buf (indent ^ "module type " ^ Ident.name id ^ "\n")
        | Some mt ->
          Buffer.add_string buf
            (indent ^ "module type " ^ Ident.name id ^ " = {\n");
          processModuleType ~indent:(indent ^ "  ") mt;
          Buffer.add_string buf (indent ^ "}\n")
      in
      processSignature ~indent rest
    | Sig_value (id, ({val_kind = Val_prim prim; val_loc} as vd)) :: items
      when prim.prim_native_name <> "" && prim.prim_native_name.[0] = '\132' ->
      Printf.printf "Rescript prim_name:%s %s\n" prim.prim_name
        (SemanticTokens.locToString val_loc);
      let newItemStr =
        sigItemToString (Printtyp.tree_of_value_description id vd)
      in
      Buffer.add_string buf (indent ^ newItemStr ^ "\n");
      processSignature ~indent items
    | Sig_value (id, vd) :: items ->
      let newItemStr =
        sigItemToString (Printtyp.tree_of_value_description id vd)
      in
      Buffer.add_string buf (indent ^ newItemStr ^ "\n");
      processSignature ~indent items
    | Sig_type (id, typeDecl, resStatus) :: items ->
      let newItemStr =
        sigItemToString
          (Printtyp.tree_of_type_declaration id typeDecl resStatus)
      in
      Buffer.add_string buf (indent ^ newItemStr ^ "\n");
      processSignature ~indent items
    | Sig_typext (id, extConstr, extStatus) :: items ->
      let newItemStr =
        sigItemToString
          (Printtyp.tree_of_extension_constructor id extConstr extStatus)
      in
      Buffer.add_string buf (indent ^ newItemStr ^ "\n");
      processSignature ~indent items
    | Sig_class _ :: items ->
      (* not needed *)
      processSignature ~indent items
    | Sig_class_type _ :: items ->
      (* not needed *)
      processSignature ~indent items
    | [] -> ()
  and processModuleType ~indent = function
    | Types.Mty_signature signature -> processSignature ~indent signature
    | mt -> assert false
    (* TODO: print and indent *)
  in

  processSignature ~indent:"" signature;
  Printf.printf "%s" (Buffer.contents buf)

let command ~cmiFile =
  match Shared.tryReadCmi cmiFile with
  | Some cmi_info -> printSignature ~signature:cmi_info.cmi_sign
  | None -> ()
