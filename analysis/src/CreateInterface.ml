let printSignature ~signature =
  let rec objectToFun typ ~rhs =
    match typ.Types.desc with
    | Tfield (name, _kind, t, obj) ->
      {typ with desc = Tarrow (Labelled name, t, objectToFun obj ~rhs, Cok)}
    | Tnil -> rhs
    | _ -> (* should not happen *) assert false
  in

  (* val_attributes is ignored by Printtyp.tree_of_signature
     so we store the info as primitive in val_kind, and later post-process
     the outcome tree to add the attribute instead *)
  let val_kind_component : Types.value_kind =
    Val_prim
      (Primitive.make ~name:"react.component" ~alloc:false ~native_name:""
         ~native_repr_args:[] ~native_repr_res:Same_as_ocaml_repr)
  in

  let rec processSignature (signature : Types.signature) =
    match signature with
    | Sig_value (id1, vd1)
      :: Sig_value
           ( id2,
             ({
                val_type = {desc = Tarrow (_, {desc = Tobject (tObj, _)}, t2, _)};
              } as vd2) )
         :: rest
      when Ident.name id1 = Ident.name id2 ^ "Props"
           && (* normal case *) vd1.val_loc.loc_ghost ->
      (* {"name": string} => React.element  ~~>  (~name:string) => React.element *)
      let newItem =
        let funType = tObj |> objectToFun ~rhs:t2 in
        Types.Sig_value
          (id2, {vd2 with val_type = funType; val_kind = val_kind_component})
      in
      newItem :: processSignature rest
    | Sig_value (id1, vd1)
      :: Sig_value
           ( id2,
             ({
                val_type =
                  {desc = Tconstr (_, [{desc = Tobject (tObj, _)}; t2], _)};
              } as vd2) )
         :: rest
      when Ident.name id1 = Ident.name id2 ^ "Props"
           && (* module type *) vd1.val_loc = vd2.val_loc ->
      (* React.componentLike<{"name": string}, React.element>  ~~>
         (~name:string) => React.element *)
      let newItem =
        let funType = tObj |> objectToFun ~rhs:t2 in
        Types.Sig_value
          (id2, {vd2 with val_type = funType; val_kind = val_kind_component})
      in
      newItem :: processSignature rest
    | Sig_module (id, modDecl, recStatus) :: rest ->
      let md_type = processModuleType modDecl.md_type in
      Sig_module (id, {modDecl with md_type}, recStatus)
      :: processSignature rest
    | Sig_modtype (id, mtd) :: rest ->
      let mtd_type =
        match mtd.mtd_type with
        | None -> None
        | Some mt -> Some (processModuleType mt)
      in
      Sig_modtype (id, {mtd with mtd_type}) :: processSignature rest
    | (Sig_value (_id, {val_kind = Val_prim prim; val_loc}) as item) :: items
      when prim.prim_native_name <> "" && prim.prim_native_name.[0] = '\132' ->
      Printf.printf "Rescript prim_name:%s %s\n" prim.prim_name
        (SemanticTokens.locToString val_loc);
      item :: processSignature items
    | item :: rest -> item :: processSignature rest
    | [] -> []
  and processModuleType = function
    | Types.Mty_signature signature ->
      Types.Mty_signature (processSignature signature)
    | mt -> mt
  in

  (* Convert the primitive "react.component" back to an attribute *)
  let rec postProcessOutSig outSig = outSig |> List.map prostProcessOutSigItem
  and prostProcessOutSigItem outSigItem =
    match outSigItem with
    | Outcometree.Osig_value vd -> (
      match vd.oval_prims with
      | ["react.component"] ->
        Outcometree.Osig_value
          {
            vd with
            oval_prims = [];
            oval_attributes = [{oattr_name = "react.component"}];
          }
      | _ -> outSigItem)
    | Osig_module (name, Omty_signature osig, recStatus) ->
      Osig_module (name, Omty_signature (postProcessOutSig osig), recStatus)
    | Osig_modtype (name, Omty_signature osig) ->
      Osig_modtype (name, Omty_signature (postProcessOutSig osig))
    | _ -> outSigItem
  in

  let signature = processSignature signature in
  let outSig = Printtyp.tree_of_signature signature in
  let outSig = postProcessOutSig outSig in
  Printtyp.reset_names ();
  let lines =
    outSig
    |> List.map (fun item ->
           Res_doc.toString ~width:!Res_cli.ResClflags.width
             (Res_outcome_printer.printOutSigItemDoc item))
  in
  Printf.printf "%s\n" (lines |> String.concat "\n")

let command ~cmiFile =
  match Shared.tryReadCmi cmiFile with
  | Some cmi_info -> printSignature ~signature:cmi_info.cmi_sign
  | None -> ()
