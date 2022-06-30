module SourceFileExtractor = struct
  let create ~path =
    match Files.readFile path with
    | None -> [||]
    | Some text -> text |> String.split_on_char '\n' |> Array.of_list

  let extract lines ~posStart ~posEnd =
    let lineStart, colStart = posStart in
    let lineEnd, colEnd = posEnd in
    let res = ref [] in
    if lineStart < 0 || lineStart > lineEnd || lineEnd >= Array.length lines
    then []
    else (
      for n = lineEnd downto lineStart do
        let line = lines.(n) in
        let len = String.length line in
        if n = lineStart && n = lineEnd then (
          if colStart >= 0 && colStart < colEnd && colEnd <= len then
            let indent = String.make colStart ' ' in
            res :=
              (indent ^ String.sub line colStart (colEnd - colStart)) :: !res)
        else if n = lineStart then (
          if colStart >= 0 && colStart < len then
            let indent = String.make colStart ' ' in
            res := (indent ^ String.sub line colStart (len - colStart)) :: !res)
        else if n = lineEnd then (
          if colEnd > 0 && colEnd <= len then
            res := String.sub line 0 colEnd :: !res)
        else res := line :: !res
      done;
      !res)
end

module AttributesUtils : sig
  type t

  val separate : string list -> t

  val contain : string -> t -> bool

  val concat : string -> t -> string
end = struct
  type t = string list

  let separate lines =
    lines
    |> List.fold_left
         (fun acc line ->
           let trimmedLine = String.trim line in
           if trimmedLine |> String.length > 0 && String.get trimmedLine 0 = '@'
           then (
             let attrDecIdxSeparator = ref (-1) in
             let i = ref 1 in
             let lineLength = String.length line in

             while !attrDecIdxSeparator = -1 do
               if !i < lineLength then
                 let idx = !i in
                 let currentChar = line.[idx] in
                 let prevChar = line.[idx - 1] in
                 if prevChar = ' ' && currentChar <> ' ' && currentChar <> '@'
                 then attrDecIdxSeparator := !i - 1
                 else i := !i + 1
               else attrDecIdxSeparator := !i
             done;

             let attr = String.sub line 0 !attrDecIdxSeparator in
             if String.length attr = 0 then acc else attr :: acc)
           else acc)
         []
    |> List.rev

  let rec contain attributeForSearch attributes =
    match attributes with
    | [] -> false
    | attribute :: rest ->
      if
        attribute |> String.trim |> String.split_on_char ' '
        |> List.exists (fun attribute -> attribute = attributeForSearch)
      then true
      else contain attributeForSearch rest

  let concat = String.concat
end

let printSignature ~extractor ~signature =
  let objectPropsToFun objTyp ~rhs ~makePropsType =
    let propsTbl = Hashtbl.create 1 in
    (* Process the object type of the make function, and map field names to types. *)
    let rec processObjType typ =
      match typ.Types.desc with
      | Tfield (name, kind, {desc = Tlink t | Tsubst t | Tpoly (t, [])}, obj) ->
        processObjType {typ with desc = Tfield (name, kind, t, obj)}
      | Tfield (name, _kind, t, obj) ->
        Hashtbl.add propsTbl name t;
        processObjType obj
      | Tnil -> ()
      | _ -> (* should not happen *) assert false
    in

    processObjType objTyp;

    (* Traverse the type of the makeProps function, and fill the prop types
       by using the corresponding field in the object type of the make function *)
    let rec fillPropsTypes makePropsType ~rhs =
      match makePropsType.Types.desc with
      | Tarrow (((Labelled lbl | Optional lbl) as argLbl), _, retT, c) -> (
        match Hashtbl.find_opt propsTbl lbl with
        | Some propT ->
          {
            makePropsType with
            desc = Tarrow (argLbl, propT, fillPropsTypes retT ~rhs, c);
          }
        | None -> fillPropsTypes retT ~rhs)
      | _ -> rhs
    in

    match objTyp.Types.desc with
    | Tnil ->
      (* component with zero props *)
      {
        objTyp with
        desc =
          Tarrow
            ( Nolabel,
              Ctype.newconstr (Path.Pident (Ident.create "unit")) [],
              rhs,
              Cok );
      }
    | _ -> fillPropsTypes makePropsType ~rhs
  in

  Printtyp.reset_names ();
  let sigItemToString (item : Outcometree.out_sig_item) =
    item |> Res_outcome_printer.printOutSigItemDoc
    |> Res_doc.toString ~width:!Res_cli.ResClflags.width
  in

  let buf = Buffer.create 10 in

  let getComponentType (typ : Types.type_expr) =
    let reactElement =
      Ctype.newconstr (Pdot (Pident (Ident.create "React"), "element", 0)) []
    in
    match typ.desc with
    | Tarrow (_, {desc = Tobject (tObj, _)}, retType, _) -> Some (tObj, retType)
    | Tconstr
        ( Pdot (Pident {name = "React"}, "component", _),
          [{desc = Tobject (tObj, _)}],
          _ ) ->
      Some (tObj, reactElement)
    | Tconstr
        ( Pdot (Pident {name = "React"}, "componentLike", _),
          [{desc = Tobject (tObj, _)}; retType],
          _ ) ->
      Some (tObj, retType)
    | _ -> None
  in

  let rec processSignature ~indent (signature : Types.signature) : unit =
    match signature with
    | Sig_value
        ( makePropsId (* makeProps *),
          {val_loc = makePropsLoc; val_type = makePropsType} )
      :: Sig_value (makeId (* make *), makeValueDesc)
      :: rest
      when Ident.name makePropsId = Ident.name makeId ^ "Props"
           && ((* from implementation *) makePropsLoc.loc_ghost
              || (* from interface *) makePropsLoc = makeValueDesc.val_loc)
           && getComponentType makeValueDesc.val_type <> None ->
      (*
        {"name": string} => retType  ~~>  (~name:string) => retType
        React.component<{"name": string}>  ~~>  (~name:string) => React.element
        React.componentLike<{"name": string}, retType>  ~~>  (~name:string) => retType
      *)
      let tObj, retType =
        match getComponentType makeValueDesc.val_type with
        | None -> assert false
        | Some (tObj, retType) -> (tObj, retType)
      in
      let funType = tObj |> objectPropsToFun ~rhs:retType ~makePropsType in
      let newItemStr =
        sigItemToString
          (Printtyp.tree_of_value_description makeId
             {makeValueDesc with val_type = funType})
      in
      Buffer.add_string buf (indent ^ "@react.component\n");
      Buffer.add_string buf (indent ^ newItemStr ^ "\n");
      processSignature ~indent rest
    | Sig_module (id, modDecl, recStatus) :: rest ->
      let colonOrEquals =
        match modDecl.md_type with
        | Mty_alias _ -> " = "
        | _ -> ": "
      in
      Buffer.add_string buf
        (indent
        ^ (match recStatus with
          | Trec_not -> "module "
          | Trec_first -> "module rec "
          | Trec_next -> "and ")
        ^ Ident.name id ^ colonOrEquals);
      processModuleType ~indent modDecl.md_type;
      Buffer.add_string buf "\n";
      processSignature ~indent rest
    | Sig_modtype (id, mtd) :: rest ->
      let () =
        match mtd.mtd_type with
        | None ->
          Buffer.add_string buf (indent ^ "module type " ^ Ident.name id ^ "\n")
        | Some mt ->
          Buffer.add_string buf (indent ^ "module type " ^ Ident.name id ^ " = ");
          processModuleType ~indent mt;
          Buffer.add_string buf "\n"
      in
      processSignature ~indent rest
    | Sig_value (id, ({val_kind = Val_prim prim; val_loc} as vd)) :: items
      when prim.prim_native_name <> "" && prim.prim_native_name.[0] = '\132' ->
      (* Rescript primitive name, e.g. @val external ... *)
      let lines =
        let posStart, posEnd = Loc.range val_loc in
        extractor |> SourceFileExtractor.extract ~posStart ~posEnd
      in
      let attributes = AttributesUtils.separate lines in

      if AttributesUtils.contain "@inline" attributes then
        (* Generate type signature for @inline declaration *)
        let divider = if List.length lines > 1 then "\n" else " " in

        let sigStr =
          sigItemToString
            (Printtyp.tree_of_value_description id {vd with val_kind = Val_reg})
        in
        Buffer.add_string buf
          ((attributes |> AttributesUtils.concat "\n") ^ divider ^ sigStr ^ "\n")
      else
        (* Copy the external declaration verbatim from the implementation file *)
        Buffer.add_string buf ((lines |> String.concat "\n") ^ "\n");

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
  and processModuleType ~indent (mt : Types.module_type) =
    match mt with
    | Mty_signature signature ->
      Buffer.add_string buf "{\n";
      processSignature ~indent:(indent ^ "  ") signature;
      Buffer.add_string buf (indent ^ "}")
    | Mty_functor _ ->
      let rec collectFunctorArgs ~args (mt : Types.module_type) =
        match mt with
        | Mty_functor (id, None, mt) when Ident.name id = "*" ->
          (* AST encoding of functor with no arguments *)
          collectFunctorArgs ~args mt
        | Mty_functor (id, mto, mt) ->
          collectFunctorArgs ~args:((id, mto) :: args) mt
        | mt -> (List.rev args, mt)
      in
      let args, retMt = collectFunctorArgs ~args:[] mt in
      Buffer.add_string buf "(";
      args
      |> List.iter (fun (id, mto) ->
             Buffer.add_string buf ("\n" ^ indent ^ "  ");
             (match mto with
             | None -> Buffer.add_string buf (Ident.name id)
             | Some mt ->
               Buffer.add_string buf (Ident.name id ^ ": ");
               processModuleType ~indent:(indent ^ "  ") mt);
             Buffer.add_string buf ",");
      if args <> [] then Buffer.add_string buf ("\n" ^ indent);
      Buffer.add_string buf (") =>\n" ^ indent);
      processModuleType ~indent retMt
    | Mty_ident path | Mty_alias (_, path) ->
      let rec outIdentToString (ident : Outcometree.out_ident) =
        match ident with
        | Oide_ident s -> s
        | Oide_dot (ident, s) -> outIdentToString ident ^ "." ^ s
        | Oide_apply (call, arg) ->
          outIdentToString call ^ "(" ^ outIdentToString arg ^ ")"
      in
      Buffer.add_string buf (outIdentToString (Printtyp.tree_of_path path))
  in

  processSignature ~indent:"" signature;
  Buffer.contents buf

let command ~path ~cmiFile =
  match Shared.tryReadCmi cmiFile with
  | Some cmi_info ->
    let extractor = SourceFileExtractor.create ~path in
    printSignature ~extractor ~signature:cmi_info.cmi_sign
  | None -> ""
