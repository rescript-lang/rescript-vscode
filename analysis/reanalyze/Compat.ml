let getStringTag s = match s with Format.String_tag s -> s | _ -> ""

(* https://github.com/ocaml/ocaml/blob/9a31c888b177f3aa603bbbe17852cbb57f047df4/stdlib/list.ml#L254-L262 passed though refmt *)
let filter_map f =
  let rec aux accu = function
    | [] -> List.rev accu
    | x :: l -> (
      match f x with None -> aux accu l | Some v -> aux (v :: accu) l)
  in
  aux []

let getStringValue const =
  match const with Parsetree.Pconst_string (s, _) -> s | _ -> assert false

let getConstString const =
  match const with Asttypes.Const_string (s, _) -> s | _ -> assert false

type 'a typedtreeCase = Typedtree.case

type 'a generalPattern = Typedtree.pattern

type ('a, 'b) type_kind = Types.type_kind

let unboxPatCstrName pat =
  match pat.Typedtree.pat_desc with
  | Tpat_construct (_, {cstr_name}, _) -> Some cstr_name
  | _ -> None

let unboxPatCstrTxt pat =
  match pat with
  | Typedtree.Tpat_construct ({txt}, _, _) -> txt
  | _ -> assert false

let getSigValue si =
  match si with
  | Types.Sig_value (id, {Types.val_loc; val_kind; val_type}) ->
    (id, val_loc, val_kind, val_type)
  | _ -> assert false

let getSigType si =
  match si with Types.Sig_type (id, t, _) -> (id, t) | _ -> assert false

let getTSubst td = match td with Types.Tsubst t -> t | _ -> assert false

let getTypeVariant (tk : ('a, 'b) type_kind) =
  match tk with Type_variant l -> l | _ -> assert false

let getSigModuleModtype si =
  match si with
  | Types.Sig_module (id, {Types.md_type = moduleType; md_loc = loc}, _)
  | Types.Sig_modtype (id, {Types.mtd_type = Some moduleType; mtd_loc = loc}) ->
    Some (id, moduleType, loc)
  | _ -> None

let getMtyFunctorModuleType (moduleType : Types.module_type) =
  match moduleType with
  | Mty_functor (_, mtParam, mt) -> Some (mtParam, mt)
  | _ -> None

let getTexpMatch desc =
  match desc with
  | Typedtree.Texp_match (e, casesOK, casesExn, partial) ->
    (e, casesOK @ casesExn, partial)
  | _ -> assert false

let texpMatchGetExceptions desc =
  match desc with
  | Typedtree.Texp_match (_, _, casesExn, _) ->
    casesExn |> List.map (fun (case : Typedtree.case) -> case.c_lhs.pat_desc)
  | _ -> assert false

let texpMatchHasExceptions desc = texpMatchGetExceptions desc != []

let getPayload x =
  let {Asttypes.txt}, payload = x in
  (txt, payload)

let tstrExceptionGet (x : Typedtree.structure_item_desc) =
  match x with
  | Tstr_exception {ext_id; ext_loc} -> Some (ext_id, ext_loc)
  | _ -> None

let moduleIdName name = name |> Ident.name

let get_desc x = x.Types.desc

module Ident = Ident