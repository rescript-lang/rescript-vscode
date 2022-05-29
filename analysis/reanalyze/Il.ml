module StringMap = Map.Make (String)

module Kind = struct
  type t = Arrow of t array * t | Star | Tuple of t list

  let rec toString k =
    match k with
    | Star -> "*"
    | Tuple ks -> "(" ^ (ks |> List.map toString |> String.concat ", ") ^ ")"
    | Arrow (arr, t) ->
      (Tuple (arr |> Array.to_list) |> toString) ^ " => " ^ (t |> toString)

  let extractDeclTypes typ =
    let rec extract acc (typ : Types.type_expr) =
      match Compat.get_desc typ with
      | Tlink t -> t |> extract acc
      | Tsubst _ -> Compat.get_desc typ |> Compat.getTSubst |> extract acc
      | Tarrow (lbl, t1, t2, _) -> t2 |> extract ((lbl, t1) :: acc)
      | _ -> (List.rev acc, typ)
    in
    typ |> extract []

  let rec fromType (typ : Types.type_expr) =
    match Compat.get_desc typ with
    | Tlink t -> t |> fromType
    | Tsubst _ -> Compat.get_desc typ |> Compat.getTSubst |> fromType
    | Tarrow _ ->
      let declTypes, retType = typ |> extractDeclTypes in
      Arrow
        ( declTypes |> List.map (fun (_lbl, t) -> t |> fromType) |> Array.of_list,
          retType |> fromType )
    | Ttuple ts -> Tuple (ts |> List.map fromType)
    | _ -> Star
end

type const = I32 of int32 | F64 of string

type offset = int

type instr =
  | Call of string
  | Const of const
  | LocalGet of offset
  | LocalSet of offset
  | F64Add
  | F64Mul
  | I32Add
  | I32Load of offset
  | I32Store of offset

type id = string

type scope = Local of offset | Tuple of scope list

type funDef = {
  id : id;
  kind : Kind.t;
  mutable body : instr list;
  mutable nextOffset : int;
  mutable numParams : int;
}

let constToString const =
  match const with
  | I32 i -> "i32.const " ^ Int32.to_string i
  | F64 s -> "f64.const " ^ s

module Init = struct
  type t = Const of const | Tuple of t list

  let rec toString i =
    match i with
    | Const const -> const |> constToString
    | Tuple is -> "(" ^ (is |> List.map toString |> String.concat ", ") ^ ")"
end

type globalDef = {id : id; init : Init.t}

type def = FunDef of funDef | GlobalDef of globalDef | LocalScope of scope

module FunDef = struct
  let create ~id ~kind = {id; kind; body = []; nextOffset = 0; numParams = 0}

  let emit ~instr def = def.body <- instr :: def.body

  let dumpParams ppf def =
    for i = 0 to def.numParams - 1 do
      Format.fprintf ppf "(param %d) " i
    done

  let dumpLocalDecls ppf def =
    for i = def.numParams to def.nextOffset - 1 do
      Format.fprintf ppf "(local %d) " i
    done
end

module Mem = struct
  type index = int

  type data = String of {index : index; string : string}

  type t = {
    mutable nextIndex : index;
    mutable dataSegments : data list;
    strings : (string, index) Hashtbl.t;
  }

  let stringAlignment = 4

  let align ~alignment size =
    match (size mod alignment) [@doesNotRaise] = 0 with
    | true -> size
    | false -> ( ((4 + (size / 4 * 4)) [@doesNotRaise]))

  let create () = {nextIndex = 0; dataSegments = []; strings = Hashtbl.create 1}

  let alloc mem ~size : index =
    let index = mem.nextIndex in
    mem.nextIndex <- mem.nextIndex + size;
    index

  let allocString mem ~string : index =
    match Hashtbl.find_opt mem.strings string with
    | None ->
      let size = 1 + String.length string |> align ~alignment:stringAlignment in
      let index = mem |> alloc ~size in
      let data = String {index; string} in
      mem.dataSegments <- data :: mem.dataSegments;
      Hashtbl.replace mem.strings string index;
      index
    | Some index -> index

  let dump ~ppf mem =
    Format.fprintf ppf "@.Dump Memory@.";
    Format.fprintf ppf "(memory $0 %d)@." mem.nextIndex;
    mem.dataSegments |> List.rev
    |> List.iter (fun data ->
           match data with
           | String {index; string} ->
             Format.fprintf ppf "(data (i32.const %d) \"%s\\00\")@." index
               string)
end

module Env = struct
  type id = string

  type t = def StringMap.t

  let add ~id ~def (env : t) = env |> StringMap.add id def

  let instrToString instr =
    "("
    ^ (match instr with
      | Call s -> "call " ^ s
      | Const const -> constToString const
      | F64Add -> "f64.add"
      | F64Mul -> "f64.mul"
      | I32Add -> "i32.add"
      | I32Load n -> "i32.load " ^ "offset=" ^ string_of_int n
      | I32Store n -> "i32.store " ^ "offset=" ^ string_of_int n
      | LocalGet n -> "local.get " ^ string_of_int n
      | LocalSet n -> "local.set " ^ string_of_int n)
    ^ ")"

  let dump ~ppf env =
    Format.fprintf ppf "@.Dump Environment@.";
    env
    |> StringMap.iter (fun _id scope ->
           match scope with
           | FunDef funDef ->
             Format.fprintf ppf "@.%s %s %a%a%s@."
               (match funDef.kind with Arrow _ -> "func" | _ -> "global")
               funDef.id FunDef.dumpParams funDef FunDef.dumpLocalDecls funDef
               (funDef.body |> List.rev_map instrToString |> String.concat " ")
           | GlobalDef {id; init} ->
             Format.fprintf ppf "@.global %s %s@." id (init |> Init.toString)
           | LocalScope _ -> assert false)

  let find ~id (env : t) = env |> StringMap.find_opt id

  let create () : t = StringMap.empty
end
