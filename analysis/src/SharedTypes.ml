type modulePath =
  | File of Uri.t * string
  | NotVisible
  | IncludedModule of Path.t * modulePath
  | ExportedModule of string * modulePath

type field = {stamp: int; fname: string Location.loc; typ: Types.type_expr}

module Constructor = struct
  type t = {
    stamp: int;
    cname: string Location.loc;
    args: (Types.type_expr * Location.t) list;
    res: Types.type_expr option;
    typeDecl: string * Types.type_declaration;
  }
end

module Type = struct
  type kind =
    | Abstract of (Path.t * Types.type_expr list) option
    | Open
    | Tuple of Types.type_expr list
    | Record of field list
    | Variant of Constructor.t list

  type t = {kind: kind; decl: Types.type_declaration}
end

module Exported = struct
  type namedStampMap = (string, int) Hashtbl.t

  type t = {
    types_: namedStampMap;
    values_: namedStampMap;
    modules_: namedStampMap;
  }

  type kind = Type | Value | Module

  let init () =
    {
      types_ = Hashtbl.create 10;
      values_ = Hashtbl.create 10;
      modules_ = Hashtbl.create 10;
    }

  let add t kind name x =
    let tbl =
      match kind with
      | Type -> t.types_
      | Value -> t.values_
      | Module -> t.modules_
    in
    if Hashtbl.mem tbl name then false
    else
      let () = Hashtbl.add tbl name x in
      true

  let find t kind name =
    let tbl =
      match kind with
      | Type -> t.types_
      | Value -> t.values_
      | Module -> t.modules_
    in
    Hashtbl.find_opt tbl name

  let iter t kind f =
    let tbl =
      match kind with
      | Type -> t.types_
      | Value -> t.values_
      | Module -> t.modules_
    in
    Hashtbl.iter f tbl
end

module Module = struct
  type kind =
    | Value of Types.type_expr
    | Type of Type.t * Types.rec_status
    | Module of t

  and item = {kind: kind; name: string}

  and structure = {
    docstring: string list;
    exported: Exported.t;
    items: item list;
  }

  and t = Ident of Path.t | Structure of structure | Constraint of t * t
end

module Declared = struct
  type 'item t = {
    name: string Location.loc;
    extentLoc: Location.t;
    stamp: int;
    modulePath: modulePath;
    isExported: bool;
    deprecated: string option;
    docstring: string list;
    item: 'item;
  }
end

module Stamps : sig
  type t

  val addConstructor : t -> int -> Constructor.t Declared.t -> unit
  val addModule : t -> int -> Module.t Declared.t -> unit
  val addType : t -> int -> Type.t Declared.t -> unit
  val addValue : t -> int -> Types.type_expr Declared.t -> unit
  val findModule : t -> int -> Module.t Declared.t option
  val findType : t -> int -> Type.t Declared.t option
  val findValue : t -> int -> Types.type_expr Declared.t option
  val init : unit -> t
  val iterConstructors : (int -> Constructor.t Declared.t -> unit) -> t -> unit
  val iterModules : (int -> Module.t Declared.t -> unit) -> t -> unit
  val iterTypes : (int -> Type.t Declared.t -> unit) -> t -> unit
  val iterValues : (int -> Types.type_expr Declared.t -> unit) -> t -> unit
end = struct
  type 't stampMap = (int, 't Declared.t) Hashtbl.t

  type kind =
    | KType of Type.t Declared.t
    | KValue of Types.type_expr Declared.t
    | KModule of Module.t Declared.t
    | KConstructor of Constructor.t Declared.t

  type t = (int, kind) Hashtbl.t

  let init () = Hashtbl.create 10

  let addConstructor (stamps : t) stamp declared =
    Hashtbl.add stamps stamp (KConstructor declared)

  let addModule stamps stamp declared =
    Hashtbl.add stamps stamp (KModule declared)

  let addType stamps stamp declared = Hashtbl.add stamps stamp (KType declared)

  let addValue stamps stamp declared =
    Hashtbl.add stamps stamp (KValue declared)

  let findModule stamps stamp =
    match Hashtbl.find_opt stamps stamp with
    | Some (KModule declared) -> Some declared
    | _ -> None

  let findType stamps stamp =
    match Hashtbl.find_opt stamps stamp with
    | Some (KType declared) -> Some declared
    | _ -> None

  let findValue stamps stamp =
    match Hashtbl.find_opt stamps stamp with
    | Some (KValue declared) -> Some declared
    | _ -> None

  let iterModules f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KModule d -> f stamp d
        | _ -> ())
      stamps

  let iterTypes f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KType d -> f stamp d
        | _ -> ())
      stamps

  let iterValues f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KValue d -> f stamp d
        | _ -> ())
      stamps

  let iterConstructors f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KConstructor d -> f stamp d
        | _ -> ())
      stamps
end

module File = struct
  type t = {
    uri: Uri.t;
    stamps: Stamps.t;
    moduleName: string;
    structure: Module.structure;
  }

  let create moduleName uri =
    {
      uri;
      stamps = Stamps.init ();
      moduleName;
      structure = {docstring = []; exported = Exported.init (); items = []};
    }
end

module QueryEnv = struct
  type t = {file: File.t; exported: Exported.t}

  let fromFile file = {file; exported = file.structure.exported}
end

module Completion = struct
  type kind =
    | Module of Module.t
    | Value of Types.type_expr
    | ObjLabel of Types.type_expr
    | Label of string
    | Type of Type.t
    | Constructor of Constructor.t * string
    | Field of field * string
    | FileModule of string

  type t = {
    name: string;
    env: QueryEnv.t;
    deprecated: string option;
    docstring: string list;
    kind: kind;
  }

  let create ~name ~kind ~env =
    {name; env; deprecated = None; docstring = []; kind}

  (* https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_completion *)
  (* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind *)
  let kindToInt kind =
    match kind with
    | Module _ -> 9
    | FileModule _ -> 9
    | Constructor (_, _) -> 4
    | ObjLabel _ -> 4
    | Label _ -> 4
    | Field (_, _) -> 5
    | Type _ -> 22
    | Value _ -> 12
end

module Env = struct
  type t = {stamps: Stamps.t; modulePath: modulePath}
end

type filePath = string

type paths =
  | Impl of {cmt: filePath; res: filePath}
  | Namespace of {cmt: filePath}
  | IntfAndImpl of {
      cmti: filePath;
      resi: filePath;
      cmt: filePath;
      res: filePath;
    }

let showPaths paths =
  match paths with
  | Impl {cmt; res} ->
    Printf.sprintf "Impl cmt:%s res:%s" (Utils.dumpPath cmt)
      (Utils.dumpPath res)
  | Namespace {cmt} -> Printf.sprintf "Namespace cmt:%s" (Utils.dumpPath cmt)
  | IntfAndImpl {cmti; resi; cmt; res} ->
    Printf.sprintf "IntfAndImpl cmti:%s resi:%s cmt:%s res:%s"
      (Utils.dumpPath cmti) (Utils.dumpPath resi) (Utils.dumpPath cmt)
      (Utils.dumpPath res)

let getSrc p =
  match p with
  | Impl {res} -> [res]
  | Namespace _ -> []
  | IntfAndImpl {resi; res} -> [resi; res]

let getUri p =
  match p with
  | Impl {res} -> Uri.fromPath res
  | Namespace {cmt} -> Uri.fromPath cmt
  | IntfAndImpl {resi} -> Uri.fromPath resi

let getCmtPath ~uri p =
  match p with
  | Impl {cmt} -> cmt
  | Namespace {cmt} -> cmt
  | IntfAndImpl {cmti; cmt} ->
    let interface = Utils.endsWith (Uri.toPath uri) "i" in
    if interface then cmti else cmt

module Tip = struct
  type t = Value | Type | Field of string | Constructor of string | Module

  let toString tip =
    match tip with
    | Value -> "Value"
    | Type -> "Type"
    | Field f -> "Field(" ^ f ^ ")"
    | Constructor a -> "Constructor(" ^ a ^ ")"
    | Module -> "Module"
end

type path = string list

let pathToString (path : path) = path |> String.concat "."

type locKind =
  | LocalReference of int * Tip.t
  | GlobalReference of string * string list * Tip.t
  | NotFound
  | Definition of int * Tip.t

type locType =
  | Typed of string * Types.type_expr * locKind
  | Constant of Asttypes.constant
  | LModule of locKind
  | TopLevelModule of string
  | TypeDefinition of string * Types.type_declaration * int

type locItem = {loc: Location.t; locType: locType}

module LocationSet = Set.Make (struct
  include Location

  let compare loc1 loc2 = compare loc2 loc1

  (* polymorphic compare should be OK *)
end)

type extra = {
  internalReferences: (int, Location.t list) Hashtbl.t;
  externalReferences:
    (string, (string list * Tip.t * Location.t) list) Hashtbl.t;
  fileReferences: (string, LocationSet.t) Hashtbl.t;
  mutable locItems: locItem list;
  (* This is the "open location", like the location...
     or maybe the >> location of the open ident maybe *)
  (* OPTIMIZE: using a stack to come up with this would cut the computation time of this considerably. *)
  opens: (Location.t, unit) Hashtbl.t;
}

type file = string

module FileSet = Set.Make (String)

type package = {
  rootPath: filePath;
  projectFiles: FileSet.t;
  dependenciesFiles: FileSet.t;
  pathsForModule: (file, paths) Hashtbl.t;
  namespace: string option;
  opens: string list;
}

type full = {extra: extra; file: File.t; package: package}

let initExtra () =
  {
    internalReferences = Hashtbl.create 10;
    externalReferences = Hashtbl.create 10;
    fileReferences = Hashtbl.create 10;
    locItems = [];
    opens = Hashtbl.create 10;
  }

type state = {
  packagesByRoot: (string, package) Hashtbl.t;
  rootForUri: (Uri.t, string) Hashtbl.t;
  cmtCache: (filePath, File.t) Hashtbl.t;
}

(* There's only one state, so it can as well be global *)
let state =
  {
    packagesByRoot = Hashtbl.create 1;
    rootForUri = Hashtbl.create 30;
    cmtCache = Hashtbl.create 30;
  }

let locKindToString = function
  | LocalReference (_, tip) -> "(LocalReference " ^ Tip.toString tip ^ ")"
  | GlobalReference _ -> "GlobalReference"
  | NotFound -> "NotFound"
  | Definition (_, tip) -> "(Definition " ^ Tip.toString tip ^ ")"

let locTypeToString = function
  | Typed (name, e, locKind) ->
    "Typed " ^ name ^ " " ^ Shared.typeToString e ^ " "
    ^ locKindToString locKind
  | Constant _ -> "Constant"
  | LModule locKind -> "LModule " ^ locKindToString locKind
  | TopLevelModule _ -> "TopLevelModule"
  | TypeDefinition _ -> "TypeDefinition"

let locItemToString {loc = {Location.loc_start; loc_end}; locType} =
  let pos1 = Utils.cmtPosToPosition loc_start in
  let pos2 = Utils.cmtPosToPosition loc_end in
  Printf.sprintf "%d:%d-%d:%d %s" pos1.line pos1.character pos2.line
    pos2.character (locTypeToString locType)

(* needed for debugging *)
let _ = locItemToString

module Completable = struct
  (* Completion context *)
  type completionContext = Type | Value | Module | Field

  type contextPath =
    | CPString
    | CPArray
    | CPApply of contextPath * Asttypes.arg_label list
    | CPId of string list * completionContext
    | CPField of contextPath * string
    | CPObj of contextPath * string
    | CPPipe of contextPath * string

  type t =
    | Cdecorator of string  (** e.g. @module *)
    | CnamedArg of contextPath * string * string list
        (** e.g. (..., "label", ["l1", "l2"]) for ...(...~l1...~l2...~label...) *)
    | Cnone  (** e.g. don't complete inside strings *)
    | Cpath of contextPath
    | Cjsx of string list * string * string list
        (** E.g. (["M", "Comp"], "id", ["id1", "id2"]) for <M.Comp id1=... id2=... ... id *)

  let toString =
    let str s = if s = "" then "\"\"" else s in
    let list l = "[" ^ (l |> List.map str |> String.concat ", ") ^ "]" in
    let completionContextToString = function
      | Value -> "Value"
      | Type -> "Type"
      | Module -> "Module"
      | Field -> "Field"
    in
    let rec contextPathToString = function
      | CPString -> "string"
      | CPApply (cp, labels) ->
        contextPathToString cp ^ "("
        ^ (labels
          |> List.map (function
               | Asttypes.Nolabel -> "Nolabel"
               | Labelled s -> "~" ^ s
               | Optional s -> "?" ^ s)
          |> String.concat ", ")
        ^ ")"
      | CPArray -> "array"
      | CPId (sl, completionContext) ->
        completionContextToString completionContext ^ list sl
      | CPField (cp, s) -> contextPathToString cp ^ "." ^ str s
      | CPObj (cp, s) -> contextPathToString cp ^ "[\"" ^ s ^ "\"]"
      | CPPipe (cp, s) -> contextPathToString cp ^ "->" ^ s
    in
    function
    | Cpath cp -> "Cpath " ^ contextPathToString cp
    | Cdecorator s -> "Cdecorator(" ^ str s ^ ")"
    | CnamedArg (cp, s, sl2) ->
      "CnamedArg("
      ^ (cp |> contextPathToString)
      ^ ", " ^ str s ^ ", " ^ (sl2 |> list) ^ ")"
    | Cnone -> "Cnone"
    | Cjsx (sl1, s, sl2) ->
      "Cjsx(" ^ (sl1 |> list) ^ ", " ^ str s ^ ", " ^ (sl2 |> list) ^ ")"
end
