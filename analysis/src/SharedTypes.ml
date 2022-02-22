type modulePath =
  | File of Uri2.t * string
  | NotVisible
  | IncludedModule of Path.t * modulePath
  | ExportedModule of string * modulePath

type field = {stamp : int; fname : string Location.loc; typ : Types.type_expr}

module Constructor = struct
  type t = {
    stamp : int;
    cname : string Location.loc;
    args : (Types.type_expr * Location.t) list;
    res : Types.type_expr option;
  }
end

module Type = struct
  type kind =
    | Abstract of (Path.t * Types.type_expr list) option
    | Open
    | Tuple of Types.type_expr list
    | Record of field list
    | Variant of Constructor.t list

  type t = {kind : kind; decl : Types.type_declaration}
end

module Exported = struct
  type namedStampMap = (string, int) Hashtbl.t

  type t = {
    types : namedStampMap;
    values : namedStampMap;
    modules : namedStampMap;
  }

  let init () =
    {
      types = Hashtbl.create 10;
      values = Hashtbl.create 10;
      modules = Hashtbl.create 10;
    }
end

module Declared = struct
  type 'item t = {
    name : string Location.loc;
    extentLoc : Location.t;
    scopeLoc : Location.t;
    stamp : int;
    modulePath : modulePath;
    isExported : bool;
    deprecated : string option;
    docstring : string list;
    item : 'item;
  }

  let empty name =
    {
      name = Location.mknoloc name;
      extentLoc = Location.none;
      scopeLoc = Location.none;
      stamp = 0;
      modulePath = NotVisible;
      isExported = false;
      deprecated = None;
      docstring = [];
      item = ();
    }
end

module ModuleKind = struct
  type moduleItem =
    | Value of Types.type_expr
    | Type of Type.t * Types.rec_status
    | Module of t

  and contents = {
    docstring : string list;
    exported : Exported.t;
    topLevel : moduleItem Declared.t list;
  }

  and t = Ident of Path.t | Structure of contents | Constraint of t * t
end

module Kind = struct
  type t =
    | Module of ModuleKind.t
    | Value of Types.type_expr
    | Type of Type.t
    | Constructor of Constructor.t * Type.t Declared.t
    | Field of field * Type.t Declared.t
    | FileModule of string

  let toInt kind =
    match kind with
    | Module _ -> 9
    | FileModule _ -> 9
    | Constructor (_, _) -> 4
    | Field (_, _) -> 5
    | Type _ -> 22
    | Value _ -> 12
end

module Stamps = struct
  type 't stampMap = (int, 't Declared.t) Hashtbl.t

  type t = {
    types : Type.t stampMap;
    values : Types.type_expr stampMap;
    modules : ModuleKind.t stampMap;
    constructors : Constructor.t stampMap;
  }

  let init () =
    {
      types = Hashtbl.create 10;
      values = Hashtbl.create 10;
      modules = Hashtbl.create 10;
      constructors = Hashtbl.create 10;
    }
end

module Env = struct
  type t = {stamps : Stamps.t; modulePath : modulePath; scope : Location.t}
end

module File = struct
  type t = {
    uri : Uri2.t;
    stamps : Stamps.t;
    moduleName : string;
    contents : ModuleKind.contents;
  }

  let create moduleName uri =
    {
      uri;
      stamps = Stamps.init ();
      moduleName;
      contents = {docstring = []; exported = Exported.init (); topLevel = []};
    }
end

module QueryEnv = struct
  type t = {file : File.t; exported : Exported.t}

  let fromFile file = {file; exported = file.contents.exported}
end

type filePath = string

type paths =
  | Impl of {cmt : filePath; res : filePath}
  | Namespace of {cmt : filePath}
  | IntfAndImpl of {
      cmti : filePath;
      resi : filePath;
      cmt : filePath;
      res : filePath;
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
  | Impl {res} -> Uri2.fromPath res
  | Namespace {cmt} -> Uri2.fromPath cmt
  | IntfAndImpl {resi} -> Uri2.fromPath resi

let getCmtPath ~uri p =
  match p with
  | Impl {cmt} -> cmt
  | Namespace {cmt} -> cmt
  | IntfAndImpl {cmti; cmt} ->
    let interface = Utils.endsWith (Uri2.toPath uri) "i" in
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

type locItem = {loc : Location.t; locType : locType}

module LocationSet = Set.Make (struct
  include Location

  let compare loc1 loc2 = compare loc2 loc1

  (* polymorphic compare should be OK *)
end)

type extra = {
  internalReferences : (int, Location.t list) Hashtbl.t;
  externalReferences :
    (string, (string list * Tip.t * Location.t) list) Hashtbl.t;
  fileReferences : (string, LocationSet.t) Hashtbl.t;
  mutable locItems : locItem list;
  (* This is the "open location", like the location...
     or maybe the >> location of the open ident maybe *)
  (* OPTIMIZE: using a stack to come up with this would cut the computation time of this considerably. *)
  opens : (Location.t, unit) Hashtbl.t;
}

type file = string

module FileSet = Set.Make (String)

type package = {
  rootPath : filePath;
  projectFiles : FileSet.t;
  dependenciesFiles : FileSet.t;
  pathsForModule : (file, paths) Hashtbl.t;
  namespace : string option;
  opens : string list;
}

type full = {extra : extra; file : File.t; package : package}

let initExtra () =
  {
    internalReferences = Hashtbl.create 10;
    externalReferences = Hashtbl.create 10;
    fileReferences = Hashtbl.create 10;
    locItems = [];
    opens = Hashtbl.create 10;
  }

type state = {
  packagesByRoot : (string, package) Hashtbl.t;
  rootForUri : (Uri2.t, string) Hashtbl.t;
  cmtCache : (filePath, float * File.t) Hashtbl.t;
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

module SymbolKind = struct
  type t =
    | Module
    | Enum
    | Interface
    | Function
    | Variable
    | Array
    | Object
    | Null
    | EnumMember
    | TypeParameter
end

let rec variableKind t =
  match t.Types.desc with
  | Tlink t -> variableKind t
  | Tsubst t -> variableKind t
  | Tarrow _ -> SymbolKind.Function
  | Ttuple _ -> Array
  | Tconstr _ -> Variable
  | Tobject _ -> Object
  | Tnil -> Null
  | Tvariant _ -> EnumMember
  | Tpoly _ -> EnumMember
  | Tpackage _ -> Module
  | _ -> Variable

let symbolKind = function
  | SymbolKind.Module -> 2
  | Enum -> 10
  | Interface -> 11
  | Function -> 12
  | Variable -> 13
  | Array -> 18
  | Object -> 19
  | Null -> 21
  | EnumMember -> 22
  | TypeParameter -> 26

let declarationKind t =
  match t.Types.type_kind with
  | Type_open | Type_abstract -> SymbolKind.TypeParameter
  | Type_record _ -> Interface
  | Type_variant _ -> Enum
