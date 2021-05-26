type visibilityPath =
  | File of Uri2.t * string
  | NotVisible
  | IncludedModule of Path.t * visibilityPath
  | ExportedModule of string * visibilityPath

type 't declared = {
  name : string Location.loc;
  extentLoc : Location.t;
  scopeLoc : Location.t;
  stamp : int;
  modulePath : visibilityPath;
  isExported : bool;
  deprecated : string option;
  docstring : string list;
  item : 't;
}

type 't stampMap = (int, 't) Hashtbl.t

type field = {stamp : int; fname : string Location.loc; typ : Types.type_expr}

type constructor = {
  stamp : int;
  cname : string Location.loc;
  args : (Types.type_expr * Location.t) list;
  res : Types.type_expr option;
}

module Type = struct
  type kind =
    | Abstract of (Path.t * Types.type_expr list) option
    | Open
    | Tuple of Types.type_expr list
    | Record of field list
    | Variant of constructor list

  type t = {kind : kind; decl : Types.type_declaration}
end

type 't namedMap = (string, 't) Hashtbl.t

type namedStampMap = int namedMap

type exported = {
  types : namedStampMap;
  values : namedStampMap;
  modules : namedStampMap;
}

let initExported () =
  {
    types = Hashtbl.create 10;
    values = Hashtbl.create 10;
    modules = Hashtbl.create 10;
  }

type moduleItem =
  | MValue of Types.type_expr
  | MType of Type.t * Types.rec_status
  | Module of moduleKind

and moduleContents = {
  docstring : string list;
  exported : exported;
  topLevel : moduleItem declared list;
}

and moduleKind =
  | Ident of Path.t
  | Structure of moduleContents
  | Constraint of moduleKind * moduleKind

type stamps = {
  types : Type.t declared stampMap;
  values : Types.type_expr declared stampMap;
  modules : moduleKind declared stampMap;
  constructors : constructor declared stampMap;
}

let initStamps () =
  {
    types = Hashtbl.create 10;
    values = Hashtbl.create 10;
    modules = Hashtbl.create 10;
    constructors = Hashtbl.create 10;
  }

type env = {stamps : stamps; modulePath : visibilityPath; scope : Location.t}

module File = struct
  type t = {
    uri : Uri2.t;
    stamps : stamps;
    moduleName : string;
    contents : moduleContents;
  }

  let create moduleName uri =
    {
      uri;
      stamps = initStamps ();
      moduleName;
      contents = {docstring = []; exported = initExported (); topLevel = []};
    }
end

module QueryEnv = struct
  type t = {file : File.t; exported : exported}

  let fromFile file = {file; exported = file.contents.exported}
end

type filePath = string

type paths =
  | Impl of filePath * filePath option
  | Intf of filePath * filePath
  (* .cm(t)i, .mli, .cmt, .rei *)
  | IntfAndImpl of filePath * filePath * filePath * filePath

open Infix

let showPaths paths =
  match paths with
  | Impl (cmt, src) -> Printf.sprintf "Impl(%s, %s)" cmt (src |? "nil")
  | Intf (cmti, src) -> Printf.sprintf "Intf(%s, %s)" cmti src
  | IntfAndImpl (cmti, srci, cmt, src) ->
    Printf.sprintf "IntfAndImpl(%s, %s, %s, %s)" cmti srci cmt src

let getSrc p =
  match p with
  | Impl (_, s) -> s
  | Intf (_, s) | IntfAndImpl (_, s, _, _) -> Some s

let getCmt ?(interface = true) p =
  match p with
  | Impl (c, _) | Intf (c, _) -> c
  | IntfAndImpl (cint, _, cimpl, _) -> if interface then cint else cimpl

let emptyDeclared name =
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

type tip = Value | Type | Field of string | Constructor of string | Module

let tipToString tip =
  match tip with
  | Value -> "Value"
  | Type -> "Type"
  | Field f -> "Field(" ^ f ^ ")"
  | Constructor a -> "Constructor(" ^ a ^ ")"
  | Module -> "Module"

type path = Tip of string | Nested of string * path

let rec pathToString path =
  match path with
  | Tip name -> name
  | Nested (name, inner) -> name ^ "." ^ pathToString inner

type locKind =
  | LocalReference of int * tip
  | GlobalReference of string * path * tip
  | NotFound
  | Definition of int * tip

type locType =
  | Typed of string * Types.type_expr * locKind
  | Constant of Asttypes.constant
  | LModule of locKind
  | TopLevelModule of string
  | TypeDefinition of string * Types.type_declaration * int

type locItem = {loc : Location.t; locType : locType}

type openTracker = {
  path : Path.t;
  loc : Location.t;
  extent : Location.t;
  mutable used : (path * tip * Location.t) list;
}

type extra = {
  internalReferences : (int, Location.t list) Hashtbl.t;
  externalReferences : (string, (path * tip * Location.t) list) Hashtbl.t;
  fileReferences : (string, Location.t list) Hashtbl.t;
  mutable locItems : locItem list;
  (* This is the "open location", like the location...
     or maybe the >> location of the open ident maybe *)
  (* OPTIMIZE: using a stack to come up with this would cut the computation time of this considerably. *)
  opens : (Location.t, openTracker) Hashtbl.t;
}

type file = string

type package = {
  rootPath : filePath;
  projectFiles : file list;
  dependenciesFiles : file list;
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
  | LocalReference (_, tip) -> "(LocalReference " ^ tipToString tip ^ ")"
  | GlobalReference _ -> "GlobalReference"
  | NotFound -> "NotFound"
  | Definition (_, tip) -> "(Definition " ^ tipToString tip ^ ")"

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

type kinds =
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

let rec variableKind t =
  match t.Types.desc with
  | Tlink t -> variableKind t
  | Tsubst t -> variableKind t
  | Tarrow _ -> Function
  | Ttuple _ -> Array
  | Tconstr _ -> Variable
  | Tobject _ -> Object
  | Tnil -> Null
  | Tvariant _ -> EnumMember
  | Tpoly _ -> EnumMember
  | Tpackage _ -> Module
  | _ -> Variable

let symbolKind = function
  | Module -> 2
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
  | Type_open | Type_abstract -> TypeParameter
  | Type_record _ -> Interface
  | Type_variant _ -> Enum
