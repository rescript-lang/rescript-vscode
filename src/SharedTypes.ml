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
  | IntfAndImpl (cint, _, cimpl, _) -> (
    match interface with true -> cint | false -> cimpl )

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
  exported : bool;
  deprecated : string option;
  docstring : string list;
  item : 't;
  (* TODO: maybe add a uri? *)
  (* scopeType: scope, *)
  (* scopeStart: (int, int), *)
}

let emptyDeclared name =
  {
    name = Location.mknoloc name;
    extentLoc = Location.none;
    scopeLoc = Location.none;
    stamp = 0;
    modulePath = NotVisible;
    exported = false;
    deprecated = None;
    docstring = [];
    item = ();
  }

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

(* type scope =
   | File
   | Switch
   | Module
   | Let
   | LetRec; *)

type 't namedMap = (string, 't) Hashtbl.t

type namedStampMap = int namedMap

type exported = {
  types : namedStampMap;
  values : namedStampMap;
  modules : namedStampMap;
  (* constructors: namedStampMap, *)
  (* classes: namedStampMap,
     classTypes: namedStampMap, *)
}

let initExported () =
  {
    types = Hashtbl.create 10;
    values = Hashtbl.create 10;
    modules = Hashtbl.create 10;
    (* constructors: Hashtbl.create(10), *)
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

and moduleKind = Ident of Path.t | Structure of moduleContents

type 't stampMap = (int, 't) Hashtbl.t

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

type file = {
  uri : Uri2.t;
  stamps : stamps;
  moduleName : string;
  contents : moduleContents;
}

let emptyFile moduleName uri =
  {
    uri;
    stamps = initStamps ();
    moduleName;
    contents = {docstring = []; exported = initExported (); topLevel = []};
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

type loc =
  | Typed of Types.type_expr * locKind
  | Constant of Asttypes.constant
  | LModule of locKind
  | TopLevelModule of string
  | TypeDefinition of string * Types.type_declaration * int
  | Explanation of string

type openTracker = {
  path : Path.t;
  loc : Location.t;
  extent : Location.t;
  mutable used : (path * tip * Location.t) list;
}

(** These are the bits of info that we need to make in-app stuff awesome *)
type extra = {
  internalReferences : (int, Location.t list) Hashtbl.t;
  externalReferences : (string, (path * tip * Location.t) list) Hashtbl.t;
  mutable locations : (Location.t * loc) list;
  (* This is the "open location", like the location...
     or maybe the >> location of the open ident maybe *)
  (* OPTIMIZE: using a stack to come up with this would cut the computation time of this considerably. *)
  opens : (Location.t, openTracker) Hashtbl.t;
}
[@@ocaml.doc
  " These are the bits of info that we need to make in-app stuff awesome "]

type full = {extra : extra; file : file}

let initExtra () =
  {
    internalReferences = Hashtbl.create 10;
    externalReferences = Hashtbl.create 10;
    locations = [];
    opens = Hashtbl.create 10;
  }

let hashList h = Hashtbl.fold (fun a b c -> (a, b) :: c) h []
