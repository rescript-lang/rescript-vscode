open Lexing

type location = Warnings.loc = { loc_start: position; loc_end: position; loc_ghost: bool }

type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : location;
}

type longident = Longident.t =
    Lident of string
  | Ldot of longident * string
  | Lapply of longident * longident
let none = Location.none
let mknoloc = Location.mknoloc

type constant = Asttypes.constant =
    Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type payload = Parsetree.payload =
  | PStr of Parsetree.structure
  | PSig of Parsetree.signature (* : SIG *)
  | PTyp of Parsetree.core_type  (* : T *)
  | PPat of Parsetree.pattern * Parsetree.expression option  (* ? P  or  ? P when E *)

module Parsetree = Parsetree
module Lexing = Lexing
module Parser = Parser
module Lexer = Lexer
module Ident = Ident
module Path = Path
