(* Copyright (C) 2021- Hongbo Zhang, Authors of ReScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

val at_bol : Lexing.lexbuf -> bool

val interpret_directive :
  Lexing.lexbuf ->
  cont:(Lexing.lexbuf -> Parser.token) ->
  token_with_comments:(Lexing.lexbuf -> Parser.token) ->
  Parser.token

val eof_check : Lexing.lexbuf -> unit

val init : unit -> unit

val check_sharp_look_ahead : (unit -> Parser.token) -> Parser.token

(* Methods below are used for cpp, they are not needed by the compiler patches*)
val remove_directive_built_in_value : string -> unit

val replace_directive_string : string -> string -> unit

val replace_directive_bool : string -> bool -> unit

val define_key_value : string -> string -> bool
(** @return false means failed to define *)

val list_variables : Format.formatter -> unit

val filter_directive_from_lexbuf :
  Lexing.lexbuf ->
  token_with_comments:(Lexing.lexbuf -> Parser.token) ->
  (int * int) list
