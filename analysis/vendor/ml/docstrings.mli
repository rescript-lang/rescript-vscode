(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                               Leo White                                *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Documentation comments *)

val init : unit -> unit
(** (Re)Initialise all docstring state *)

val warn_bad_docstrings : unit -> unit
(** Emit warnings for unattached and ambiguous docstrings *)

(** {2 Docstrings} *)

type docstring
(** Documentation comments *)

val docstring : string -> Location.t -> docstring
(** Create a docstring *)

val register : docstring -> unit
(** Register a docstring *)

val docstring_body : docstring -> string
(** Get the text of a docstring *)

val docstring_loc : docstring -> Location.t
(** Get the location of a docstring *)

(** {2 Set functions}

   These functions are used by the lexer to associate docstrings to
   the locations of tokens. *)

val set_pre_docstrings : Lexing.position -> docstring list -> unit
(** Docstrings immediately preceding a token *)

val set_post_docstrings : Lexing.position -> docstring list -> unit
(** Docstrings immediately following a token *)

val set_floating_docstrings : Lexing.position -> docstring list -> unit
(** Docstrings not immediately adjacent to a token *)

val set_pre_extra_docstrings : Lexing.position -> docstring list -> unit
(** Docstrings immediately following the token which precedes this one *)

val set_post_extra_docstrings : Lexing.position -> docstring list -> unit
(** Docstrings immediately preceding the token which follows this one *)

(** {2 Items}

    The {!docs} type represents documentation attached to an item. *)

type docs = {docs_pre: docstring option; docs_post: docstring option}

val empty_docs : docs

val docs_attr : docstring -> Parsetree.attribute

val add_docs_attrs : docs -> Parsetree.attributes -> Parsetree.attributes
(** Convert item documentation to attributes and add them to an
    attribute list *)

val symbol_docs : unit -> docs
(** Fetch the item documentation for the current symbol. This also
    marks this documentation (for ambiguity warnings). *)

val symbol_docs_lazy : unit -> docs Lazy.t

val rhs_docs : int -> int -> docs
(** Fetch the item documentation for the symbols between two
    positions. This also marks this documentation (for ambiguity
    warnings). *)

val rhs_docs_lazy : int -> int -> docs Lazy.t

val mark_symbol_docs : unit -> unit
(** Mark the item documentation for the current symbol (for ambiguity
    warnings). *)

val mark_rhs_docs : int -> int -> unit
(** Mark as associated the item documentation for the symbols between
    two positions (for ambiguity warnings) *)

(** {2 Fields and constructors}

    The {!info} type represents documentation attached to a field or
    constructor. *)

type info = docstring option

val empty_info : info

val info_attr : docstring -> Parsetree.attribute

val add_info_attrs : info -> Parsetree.attributes -> Parsetree.attributes
(** Convert field info to attributes and add them to an
    attribute list *)

val symbol_info : unit -> info
(** Fetch the field info for the current symbol. *)

val rhs_info : int -> info
(** Fetch the field info following the symbol at a given position. *)

(** {2 Unattached comments}

    The {!text} type represents documentation which is not attached to
    anything. *)

type text = docstring list

val empty_text : text
val empty_text_lazy : text Lazy.t

val text_attr : docstring -> Parsetree.attribute

val add_text_attrs : text -> Parsetree.attributes -> Parsetree.attributes
(** Convert text to attributes and add them to an attribute list *)

val symbol_text : unit -> text
(** Fetch the text preceding the current symbol. *)

val symbol_text_lazy : unit -> text Lazy.t

val rhs_text : int -> text
(** Fetch the text preceding the symbol at the given position. *)

val rhs_text_lazy : int -> text Lazy.t

(** {2 Extra text}

    There may be additional text attached to the delimiters of a block
    (e.g. [struct] and [end]). This is fetched by the following
    functions, which are applied to the contents of the block rather
    than the delimiters. *)

val symbol_pre_extra_text : unit -> text
(** Fetch additional text preceding the current symbol *)

val symbol_post_extra_text : unit -> text
(** Fetch additional text following the current symbol *)

val rhs_pre_extra_text : int -> text
(** Fetch additional text preceding the symbol at the given position *)

val rhs_post_extra_text : int -> text
(** Fetch additional text following the symbol at the given position *)
