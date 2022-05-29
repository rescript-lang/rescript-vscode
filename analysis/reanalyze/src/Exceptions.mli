type t
val add : Exn.t -> t -> t
val diff : t -> t -> t
val empty : t
val isEmpty : t -> bool
val iter : (Exn.t -> unit) -> t -> unit
val fromList : Exn.t list -> t
val pp :
  exnTable:(Exn.t, Common.LocSet.t) Hashtbl.t option ->
    Format.formatter -> t -> unit
val toList : t -> Exn.t list
val union : t -> t -> t