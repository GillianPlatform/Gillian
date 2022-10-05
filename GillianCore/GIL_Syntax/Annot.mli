(** {b GIL annot}. *)
type expansion_kind = NoExpansion | Function of string [@@deriving yojson]

type t [@@deriving yojson]

(** Initialize an annotation *)
val make :
  ?origin_loc:Location.t ->
  ?origin_id:int ->
  ?loop_info:string list ->
  ?lift_hidden:bool ->
  ?expansion_kind:expansion_kind ->
  unit ->
  t

(** Get the loop info *)
val get_loop_info : t -> string list

(** Set the loop info *)
val set_loop_info : t -> string list -> t

(** Get the origin location *)
val get_origin_loc : t -> Location.t option

(* Get the origin id *)
val get_origin_id : t -> int option
val is_hidden : t -> bool
val hide : t -> t
val get_expansion_kind : t -> expansion_kind
val set_expansion_kind : expansion_kind -> t -> t
