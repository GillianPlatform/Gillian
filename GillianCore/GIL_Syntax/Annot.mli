(** {b GIL annot}. *)
type t

(** Initialize an annotation *)
val make :
  ?origin_loc:Location.t ->
  ?origin_id:int ->
  ?loop_info:string list ->
  unit ->
  t

(** Get the loop info *)
val get_loop_info : t -> string list

(** Set the loop info *)
val set_loop_info : t -> string list -> t

(** get the origin location *)
val get_origin_loc : t -> Location.t option
