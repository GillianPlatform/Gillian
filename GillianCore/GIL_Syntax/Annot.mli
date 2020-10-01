(** {b GIL annot}. *)
type t

(** Initialize an annotation *)
val init :
  ?line_offset:int option ->
  ?origin_id:int ->
  ?loop_info:string list ->
  unit ->
  t

(** Get the loop info *)
val get_loop_info : t -> string list

(** Set the loop info *)
val set_loop_info : t -> string list -> t

(** get the line offset *)
val get_line_offset : t -> int option

val line_info_to_str : (string * int * int) list -> string
