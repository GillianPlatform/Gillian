(** {b GIL annot}. *)
type t

val init : ?line_offset:int option -> ?origin_id:int -> unit -> t
(** Initialize an annotation *)

val get_line_offset : t -> int option
(** get the line offset *)

val line_info_to_str : (string * int * int) list -> string
