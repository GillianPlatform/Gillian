module type S = sig
  type t [@@deriving yojson]

  val make_basic : ?origin_loc:Location.t -> ?loop_info:string list -> unit -> t
  val get_origin_loc : t -> Location.t option
  val get_loop_info : t -> string list
  val set_loop_info : string list -> t -> t
  val is_hidden : t -> bool
end

module Basic : sig
  include S

  val equal : t -> t -> bool
  val make : ?origin_loc:Location.t -> ?loop_info:string list -> unit -> t
end
