module type S = sig
  type t [@@deriving yojson]

  val from_loc : ?origin_loc:Location.t -> unit -> t
  val get_origin_loc : t -> Location.t option
  val get_loop_info : t -> string list
  val is_hidden : t -> bool
end

module Default = struct
  type t = Location.t option [@@deriving yojson]

  let from_loc ?origin_loc () : t = origin_loc
  let get_origin_loc t = t
  let get_loop_info (_ : t) = []
  let is_hidden (_ : t) = false
end
