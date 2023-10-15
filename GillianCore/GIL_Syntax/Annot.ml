module type S = sig
  type t [@@deriving yojson]

  val make_basic : ?origin_loc:Location.t -> ?loop_info:string list -> unit -> t
  val get_origin_loc : t -> Location.t option
  val get_loop_info : t -> string list
  val set_loop_info : string list -> t -> t
  val is_hidden : t -> bool
end

module Basic = struct
  type t = { origin_loc : Location.t option; loop_info : string list }
  [@@deriving yojson, make, eq]

  let make_basic = make
  let get_origin_loc { origin_loc; _ } = origin_loc
  let get_loop_info { loop_info; _ } = loop_info
  let set_loop_info loop_info annot = { annot with loop_info }
  let is_hidden (_ : t) = false
end
