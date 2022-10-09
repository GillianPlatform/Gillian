(** {b GIL annot}. *)
type expansion_kind = NoExpansion | Function of string [@@deriving yojson]

type t = {
  origin_loc : Location.t option;  (** Better not to know what this is for *)
  origin_id : int option;  (** Origin Id, that should be abstracted away *)
  loop_info : string list; [@default []]
  expansion_kind : expansion_kind; [@default NoExpansion]
      (** Should this command be expanded when lifting? (i.e. loops to functions in WISL) *)
}
[@@deriving yojson, make]

let get_loop_info (annot : t) = annot.loop_info

let set_loop_info (annot : t) (loop_info : string list) =
  { annot with loop_info }

let get_origin_loc annot = annot.origin_loc
let get_origin_id annot = annot.origin_id
let is_hidden (annot : t) = Option.is_none annot.origin_id
let hide (annot : t) = { annot with origin_id = None }
let get_expansion_kind (annot : t) = annot.expansion_kind

let set_expansion_kind expansion_kind (annot : t) =
  { annot with expansion_kind }
