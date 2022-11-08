type expansion_kind = NoExpansion | Proc of string [@@deriving yojson]

type t = {
  origin_loc : Gil_syntax.Location.t option;
      (** Better not to know what this is for *)
  origin_id : int option;  (** Origin Id, that should be abstracted away *)
  loop_info : string list;
  expansion_kind : expansion_kind; [@default NoExpansion]
      (** Does this command expand (i.e. calling a loop body proc)? If so, how? *)
  is_hidden : bool; [@default false]
      (** Should this command be hidden when debugging? *)
  is_loop_prefix : bool; [@default false]
      (** Marks commands before the start of a loop body *)
  is_end_of_cmd : bool; [@default false]
      (** Marks the final command in a sequence of commands originating from one WISL statement *)
  is_return : bool; [@default false]
      (** Marks commands that are part of the return sequence *)
}
[@@deriving yojson, make]

let make_basic ?origin_loc ?loop_info () = make ?origin_loc ?loop_info ()
let get_origin_loc { origin_loc; _ } = origin_loc
let get_loop_info { loop_info; _ } = loop_info
let set_loop_info loop_info annot = { annot with loop_info }
let is_hidden { is_hidden; _ } = is_hidden
