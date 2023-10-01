type nest_kind =
  | NoNest  (** This command doesn't contain a nest *)
  | LoopBody of string
      (** This command nests its loop body an (abstracted) function call *)
  | FunCall of string  (** This command nests the body of a function call *)
[@@deriving yojson]

type stmt_end_kind = NotEnd | EndNormal | EndWithBranch of WBranchCase.t
[@@deriving yojson]

(** How does this command map to a WISL statment? *)
type stmt_kind =
  | Single  (** A command that maps one-to-one with a WISL statement *)
  | LoopPrefix  (** A command in the prefix of a loop body function *)
  | Multi of stmt_end_kind
      (** A command that makes up part of a WISL statement, and whether this is the last cmd of said statement *)
[@@deriving yojson]

type t = {
  origin_loc : Gil_syntax.Location.t option;
      (** Better not to know what this is for *)
  origin_id : int option;  (** Origin Id, that should be abstracted away *)
  loop_info : string list;
  stmt_kind : stmt_kind; [@default Single]
  nest_kind : nest_kind; [@default NoNest]
  is_hidden : bool; [@default false]
      (** Should this command be hidden when debugging? *)
  is_return : bool; [@default false]
}
[@@deriving yojson, make]

let make_basic ?origin_loc ?loop_info () = make ?origin_loc ?loop_info ()
let get_origin_loc { origin_loc; _ } = origin_loc
let get_loop_info { loop_info; _ } = loop_info
let set_loop_info loop_info annot = { annot with loop_info }
let is_hidden { is_hidden; _ } = is_hidden
