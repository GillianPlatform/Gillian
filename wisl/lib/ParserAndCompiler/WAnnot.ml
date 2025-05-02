type nest_kind =
  | LoopBody of string
      (** This command nests its loop body an (abstracted) function call *)
  | FunCall of string  (** This command nests the body of a function call *)
[@@deriving yojson]

(** How does this command map to a WISL statment? *)
type stmt_kind =
  | Normal of bool
      (** A command that makes up part of a WISL statement, and whether this is the last cmd of said statement *)
  | Return of bool
      (** Same as [Normal], but specific to the return statement *)
  | Hidden  (** A command that doesn't map to a particular WISL statement *)
  | LoopPrefix  (** A command in the prefix of a loop body function *)
[@@deriving yojson, show]

type t = {
  origin_loc : Utils.Location.t option;
      (** Better not to know what this is for *)
  origin_id : int option;  (** Origin Id, that should be abstracted away *)
  loop_info : string list;
  stmt_kind : stmt_kind; [@default Normal true]
  branch_kind : WBranchCase.kind option;
  nest_kind : nest_kind option;
}
[@@deriving yojson, make]

let make_multi
    ?origin_loc
    ?origin_id
    ?loop_info
    ?nest_kind
    ?(is_return = false)
    () =
  let make = make ?origin_loc ?origin_id ?loop_info ?nest_kind in
  if is_return then
    (make ~stmt_kind:(Return false) (), make ~stmt_kind:(Return true) ())
  else (make ~stmt_kind:(Normal false) (), make ~stmt_kind:(Normal true) ())

let make_basic ?origin_loc ?loop_info () =
  make ~stmt_kind:Hidden ?origin_loc ?loop_info ()

let get_origin_loc { origin_loc; _ } = origin_loc
let get_loop_info { loop_info; _ } = loop_info
let set_loop_info loop_info annot = { annot with loop_info }

let is_hidden { stmt_kind; _ } =
  match stmt_kind with
  | Hidden -> true
  | _ -> false
