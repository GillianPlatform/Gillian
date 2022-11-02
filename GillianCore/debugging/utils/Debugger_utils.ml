(* VSCode column numbers start from 1 while GIL location columns start from 0 *)
let location_to_display_location (loc : Location.t) : Location.t =
  let loc_source = loc.loc_source in
  let loc_start : Location.position =
    {
      pos_line = loc.loc_start.pos_line;
      pos_column = loc.loc_start.pos_column + 1;
    }
  in
  let loc_end : Location.position =
    { pos_line = loc.loc_end.pos_line; pos_column = loc.loc_end.pos_column + 1 }
  in
  { loc_start; loc_end; loc_source }

(* TODO: these types should be categorised and put in separate files. *)

type stop_reason =
  | Step
  | ReachedStart
  | ReachedEnd
  | Breakpoint
  | ExecutionError
[@@deriving yojson]

type frame = {
  index : int;
  name : string;
  source_path : string;
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
}

(* TODO: this should contain a variable list *)
type scope = { name : string; id : int }

type variable = {
  name : string;
  value : string;
  type_ : string option;
  var_ref : int;
}

type variables = (int, variable list) Hashtbl.t
type exception_info = { id : string; description : string option }

let create_leaf_variable (name : string) (value : string) ?(type_ = None) () :
    variable =
  { name; value; type_; var_ref = 0 }

let create_node_variable (name : string) (var_ref : int) ?(value = "") () :
    variable =
  { name; value; type_ = Some "object"; var_ref }

module ExecMap = ExecMap
module UnifyMap = UnifyMap
