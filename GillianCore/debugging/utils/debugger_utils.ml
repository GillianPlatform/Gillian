(** @canonical Gillian.Debugger.Utils *)

(** Converts a GIL location (column number starts from 0) to a VSCode location (column number starts from 1) *)
let location_to_display_location
    ({ loc_start; loc_end; loc_source } : Location.t) : Location.t =
  let loc_start = { loc_start with pos_column = loc_start.pos_column + 1 } in
  let loc_end = { loc_end with pos_column = loc_end.pos_column + 1 } in
  { loc_start; loc_end; loc_source }

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
type exception_info = { id : string; description : string option }

module Variable = struct
  (** @inline *)
  include Variable
end

module Exec_map = struct
  (** @inline *)
  include Exec_map
end

module Unify_map = struct
  (** @inline *)
  include Unify_map
end
