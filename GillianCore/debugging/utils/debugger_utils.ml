(** @canonical Gillian.Debugger.Utils

  Miscellaneous types and functions for debugger-related purposes *)

(** Converts a GIL location (column number starts from 0) to a VSCode location (column number starts from 1) *)
let location_to_display_location
    ({ loc_start; loc_end; loc_source } : Location.t) : Location.t =
  let loc_start = { loc_start with pos_column = loc_start.pos_column + 1 } in
  let loc_end = { loc_end with pos_column = loc_end.pos_column + 1 } in
  { loc_start; loc_end; loc_source }

(** Explains why the debugger has stopped *)
type stop_reason =
  | Step  (** The step has been completed *)
  | ReachedStart
      (** The start of the program has been reached (when executing backwards) *)
  | ReachedEnd  (** The end of the program has been reached *)
  | Breakpoint  (** A breakpoint has been reached *)
  | ExecutionError  (** An error in execution has occurred *)
[@@deriving yojson]

(** Describes a frame on the stack of execution *)
type frame = {
  index : int;  (** The index of the frame in the stack *)
  name : string;  (** The name of the frame *)
  source_path : string;  (** The path to the source file of the relevant code *)
  start_line : int;  (** The line number of the start of the relevant code *)
  start_column : int;
      (** The column number of the start of the relevant code *)
  end_line : int;  (** The line number of the end of the relevant code *)
  end_column : int;  (** The column number of the end of the relevant code *)
}
[@@deriving make]

type 'memory astate = {
  store : (string * Expr.t) list;
  memory : 'memory;
  pfs : PFS.t option;
  types : Type_env.t option;
  preds : Preds.t option;
}
[@@deriving make]

let proc_id_prefix = "proc__"
let proc_id_prefix_len = String.length proc_id_prefix

let proc_name_of_id s =
  if String.starts_with ~prefix:proc_id_prefix s then
    let proc_name =
      String.sub s proc_id_prefix_len (String.length s - proc_id_prefix_len)
    in
    Some proc_name
  else None

(** Describes an exception *)
type exception_info = { id : string; description : string option }

module Variable = struct
  (** @inline *)
  include Variable
end

module Exec_map = struct
  (** @inline *)
  include Exec_map
end

module Match_map = struct
  (** @inline *)
  include Match_map
end
