module Types = struct
  type match_result = Match_map.match_result = Success | Failure
  [@@deriving yojson]

  type ('err, 'annot, 'ast) memory_error_info = {
    error : 'err;  (** The memory error that needs to be lifted *)
    command : (int Cmd.t * 'annot) option;  (** The command where it happened *)
    tl_ast : 'ast option;
        (** If the program was compiled from the target language, we keep the tl ast around *)
  }

  type 'cmd_report executed_cmd_data = {
    kind : (Branch_case.t, unit) Exec_map.cmd_kind;
    id : Logging.Report_id.t;
    cmd_report : 'cmd_report;
    matches : Exec_map.matching list;
    errors : string list;
    branch_path : Branch_case.path;
  }
  [@@deriving yojson]

  type handle_cmd_result =
    | Stop of Logging.Report_id.t option
    | ExecNext of (Logging.Report_id.t option * Branch_case.t option)
  [@@deriving yojson]
end

include Types

module type S = sig
  (** The lifter's state *)
  type t

  type memory_error
  type tl_ast
  type memory
  type cmd_report
  type annot

  (** Given a proc name, a tl_ast, and the data from the first executed GIL
    command, initialise the lifter's state and handle the first command.

    Returns [None] if lifting is unsupported (i.e. if [tl_ast] is [None]). *)
  val init :
    proc_name:string ->
    all_procs:string list ->
    tl_ast option ->
    (annot, int) Prog.t ->
    cmd_report executed_cmd_data ->
    (t * handle_cmd_result) option

  (** Exception-raising version of {!init}. *)
  val init_exn :
    proc_name:string ->
    all_procs:string list ->
    tl_ast option ->
    (annot, int) Prog.t ->
    cmd_report executed_cmd_data ->
    t * handle_cmd_result

  (** Gives a JSON representation of the lifter's state.

    Used for debugging problems with the lifter.*)
  val dump : t -> Yojson.Safe.t

  (** Handles the execution result of a GIL command. *)
  val handle_cmd :
    Logging.Report_id.t ->
    Branch_case.t option ->
    cmd_report executed_cmd_data ->
    t ->
    handle_cmd_result

  (** Gets the non-lifted execution map of GIL commands.

    In most cases, it's recommended to use a {!Gil_fallback_lifter}, and just
    defer this call to the GIL lifter. *)
  val get_gil_map : t -> Exec_map.Packaged.t

  (** Gets the lifted execution map.

    Returns [None] if lifting is not supported. *)
  val get_lifted_map : t -> Exec_map.Packaged.t option

  (** Exception-raising version of {!get_lifted_map}. *)
  val get_lifted_map_exn : t -> Exec_map.Packaged.t

  (** Gives a list of matches that occurred at the specified command. *)
  val get_matches_at_id : Logging.Report_id.t -> t -> Exec_map.matching list

  (** Gives the id of the root (first) command in the execution map. *)
  val get_root_id : t -> Logging.Report_id.t option

  (** Gives the path of (GIL) branch cases that lead to the specified command. *)
  val path_of_id : Logging.Report_id.t -> t -> Branch_case.path

  (** Gets a list of ID/branch-case pairs that correspond to any commands that
    directly succeed the specified command. *)
  val existing_next_steps :
    Logging.Report_id.t ->
    t ->
    (Logging.Report_id.t * Branch_case.t option) list

  (** Translates a command ID and (packaged) TL branch case to the command ID
    and GIL branch case necessary to step forward. *)
  val next_gil_step :
    Logging.Report_id.t ->
    Exec_map.Packaged.branch_case option ->
    t ->
    Logging.Report_id.t * Branch_case.t option

  (** If the given command has a previous command, gives its ID, and
    the TL branch case that connects them (if applicable). *)
  val previous_step :
    Logging.Report_id.t ->
    t ->
    (Logging.Report_id.t * Exec_map.Packaged.branch_case option) option

  (** Gives a branch path that steps forward from the given command.
  
    If the given command branches, the desired branch case can be specified. *)
  val select_next_path :
    Branch_case.t option -> Logging.Report_id.t -> t -> Branch_case.path

  (** Gives a branch path under the specified command (or the root command, if
    not specified) that hasn't yet been fully explored.

    The order in which unfinished branches are given in later calls is
    unimportant.
    
    Returns [None] if all paths under the specified command have been fully
    explored. *)
  val find_unfinished_path :
    ?at_id:Logging.Report_id.t ->
    t ->
    (Logging.Report_id.t * Branch_case.t option) option

  val memory_error_to_exception_info :
    (memory_error, annot, tl_ast) memory_error_info -> exception_info

  val add_variables :
    store:(string * Expr.t) list ->
    memory:memory ->
    is_gil_file:bool ->
    get_new_scope_id:(unit -> int) ->
    Variable.ts ->
    Variable.scope list
end

module type Intf = sig
  (** @inline *)
  include module type of struct
    (** @inline *)
    include Types
  end

  module type S = S

  val make_executed_cmd_data :
    (Branch_case.t, unit) Exec_map.cmd_kind ->
    Logging.Report_id.t ->
    'cmd_report ->
    ?matches:Exec_map.matching list ->
    ?errors:string list ->
    Branch_case.path ->
    'cmd_report executed_cmd_data
end
