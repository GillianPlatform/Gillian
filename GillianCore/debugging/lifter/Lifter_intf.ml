module Types = struct
  type unify_result = UnifyMap.unify_result = Success | Failure
  [@@deriving yojson]

  type ('err, 'annot, 'ast) memory_error_info = {
    error : 'err;  (** The memory error that needs to be lifted *)
    command : (int Cmd.t * 'annot) option;  (** The command where it happened *)
    tl_ast : 'ast option;
        (** If the program was compiled from the target language, we keep the tl ast around *)
  }

  type 'cmd_report executed_cmd_data = {
    kind : (BranchCase.t, unit) ExecMap.cmd_kind;
    id : Logging.ReportId.t;
    cmd_report : 'cmd_report;
    unifys : ExecMap.unifys;
    errors : string list;
    branch_path : BranchCase.path;
  }
  [@@deriving yojson]

  type handle_cmd_result =
    | Stop
    | ExecNext of (Logging.ReportId.t option * BranchCase.t option)
  [@@deriving yojson]
end

include Types

module type S = sig
  type t
  type memory_error
  type tl_ast
  type memory
  type cmd_report
  type annot

  val init :
    string ->
    tl_ast option ->
    cmd_report executed_cmd_data ->
    t * handle_cmd_result

  val init_opt :
    string ->
    tl_ast option ->
    cmd_report executed_cmd_data ->
    (t * handle_cmd_result) option

  val dump : t -> Yojson.Safe.t

  val handle_cmd :
    Logging.ReportId.t ->
    BranchCase.t option ->
    cmd_report executed_cmd_data ->
    t ->
    handle_cmd_result

  val get_gil_map : t -> ExecMap.Packaged.t
  val get_lifted_map_opt : t -> ExecMap.Packaged.t option
  val get_lifted_map : t -> ExecMap.Packaged.t
  val get_unifys_at_id : Logging.ReportId.t -> t -> ExecMap.unifys
  val get_root_id : t -> Logging.ReportId.t option
  val path_of_id : Logging.ReportId.t -> t -> BranchCase.path

  val existing_next_steps :
    Logging.ReportId.t -> t -> (Logging.ReportId.t * BranchCase.t option) list

  val next_step_specific :
    Logging.ReportId.t ->
    ExecMap.Packaged.branch_case option ->
    t ->
    Logging.ReportId.t * BranchCase.t option

  val previous_step :
    Logging.ReportId.t ->
    t ->
    (Logging.ReportId.t * ExecMap.Packaged.branch_case option) option

  val select_next_path :
    BranchCase.t option -> Logging.ReportId.t -> t -> BranchCase.path

  val find_unfinished_path :
    ?at_id:Logging.ReportId.t ->
    t ->
    (Logging.ReportId.t * BranchCase.t option) option

  val memory_error_to_exception_info :
    (memory_error, annot, tl_ast) memory_error_info -> exception_info

  val add_variables :
    store:(string * Expr.t) list ->
    memory:memory ->
    is_gil_file:bool ->
    get_new_scope_id:(unit -> int) ->
    Variable.ts ->
    scope list
end

module type Intf = sig
  (** @inline *)
  include module type of struct
    (** @inline *)
    include Types
  end

  module type S = S

  val make_executed_cmd_data :
    (BranchCase.t, unit) ExecMap.cmd_kind ->
    Logging.ReportId.t ->
    'cmd_report ->
    ?unifys:ExecMap.unifys ->
    ?errors:string list ->
    BranchCase.path ->
    'cmd_report executed_cmd_data
end
