type rid = Logging.ReportId.t

type unify_result = UnifyMap.unify_result = Success | Failure
[@@deriving yojson]

type ('err, 'ast) memory_error_info = {
  error : 'err;  (** The memory error that needs to be lifted *)
  command : (int Cmd.t * Annot.t) option;  (** The command where it happened *)
  tl_ast : 'ast option;
      (** If the program was compiled from the target language, we keep the tl ast around *)
}

type 'cmd_report executed_cmd_data = {
  kind : (BranchCase.t, unit) ExecMap.cmd_kind;
  id : rid;
  cmd_report : 'cmd_report;
  unifys : ExecMap.unifys;
  errors : string list;
  branch_path : BranchCase.path;
}
[@@deriving yojson]

val make_executed_cmd_data :
  (BranchCase.t, unit) ExecMap.cmd_kind ->
  rid ->
  'cmd_report ->
  ?unifys:ExecMap.unifys ->
  ?errors:string list ->
  BranchCase.path ->
  'cmd_report executed_cmd_data

type handle_cmd_result =
  | Stop
  | ExecNext of (rid option * BranchCase.t option)
  | StartProc of string
[@@deriving yojson]

module type S = sig
  type t
  type memory_error
  type tl_ast
  type memory
  type cmd_report

  val init :
    tl_ast option -> cmd_report executed_cmd_data -> t * handle_cmd_result

  val init_opt :
    tl_ast option ->
    cmd_report executed_cmd_data ->
    (t * handle_cmd_result) option

  val dump : t -> Yojson.Safe.t

  val handle_cmd :
    rid ->
    BranchCase.t option ->
    cmd_report executed_cmd_data ->
    t ->
    handle_cmd_result

  val get_gil_map : t -> ExecMap.Packaged.t
  val get_lifted_map_opt : t -> ExecMap.Packaged.t option
  val get_lifted_map : t -> ExecMap.Packaged.t
  val get_unifys_at_id : rid -> t -> ExecMap.unifys
  val get_root_id : t -> rid option
  val path_of_id : rid -> t -> BranchCase.path
  val existing_next_steps : rid -> t -> (rid * BranchCase.t option) list

  val next_step_specific :
    rid -> ExecMap.Packaged.branch_case option -> t -> rid * BranchCase.t option

  val previous_step :
    rid -> t -> (rid * ExecMap.Packaged.branch_case option) option

  val select_next_path : BranchCase.t option -> rid -> t -> BranchCase.path

  val find_unfinished_path :
    ?at_id:rid -> t -> (rid * BranchCase.t option) option

  val memory_error_to_exception_info :
    (memory_error, tl_ast) memory_error_info -> exception_info

  val add_variables :
    store:(string * Expr.t) list ->
    memory:memory ->
    is_gil_file:bool ->
    get_new_scope_id:(unit -> int) ->
    variables ->
    scope list
end
