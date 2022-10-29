module L = Logging

type rid = L.ReportId.t [@@deriving yojson]

type unify_result = UnifyMap.unify_result = Success | Failure
[@@deriving yojson]

type ('err, 'ast) memory_error_info = {
  error : 'err;  (** The memory error that needs to be lifted *)
  command : (int Cmd.t * Annot.t) option;  (** The command where it happened *)
  tl_ast : 'ast option;
}

type 'cmd_report executed_cmd_data = {
  kind : (BranchCase.t, unit) ExecMap.cmd_kind;
  id : rid;
  cmd_report : 'cmd_report;
  unifys : ExecMap.unifys; [@default []]
  errors : string list; [@default []]
  branch_path : BranchCase.path;
}
[@@deriving yojson]

let make_executed_cmd_data
    kind
    id
    cmd_report
    ?(unifys = [])
    ?(errors = [])
    branch_path =
  { kind; id; cmd_report; unifys; errors; branch_path }

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
  val get_unifys_at_id : Logging.ReportId.t -> t -> ExecMap.unifys
  val get_root_id : t -> Logging.ReportId.t option
  val path_of_id : Logging.ReportId.t -> t -> BranchCase.path
  val existing_next_steps : rid -> t -> (rid * BranchCase.t option) list

  val next_step_specific :
    rid -> ExecMap.Packaged.branch_case option -> t -> rid * BranchCase.t option

  val previous_step :
    rid -> t -> (rid * ExecMap.Packaged.branch_case option) option

  val select_next_path :
    BranchCase.t option -> Logging.ReportId.t -> t -> BranchCase.path

  val find_unfinished_path :
    ?at_id:rid -> t -> (Logging.ReportId.t * BranchCase.t option) option

  (** Take the origin [tl_ast], an origin [node_id] and returns
      a string representing the evaluation step for the exec map.
      Should never be called if [source_map_ability] is false *)
  (* val get_origin_node_str : tl_ast -> int option -> string *)

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
