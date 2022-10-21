type unify_result = Success | Failure [@@deriving yojson]

type ('err, 'ast) memory_error_info = {
  error : 'err;  (** The memory error that needs to be lifted *)
  command : (int Cmd.t * Annot.t) option;  (** The command where it happened *)
  tl_ast : 'ast option;
      (** If the program was compiled from the target language, we keep the tl ast around *)
}

type 'cmd_report executed_cmd_data = {
  kind : BranchCase.t ExecMap.cmd_kind;
  id : Logging.ReportId.t;
  cmd : 'cmd_report;
  unifys : ExecMap.unifys;
  errors : string list;
  branch_path : BranchCase.path;
}

val make_executed_cmd_data :
  BranchCase.t ExecMap.cmd_kind ->
  Logging.ReportId.t ->
  'cmd_report ->
  ?unifys:ExecMap.unifys ->
  ?errors:string list ->
  BranchCase.path ->
  'cmd_report executed_cmd_data

type handle_cmd_result =
  | Stop
  | ExecNext of (Logging.ReportId.t option * BranchCase.t option)
[@@deriving yojson]

module type S = sig
  type t
  type memory_error
  type tl_ast
  type memory
  type cmd_report

  val init : cmd_report executed_cmd_data -> t * handle_cmd_result
  val dump : t -> Yojson.Safe.t
  val handle_cmd : cmd_report executed_cmd_data -> t -> handle_cmd_result
  val get_gil_map : t -> ExecMap.Packaged.t
  val get_lifted_map : t -> ExecMap.Packaged.t option
  val get_unifys_at_id : Logging.ReportId.t -> t -> ExecMap.unifys
  val get_root_id : t -> Logging.ReportId.t option
  val path_of_id : Logging.ReportId.t -> t -> BranchCase.path

  val select_next_path :
    BranchCase.t option -> Logging.ReportId.t -> t -> BranchCase.path

  val find_unfinished_path :
    ?at_path:BranchCase.path ->
    t ->
    (Logging.ReportId.t * BranchCase.t option) option

  (** Take the origin [tl_ast], an origin [node_id] and returns
      a string representing the evaluation step for the exec map.
      Should never be called if [source_map_ability] is false *)
  val get_origin_node_str : tl_ast -> int option -> string

  val memory_error_to_exception_info :
    (memory_error, tl_ast) memory_error_info -> DebuggerTypes.exception_info

  val add_variables :
    store:(string * Expr.t) list ->
    memory:memory ->
    is_gil_file:bool ->
    get_new_scope_id:(unit -> int) ->
    DebuggerTypes.variables ->
    DebuggerTypes.scope list
end

module BaseGilLifter
    (V : Verifier.S)
    (SMemory : SMemory.S)
    (PC : ParserAndCompiler.S) :
  S
    with type memory = SMemory.t
     and type tl_ast = PC.tl_ast
     and type memory_error = SMemory.err_t
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
