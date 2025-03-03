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
    is_breakpoint : bool;
    next_kind : (Branch_case.t, unit) Exec_map.next_kind;
    id : Logging.Report_id.t;
    cmd_report : 'cmd_report;
    matches : Match_map.matching list;
    errors : string list;
    branch_path : Branch_case.path;
  }
  [@@deriving yojson]

  type handle_cmd_result =
    | Stop of Logging.Report_id.t option
    | ExecNext of (Logging.Report_id.t option * Branch_case.t option)
  [@@deriving yojson]

  type _ Effect.t +=
    | IsBreakpoint : (string * int list) -> bool Effect.t
    | Node_updated :
        (Logging.Report_id.t * Exec_map.Packaged.node option)
        -> unit Effect.t
end

include Types

module type S = sig
  (** The lifter's state *)
  type t

  type memory_error
  type tl_ast
  type init_data
  type pc_err
  type memory
  type cmd_report
  type annot

  type _ Effect.t +=
    | Step :
        (Logging.Report_id.t option * Branch_case.t option * Branch_case.path)
        -> cmd_report executed_cmd_data Effect.t

  (** Given a proc name, a tl_ast, and the data from the first executed GIL
    command, initialise the lifter's state and handle the first command.

    Returns [None] if lifting is unsupported (i.e. if [tl_ast] is [None]). *)
  val init :
    proc_name:string ->
    all_procs:string list ->
    tl_ast option ->
    (annot, int) Prog.t ->
    (t * (unit -> Logging.Report_id.t * stop_reason)) option

  (** Exception-raising version of {!init}. *)
  val init_exn :
    proc_name:string ->
    all_procs:string list ->
    tl_ast option ->
    (annot, int) Prog.t ->
    t * (unit -> Logging.Report_id.t * stop_reason)

  (** Gives a JSON representation of the lifter's state.

      Used for debugging problems with the lifter.*)
  val dump : t -> Yojson.Safe.t

  val step_over : t -> Logging.Report_id.t -> Logging.Report_id.t * stop_reason
  val step_in : t -> Logging.Report_id.t -> Logging.Report_id.t * stop_reason
  val step_out : t -> Logging.Report_id.t -> Logging.Report_id.t * stop_reason
  val step_back : t -> Logging.Report_id.t -> Logging.Report_id.t * stop_reason

  val step_branch :
    t ->
    Logging.Report_id.t ->
    Exec_map.Packaged.branch_case option ->
    Logging.Report_id.t * stop_reason

  val continue : t -> Logging.Report_id.t -> Logging.Report_id.t * stop_reason

  val continue_back :
    t -> Logging.Report_id.t -> Logging.Report_id.t * stop_reason

  (** Gives a list of matches that occurred at the specified command. *)
  val get_matches_at_id : Logging.Report_id.t -> t -> Match_map.matching list

  val memory_error_to_exception_info :
    (memory_error, annot, tl_ast) memory_error_info -> exception_info

  val add_variables :
    store:(string * Expr.t) list ->
    memory:memory ->
    is_gil_file:bool ->
    get_new_scope_id:(unit -> int) ->
    Variable.ts ->
    Variable.scope list

  (* A proxy for ParserAndCompiler.parse_and_compile_files; this allows specifying an entrypoint function,
     and can receive a new entrypoint function to substitute it with.
     This is necessary for debugging harnessed programs, like with Gillian-C2. *)
  val parse_and_compile_files :
    entrypoint:string ->
    string list ->
    ((annot, tl_ast, init_data) ParserAndCompiler.compiled_progs * string)
    Gillian_result.t
end

module type Intf = sig
  (** @inline *)
  include module type of struct
    (** @inline *)
    include Types
  end

  module type S = S

  val make_executed_cmd_data :
    ?is_breakpoint:bool ->
    (Branch_case.t, unit) Exec_map.next_kind ->
    Logging.Report_id.t ->
    'cmd_report ->
    ?matches:Match_map.matching list ->
    ?errors:string list ->
    Branch_case.path ->
    'cmd_report executed_cmd_data
end
