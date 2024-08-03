module type S = sig
  type tl_ast
  type t

  module Inspect : sig
    type debug_state_view [@@deriving yojson]

    val get_debug_state : t -> debug_state_view
    val get_match_map : Logging.Report_id.t -> t -> Match_map.t
  end

  val launch : string -> string option -> (t, string) result
  val jump_to_id : Logging.Report_id.t -> t -> (unit, string) result
  val step_in : t -> stop_reason
  val step : ?reverse:bool -> t -> stop_reason

  val step_specific :
    Exec_map.Packaged.branch_case option ->
    Logging.Report_id.t ->
    t ->
    (stop_reason, string) result

  val step_out : t -> stop_reason
  val run : ?reverse:bool -> ?launch:bool -> t -> stop_reason
  val start_proc : string -> t -> (stop_reason, string) result
  val terminate : t -> unit
  val get_frames : t -> frame list
  val get_scopes : t -> Variable.scope list
  val get_variables : int -> t -> Variable.t list
  val get_exception_info : t -> exception_info
  val set_breakpoints : string option -> int list -> t -> unit
end

module type Make = functor
  (ID : Init_data.S)
  (PC : ParserAndCompiler.S with type init_data = ID.t)
  (V : Verifier.S with type SPState.init_data = ID.t and type annot = PC.Annot.t)
  (Lifter : Debugger_lifter.S
              with type memory = V.SAInterpreter.heap_t
               and type memory_error = V.SPState.m_err_t
               and type tl_ast = PC.tl_ast
               and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
               and type annot = PC.Annot.t)
  -> S

module type Intf = sig
  (** @canonical Gillian.Debugger.S *)
  module type S = S

  (**/**)

  module type Make = Make

  (**/**)

  module Verification_debugger : sig
    module Make : Make
  end

  module Symbolic_debugger : sig
    module Make : Make
  end
end
