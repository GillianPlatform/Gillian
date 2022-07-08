module type S = sig
  type st
  type heap_t
  type state
  type m_err

  module SPState :
    PState.S
      with type t = state
       and type vt = SVal.M.t
       and type st = st
       and type store_t = SStore.t
       and type heap_t = heap_t
       and type m_err_t = m_err
       and type preds_t = Preds.SPreds.t

  module SAInterpreter :
    GInterpreter.S
      with type vt = SVal.M.t
       and type st = st
       and type store_t = SStore.t
       and type state_t = state
       and type heap_t = heap_t
       and type state_err_t = SPState.err_t

  module SUnifier : Unifier.S with type st = SVal.SESubst.t

  type t
  type prog_t = (Annot.t, int) Prog.t
  type proc_tests = (string * t) list [@@deriving to_yojson]

  val start_time : float ref
  val reset : unit -> unit
  val verify_prog : prog_t -> bool -> SourceFiles.t option -> unit

  val verify_up_to_procs :
    prog_t -> SAInterpreter.result_t SAInterpreter.cont_func

  val postprocess_files : SourceFiles.t option -> unit

  module Debug : sig
    val get_tests_for_prog : prog_t -> proc_tests

    val analyse_result :
      t -> Logging.ReportId.t -> SAInterpreter.result_t -> bool
  end
end

module Make
    (SState : SState.S
                with type vt = SVal.M.t
                 and type st = SVal.SESubst.t
                 and type store_t = SStore.t)
    (SPState : PState.S
                 with type vt = SState.vt
                  and type st = SState.st
                  and type state_t = SState.t
                  and type store_t = SState.store_t
                  and type preds_t = Preds.SPreds.t)
    (External : External.S) :
  S with type heap_t = SPState.heap_t and type m_err = SPState.m_err_t
