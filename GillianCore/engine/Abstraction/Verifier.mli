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

  module SAInterpreter :
    GInterpreter.S
      with type vt = SVal.M.t
       and type st = st
       and type store_t = SStore.t
       and type state_t = state
       and type heap_t = heap_t
       and type state_err_t = SPState.err_t

  type t

  val start_time : float ref

  val reset : unit -> unit

  val verify_prog :
    (Annot.t, int) Prog.t -> bool -> SourceFiles.t option -> unit

  val verify_up_to_procs :
    (Annot.t, int) Prog.t -> SAInterpreter.result_t SAInterpreter.cont_func

  val postprocess_files : SourceFiles.t option -> unit
end

module Make
    (SState : State.S
                with type vt = SVal.M.t
                 and type st = SVal.SESubst.t
                 and type store_t = SStore.t)
    (SPState : PState.S
                 with type vt = SVal.M.t
                  and type st = SVal.SESubst.t
                  and type store_t = SStore.t
                  and type preds_t = Preds.SPreds.t)
    (External : External.S) :
  S with type heap_t = SPState.heap_t and type m_err = SPState.m_err_t
