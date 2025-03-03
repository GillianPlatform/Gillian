module type S = sig
  type heap_t
  type state
  type m_err
  type annot

  module SPState :
    PState.S
      with type t = state
       and type heap_t = heap_t
       and type m_err_t = m_err

  module SAInterpreter :
    G_interpreter.S
      with type vt = SVal.M.t
       and type st = SVal.SESubst.t
       and type store_t = SStore.t
       and type state_t = state
       and type heap_t = heap_t
       and type state_err_t = SPState.err_t
       and type annot = annot

  module SMatcher : Matcher.S

  type t
  type prog_t = (annot, int) Prog.t
  type proc_tests = (string * t) list [@@deriving to_yojson]

  val start_time : float ref
  val reset : unit -> unit

  val verify_prog :
    init_data:SPState.init_data ->
    prog_t ->
    bool ->
    SourceFiles.t option ->
    unit Gillian_result.t

  val verify_up_to_procs :
    ?proc_name:string ->
    init_data:SPState.init_data ->
    prog_t ->
    SAInterpreter.result_t SAInterpreter.cont_func

  val postprocess_files : SourceFiles.t option -> unit

  module Debug : sig
    val get_tests_for_prog : init_data:SPState.init_data -> prog_t -> proc_tests

    val analyse_result :
      t -> Logging.Report_id.t -> SAInterpreter.result_t -> bool
  end
end

module Make
    (SState : SState.S
                with type vt = SVal.M.t
                 and type st = SVal.SESubst.t
                 and type store_t = SStore.t)
    (SPState : PState.S
                 with type state_t = SState.t
                  and type init_data = SState.init_data)
    (PC : ParserAndCompiler.S)
    (External : External.T(PC.Annot).S) :
  S
    with type heap_t = SPState.heap_t
     and type m_err = SPState.m_err_t
     and type state = SPState.t
     and module SPState = SPState
     and type annot = PC.Annot.t
