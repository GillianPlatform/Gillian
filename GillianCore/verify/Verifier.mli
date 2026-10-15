module type S = sig
  type heap_t
  type m_err
  type annot

  module State : SState.S with type heap_t = heap_t and type m_err_t = m_err

  type state = State.t

  module SAInterpreter :
    G_interpreter.S
      with type vt = SVal.M.t
       and type st = SVal.SESubst.t
       and type store_t = SStore.t
       and type state_t = state
       and type heap_t = heap_t
       and type state_err_t = State.err_t
       and type annot = annot

  module SMatcher : Matcher.S with type state_t = State.t

  type t
  type prog_t = (annot, int) Prog.t
  type proc_tests = (string * t) list [@@deriving to_yojson]

  val start_time : float ref
  val reset : unit -> unit

  val verify_prog :
    init_data:State.init_data ->
    prog_t ->
    bool ->
    SourceFiles.t option ->
    unit Gillian_result.t

  val init_proc :
    init_data:State.init_data ->
    prog_t ->
    string ->
    SAInterpreter.result_t SAInterpreter.cont_func list

  val postprocess_files : SourceFiles.t option -> unit

  module Debug : sig
    val get_tests_for_prog :
      init_data:State.init_data -> prog_t -> MP.preds_tbl_t * proc_tests

    val analyse_result :
      t -> Logging.Report_id.t -> SAInterpreter.result_t -> bool
  end
end

module Make
    (State : SState.S)
    (PC : ParserAndCompiler.S)
    (External : External.T(PC.Annot).S) :
  S
    with type heap_t = State.heap_t
     and type m_err = State.m_err_t
     and module State = State
     and type annot = PC.Annot.t
