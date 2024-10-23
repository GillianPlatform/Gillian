(** @canonical Gillian.Command_line *)

module ParserAndCompiler = ParserAndCompiler

module Make
    (ID : Init_data.S)
    (CMemory : CMemory.S with type init_data = ID.t)
    (SMemory : SMemory.S with type init_data = ID.t)
    (PC : ParserAndCompiler.S with type init_data = ID.t)
    (External : External.T(PC.Annot).S)
    (Runners : sig
      val runners : Bulk.Runner.t list
    end)
    (Lifter : functor
      (V : Verifier.S with type annot = PC.Annot.t)
      ->
      Debugger_lifter.S
        with type memory = SMemory.t
         and type memory_error = SMemory.err_t
         and type tl_ast = PC.tl_ast
         and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
         and type annot = PC.Annot.t
         and type init_data = ID.t
         and type pc_err = PC.err) : sig
  (** Parses command-line arguments and starts the requested Gillian execution *)
  val main : unit -> unit
end
