module type Gil_lifter_with_state = sig
  module Lifter : Gil_lifter.S

  val get_state : unit -> Lifter.t
end

module type Make = functor
  (SMemory : SMemory.S)
  (PC : ParserAndCompiler.S)
  (TLLifter : functor (Gil : Gil_lifter_with_state)
     (V : Verifier.S with type annot = PC.Annot.t) ->
     Lifter.S
       with type memory = SMemory.t
        and type tl_ast = PC.tl_ast
        and type memory_error = SMemory.err_t
        and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
        and type annot = PC.Annot.t
        and type init_data = PC.init_data
        and type pc_err = PC.err)
  (V : Verifier.S with type annot = PC.Annot.t)
  ->
  Lifter.S
    with type memory = SMemory.t
     and type tl_ast = PC.tl_ast
     and type memory_error = SMemory.err_t
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
     and type annot = PC.Annot.t
     and type init_data = PC.init_data
     and type pc_err = PC.err

module type Intf = sig
  (** A {!Gil_lifter}, along with a function to get its state *)
  module type Gil_lifter_with_state = Gil_lifter_with_state

  (**/**)

  module type Make = Make

  (**/**)

  module Make : Make
end
