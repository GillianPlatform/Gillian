open Llvm_memory_model
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (MonadicSMemory)
module Init_data = Gillian.General.Init_data.Dummy
module DummyParserAndCompiler = ParserAndCompiler.Dummy

module DummyLifter (V : Gillian.Abstraction.Verifier.S) :
  Debugger_lifter.S
    with type memory = SMemory.t
     and type memory_error = SMemory.err_t
     and type tl_ast = DummyParserAndCompiler.tl_ast
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
     and type annot = DummyParserAndCompiler.Annot.t
     and type init_data = DummyParserAndCompiler.init_data
     and type pc_err = DummyParserAndCompiler.err = struct
  type pc_err = DummyParserAndCompiler.err
  type init_data = unit
  type t = unit
  type memory = SMemory.t
  type memory_error = SMemory.err_t
  type tl_ast = DummyParserAndCompiler.tl_ast
  type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
  type annot = DummyParserAndCompiler.Annot.t

  type exec_args =
    Logging.Report_id.t option
    * Gil_syntax.Branch_case.t option
    * Gil_syntax.Branch_case.path

  type _ Effect.t +=
    | Step :
        exec_args
        -> cmd_report Gillian.Debugger.Lifter.executed_cmd_data Effect.t

  let init
      ~proc_name:_
      ~all_procs:_
      (_st : tl_ast option)
      (_prg : (annot, int) Gil_syntax.Prog.t) =
    raise (Failure "unsupported")

  (** Exception-raising version of {!init}. *)
  let init_exn
      ~proc_name:_
      ~all_procs:_
      (_st : tl_ast option)
      (_p : (annot, int) Gil_syntax.Prog.t) =
    raise (Failure "unsupported")

  (** Gives a JSON representation of the lifter's state.

  Used for debugging problems with the lifter.*)
  let dump (_st : t) = raise (Failure "unsupported")

  let step_over (_ : t) (_ : Logging.Report_id.t) =
    raise (Failure "unsupported")

  let step_in _ _ = raise (Failure "unsupported")
  let step_out _ _ = raise (Failure "unsupported")
  let step_back _ _ = raise (Failure "unsupported")
  let step_branch _ _ _ = raise (Failure "unsupported")
  let continue _ _ = raise (Failure "unsupported")
  let continue_back _ _ = raise (Failure "unsupported")

  (** Gets the non-lifted execution map of GIL commands.

  In most cases, it's recommended to use a {!Gil_fallback_lifter}, and just
  defer this call to the GIL lifter. *)
  let get_gil_map _ = raise (Failure "unsupported")

  (** Gets the lifted execution map.

  Returns [None] if lifting is not supported. *)
  let get_lifted_map _ = raise (Failure "unsupported")

  (** Exception-raising version of {!get_lifted_map}. *)
  let get_lifted_map_exn _ = raise (Failure "unsupported")

  (** Gives a list of matches that occurred at the specified command. *)
  let get_matches_at_id _ _ = raise (Failure "unsupported")

  let memory_error_to_exception_info _ = raise (Failure "unsupported")

  let add_variables ~store:_ ~memory:_ ~is_gil_file:_ ~get_new_scope_id:_ _ =
    raise (Failure "unsupported")

  let parse_and_compile_files ~entrypoint:_ fls = raise (Failure "unsupported")
end

module CLI =
  Gillian.Command_line.Make (Init_data) (CMemory) (SMemory)
    (DummyParserAndCompiler)
    (External.M)
    (struct
      let runners : Gillian.Bulk.Runner.t list =
        [ (module Gillian_llvm_lib.SRunner) ]
    end)
    (DummyLifter)

let () = CLI.main ()
