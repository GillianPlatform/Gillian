open Cmdliner
module ParserAndCompiler = ParserAndCompiler
module Act_console = Act_console

module Make
    (ID : Init_data.S)
    (CMemory : CMemory.S with type init_data = ID.t)
    (SMemory : Monadic.MonadicSMemory.S with type init_data = ID.t)
    (PC : ParserAndCompiler.S with type init_data = ID.t)
    (External : External.T(PC.Annot).S)
    (Runners : Runners.S)
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
         and type pc_err = PC.err) =
struct
  module Gil_parsing = Gil_parsing.Make (PC.Annot)
  module CState = CState.Make (CMemory)
  module AbsMemory = Combinators.Abstraction.Make (SMemory)
  module VSMemory = Monadic.MonadicSMemory.Lift (AbsMemory)

  module C_interpreter =
    G_interpreter.Make (CVal.M) (CVal.CESubst) (CStore) (CState) (PC) (External)

  module VSState = SState.Make (VSMemory)
  module SState = SState.Make (Monadic.MonadicSMemory.Lift (SMemory))

  module S_interpreter =
    G_interpreter.Make (SVal.M) (SVal.SESubst) (SStore) (SState) (PC) (External)

  module SPState = PState.Make (VSState)
  module Verification = Verifier.Make (VSState) (SPState) (PC) (External)
  module Abductor = Abductor.Make (SPState) (PC) (External)

  (* The tool-provided [Lifter] works over the raw memory [SMemory.t] and its
     errors, but the verification state's heap is
     [SMemory.t Combinators.Abstraction.abs] (the predicate/wand-carrying
     wrapper) and its memory errors are [AbsMemory.err_t]. [get_variables] and
     [memory_error_to_exception_info] are the only members of
     [Debugger_lifter.S] that mention those types, so we apply [Lifter] to
     [Verification] to get all the right associated types and re-project the
     abstraction wrapper back onto the raw lifter. *)
  module V_lifter = struct
    module Raw = Lifter (Verification)
    include Raw

    type memory = Verification.heap_t
    type memory_error = Verification.SPState.m_err_t

    let get_variables t (astate : memory Debugger_utils.astate) id =
      let raw_astate : SMemory.t Debugger_utils.astate =
        { astate with memory = astate.memory.Combinators.Abstraction.mem }
      in
      Raw.get_variables t raw_astate id

    let memory_error_to_exception_info
        (info : (memory_error, _, _) Debugger_lifter.memory_error_info) :
        Debugger_utils.exception_info =
      match info.error with
      | AbsMemory.SubError error ->
          Raw.memory_error_to_exception_info { info with error }
      | error ->
          { id = Fmt.to_to_string AbsMemory.pp_err error; description = None }
  end

  module Symb_debugger =
    Debugger.Symbolic_debugger.Make (ID) (PC) (Verification) (V_lifter)

  module Verif_debugger =
    Debugger.Verification_debugger.Make (ID) (PC) (Verification) (V_lifter)

  let split_cmds cmds =
    let cmds, debug_cmds, lsp_cmds =
      List.fold_left
        Console.(
          fun (acc_normal, acc_debug, acc_lsp) -> function
            | Normal cmd -> (cmd :: acc_normal, acc_debug, acc_lsp)
            | Debug cmd -> (acc_normal, cmd :: acc_debug, acc_lsp)
            | Lsp cmd -> (acc_normal, acc_debug, cmd :: acc_lsp))
        ([], [], []) cmds
    in
    let cmds =
      match debug_cmds with
      | [] -> cmds
      | _ ->
          let debug_info = Cmd.info "debug" ~doc:"Commands for debugger mode" in
          Cmd.group debug_info debug_cmds :: cmds
    in
    let cmds =
      match lsp_cmds with
      | [] -> cmds
      | _ ->
          let lsp_info =
            Cmd.info "lsp" ~doc:"Commands for language server mode"
          in
          Cmd.group lsp_info lsp_cmds :: cmds
    in
    cmds

  let main' () =
    Memtrace.trace_if_requested ();

    let doc = "An analysis toolchain" in

    let man =
      [
        `S Manpage.s_description;
        `P "Analysis toolchain for a given language, based on Gillian";
      ]
    in
    let info =
      Cmd.info ~exits:Common_args.exit_code_info
        (Filename.basename Sys.executable_name)
        ~doc ~man
    in

    let consoles : (module Console.S) list =
      [
        (module Compiler_console.Make (ID) (PC));
        (module C_interpreter_console.Make (ID) (PC) (CState) (C_interpreter)
                  (Gil_parsing));
        (module Wpst_console.Make (ID) (PC) (SState) (S_interpreter)
                  (Gil_parsing)
                  (Debug_adapter.Make (Symb_debugger)));
        (module Verification_console.Make (ID) (PC) (Verification) (Gil_parsing)
                  (Debug_adapter.Make (Verif_debugger)));
        (module Act_console.Make (ID) (PC) (Abductor) (Gil_parsing));
        (module Bulk_console.Make (PC) (Runners));
      ]
    in
    let cmds =
      consoles
      |> List.concat_map (fun (module C : Console.S) -> C.cmds)
      |> split_cmds
    in
    exit (Cmd.eval (Cmd.group info cmds))

  let main () =
    let open Effect.Deep in
    try main' () with
    | effect Utils.Sys_error_during_logging (s, bt), k ->
        let msg =
          Fmt.str "!! ERROR DURING LOGGING: %s !!\nBacktrace:\n%s" s bt
        in
        Logging.print_to_all msg;
        Debugger_log.log (fun m -> m "%s" msg);
        continue k ()
    | effect eff, k -> discontinue k (Effect.Unhandled eff)
end
