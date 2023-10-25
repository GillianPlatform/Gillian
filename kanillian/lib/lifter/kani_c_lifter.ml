open Gillian.Gil_syntax
open Gillian.Debugger.Lifter
open Gillian.Debugger.Utils
module Exec_map = Gillian.Debugger.Utils.Exec_map

module Make
    (SMemory : Gillian.Symbolic.Memory_S)
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (Verification : Engine.Verifier.S
                      with type annot = Kanillian_compiler.K_annot.t) =
struct
  type t = unit

  module Annot = Kanillian_compiler.K_annot

  type memory_error = SMemory.err_t
  type tl_ast = Goto_lib.Program.t
  type memory = SMemory.t
  type cmd_report = Verification.SAInterpreter.Logging.ConfigReport.t
  type annot = Annot.t

  let init :
      proc_name:string ->
      all_procs:string list ->
      tl_ast option ->
      (annot, int) Prog.t ->
      cmd_report executed_cmd_data ->
      (t * handle_cmd_result) option =
    failwith "TODO"

  let init_exn :
      proc_name:string ->
      all_procs:string list ->
      tl_ast option ->
      (annot, int) Prog.t ->
      cmd_report executed_cmd_data ->
      t * handle_cmd_result =
    failwith "TODO"

  let dump : t -> Yojson.Safe.t = failwith "TODO"

  let handle_cmd :
      Logging.Report_id.t ->
      BranchCase.t option ->
      cmd_report executed_cmd_data ->
      t ->
      handle_cmd_result =
    failwith "TODO"

  let get_gil_map : t -> Exec_map.Packaged.t = failwith "TODO"
  let get_lifted_map : t -> Exec_map.Packaged.t option = failwith "TODO"
  let get_lifted_map_exn : t -> Exec_map.Packaged.t = failwith "TODO"

  let get_unifys_at_id : Logging.Report_id.t -> t -> Exec_map.unification list =
    failwith "TODO"

  let get_root_id : t -> Logging.Report_id.t option = failwith "TODO"
  let path_of_id : Logging.Report_id.t -> t -> BranchCase.path = failwith "TODO"

  let existing_next_steps :
      Logging.Report_id.t ->
      t ->
      (Logging.Report_id.t * BranchCase.t option) list =
    failwith "TODO"

  let next_gil_step :
      Logging.Report_id.t ->
      Exec_map.Packaged.branch_case option ->
      t ->
      Logging.Report_id.t * BranchCase.t option =
    failwith "TODO"

  let previous_step :
      Logging.Report_id.t ->
      t ->
      (Logging.Report_id.t * Exec_map.Packaged.branch_case option) option =
    failwith "TODO"

  let select_next_path :
      BranchCase.t option -> Logging.Report_id.t -> t -> BranchCase.path =
    failwith "TODO"

  let find_unfinished_path :
      ?at_id:Logging.Report_id.t ->
      t ->
      (Logging.Report_id.t * BranchCase.t option) option =
    failwith "TODO"

  let memory_error_to_exception_info :
      (memory_error, annot, tl_ast) memory_error_info -> exception_info =
    failwith "TODO"

  let add_variables :
      store:(string * Expr.t) list ->
      memory:memory ->
      is_gil_file:bool ->
      get_new_scope_id:(unit -> int) ->
      Variable.ts ->
      Variable.scope list =
    failwith "TODO"
end
