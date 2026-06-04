open Gillian.Utils.Exec_mode

let env_path_var = "GILLIAN_C2_RUNTIME_PATH"

open Archi

type t = {
  file : string;
  arch : Archi.t list;
  exec : Gillian.Utils.Exec_mode.t list;
}

(** All imports, should not be used as such, imports should be selected using
    the [import] function *)
let all_imports =
  [
    { file = "internals.gil"; arch = any_arch; exec = non_bi_exec };
    { file = "internals_act.gil"; arch = any_arch; exec = bi_exec };
    { file = "global_environment_common.gil"; arch = any_arch; exec = all_exec };
    { file = "internal_casts.gil"; arch = any_arch; exec = all_exec };
    { file = "internal_binops.gil"; arch = any_arch; exec = all_exec };
    { file = "internal_unops.gil"; arch = any_arch; exec = all_exec };
    { file = "internal_stdlib.gil"; arch = any_arch; exec = all_exec };
    { file = "rust_allocation_internals.gil"; arch = any_arch; exec = all_exec };
    { file = "string.gil"; arch = any_arch; exec = all_exec };
    { file = "logic_common.gil"; arch = any_arch; exec = exec_with_preds };
  ]

let imports exec_mode =
  let select x = List.mem exec_mode x.exec in
  List.map (fun imp -> (imp.file, false)) (List.filter select all_imports)
