(** Configuration for the framework

  This mostly consists of modifiable flags and hardcoded values *)

(** {2 Global config} *)

let under_approximation = ref false

let results_dir, set_result_dir =
  let rd = ref ".gillian" in
  ((fun () -> !rd), fun r -> rd := r)

let entry_point = ref "main"
let json_ui = ref false
let ci = ref false
let current_exec_mode : Exec_mode.t ref = ref Exec_mode.Verification
let previously_normalised = ref false
let unfolding = ref true
let manual_proof = ref false
let max_branching = ref 100

(* let perform_syntax_checks = ref false *)
let lemma_proof = ref false

(* FIXME: it seems like ngil files are never used anymore *)

(** {2 Hashtable sizes} *)

let small_tbl_size = 1
let medium_tbl_size = 1
let big_tbl_size = 1

(** {2 Bi-abduction config}
  These values seem to never be modified *)

let specs_to_stdout = ref false

(** {2 Debugging config} *)

(** Whether Gillian is running in debugger mode *)
let debug = ref false

(** {2 Printing config} *)

let no_heap = ref false

(** {2 Limited printing} *)

let pbn = ref false

(** {2 Statistics} *)

let stats = ref false

(** {2 Symbolic execution} *)

(** TODO: This should have a better name. *)

let bi_dflt = ref true
let bi_unfold_depth = ref 1
let bi_unroll_depth = ref 1
let bi_no_spec_depth = ref 0
let delay_entailment = ref true

(* If true, will dump a folder containing all the smt queries made to z3 *)
let dump_smt = ref false

(** {2 Bulk testing} *)

(** If activated, at the end of bulk execution or bulk wpst, and only with the Rely runner,
    a list of all failures will be printed in [stdout] *)
let bulk_print_all_failures = ref true

(** {2 Runtime settings} *)

let set_runtime_paths, get_runtime_paths =
  let runtime_paths : string list ref = ref [] in
  let set ?env_var ls =
    let new_runtime_paths =
      match (ls, env_var) with
      | [], Some v ->
          Option.fold ~none:[] ~some:(String.split_on_char ':')
            (Sys.getenv_opt v)
      | l, _ -> l
    in
    runtime_paths := new_runtime_paths
  in
  let get () = !runtime_paths in
  (set, get)

(** @canonical Gillian.Utils.Config.Verification *)
module Verification = struct
  let procs_to_verify = ref ([] : string list)
  let lemmas_to_verify = ref ([] : string list)
  let verify_only_some_of_the_things = ref false

  let set_procs_to_verify = function
    | [] -> ()
    | a ->
        procs_to_verify := a;
        verify_only_some_of_the_things := true

  let set_lemmas_to_verify = function
    | [] -> ()
    | a ->
        lemmas_to_verify := a;
        verify_only_some_of_the_things := true
end
