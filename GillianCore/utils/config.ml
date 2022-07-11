(** This file contains some configurations for the framework.
    Mostly composed of modifiable flags, or hardcoded values.  *)

(** {2 global config} *)

let results_dir, set_result_dir =
  let rd = ref ".gillian" in
  ((fun () -> !rd), fun r -> rd := r)

let entry_point = ref "main"
let ci = ref false
let current_exec_mode : ExecMode.t ref = ref ExecMode.Verification
let previously_normalised = ref false

(* FIXME: it seems like ngil files are never used anymore *)

(** {2 Hashtable sizes} *)

let small_tbl_size = 1
let medium_tbl_size = 1
let big_tbl_size = 1

(** {2 Bi-abduction configuration}
    These values seem to never be modified.. *)

let specs_to_stdout = ref false

(** This value seems to never be modified *)
let bug_specs_propagation = ref false

(** {2 Debugging configuration} *)

let debug = ref false

(** {2 Printing configuration} *)

let no_heap = ref false

(** {2 Global configuration} *)

let unfolding = ref true
let manual_proof = ref false

(* let perform_syntax_checks = ref false *)
let lemma_proof = ref false

(** {2 Parallel threading} *)

let parallel = ref false

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

module Verification = struct
  let exact = ref false
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
