(** Configuration for the framework

  This mostly consists of modifiable flags and hardcoded values *)

(** {2 Global config} *)

let under_approximation = ref false

let results_dir, set_result_dir =
  let rd = ref ".gillian" in
  ((fun () -> !rd), fun r -> rd := r)

let file_content_overrides : (string, string) Hashtbl.t = Hashtbl.create 0
let entry_point = ref "main"
let json_ui = ref false
let ci = ref false
let current_exec_mode : Exec_mode.t ref = ref Exec_mode.Verification
let previously_normalised = ref false
let unfolding = ref true
let manual_proof = ref false
let max_branching = ref 100
let leak_check = ref false

(* let perform_syntax_checks = ref false *)
let lemma_proof = ref false
let dump_annots = ref false
let debug_log_verbose = ref false

(* FIXME: it seems like ngil files are never used anymore *)

(** {2 Hashtable sizes} *)

let small_tbl_size = 0
let medium_tbl_size = 0
let big_tbl_size = 0

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

(* If true, will dump a folder containing all smt queries made to the solver *)
let dump_smt = ref false

(** {2 Bulk testing} *)

(** If activated, at the end of bulk execution or bulk wpst, and only with the Rely runner,
    a list of all failures will be printed in [stdout] *)
let bulk_print_all_failures = ref true

(** {2 Runtime settings} *)

let set_runtime_paths, get_runtime_paths =
  let runtime_paths : string list ref = ref [] in
  let set ?default_folders ls =
    let new_runtime_paths =
      match (ls, default_folders) with
      | [], Some fs -> fs
      | l, _ -> l
    in
    runtime_paths := new_runtime_paths
  in
  let get () = !runtime_paths in
  (set, get)

(** @canonical Gillian.Utils.Config.Verification *)
module Verification = struct
  type things_to_verify = Specific | All | ProcsOnly | LemmasOnly

  let procs_to_verify = ref ([] : string list)
  let lemmas_to_verify = ref ([] : string list)
  let things_to_verify = ref All

  let set_procs_to_verify = function
    | [] -> ()
    | a ->
        procs_to_verify := a;
        things_to_verify := Specific

  let set_lemmas_to_verify = function
    | [] -> ()
    | a ->
        lemmas_to_verify := a;
        things_to_verify := Specific
end

(** {2 Resetting} 
    With the addition of in-file config statements, we want to be able to "reset" the config
    when analysis is run multiple times in one "instance", i.e. the LSP. *)
let reset_config_f = ref None

let reset_config () =
  match !reset_config_f with
  | Some f ->
      reset_config_f := None;
      f ()
  | None -> ()
