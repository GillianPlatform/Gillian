(** This module contains configuration for JaVerT. It is a list of flags that can be activated, or values that
    are configurable. *)

(** {2 Syntax} *)

let syntax_js = ref false

(** {2 JS2JSIL} *)

let use_strict = ref false

let js2jsil_harnessing = ref false

let js2jsil_line_numbers = ref false

let js2jsil_sep_procs = ref false

let unfolding = ref true

(** {2 Legacy config that is still used} *)

let cosette = ref false

let js = ref true

(** {2 Other config} *)
let env_var_import_path = "GILLIAN_JS_RUNTIME_PATH"

let import_paths =
  String.split_on_char ':'
    ( match Sys.getenv_opt "GILLIAN_JS_RUNTIME_PATH" with
    | Some s ->
        print_endline s;
        s
    | None   -> "" )
