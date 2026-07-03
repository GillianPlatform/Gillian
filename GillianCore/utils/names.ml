(** Prefix constants for variable names, with testers and builders *)

(** {2 Special names} *)

(** Name of the variable that holds the return value of a GIL function call. *)
let return_variable = "ret"

(** {2 Prefixes} *)

(** Literal location prefix *)
let lloc_prefix = "$loc_"

(** Program variable prefix *)
let pvar_prefix = "_pvar_"

(** Abstract location prefix *)
let aloc_prefix = "#loc_"

(** Logical variable prefix *)
let lvar_prefix = "#lvar_"

(** Logical variable prefix *)
let lvar_prefix_bi = "#lvar_bi_"

(** {2 Name testers} *)

let is_aloc_name (name : string) : bool =
  String.starts_with ~prefix:aloc_prefix name

let is_internal_lvar_name (name : string) : bool =
  is_aloc_name name || String.starts_with ~prefix:lvar_prefix name

let is_lvar_name (name : string) : bool = String.starts_with ~prefix:"#" name

let is_spec_var_name (name : string) : bool =
  is_lvar_name name && not (is_internal_lvar_name name)

let is_lloc_name (name : string) : bool =
  String.starts_with ~prefix:lloc_prefix name

let is_pvar_name (name : string) : bool =
  (not (is_lvar_name name)) && not (is_lloc_name name)

(** {2 Name builders} *)

let make_lvar_name (name : string) : string = lvar_prefix ^ name
let make_svar_name (name : string) : string = "#" ^ name
