(** Prefix constants for variable names, with testers and builders *)

(** {2 Special names} *)

(** Name of the variable that holds the return value of a GIL function call. *)
let return_variable = "ret"

(** {2 Prefixes} *)

(** Literal location prefix  *)
let lloc_prefix = "$l"

(** Program variable prefix  *)
let pvar_prefix = "_pvar_"

(** Abstract location prefix  *)
let aloc_prefix = "_$l_"

(** Logical variable prefix  *)
let lvar_prefix = "_lvar_"

(** Logical variable prefix  *)
let lvar_prefix_bi = "_lvar_bi_"

(** {2 Name testers} *)

let is_pvar_name (name : string) : bool =
  try
    name.[0] <> '#'
    && name.[0] <> '$'
    && (String.length name < 6 || String.sub name 0 6 <> lvar_prefix)
  with _ -> false

let is_aloc_name (name : string) : bool =
  try String.sub name 0 4 = aloc_prefix with _ -> false

let is_lvar_name (name : string) : bool =
  try name.[0] = '#' || String.sub name 0 6 = lvar_prefix with _ -> false

let is_spec_var_name (name : string) : bool =
  try name.[0] = '#' with _ -> false

let is_lloc_name (name : string) : bool =
  try String.sub name 0 2 = lloc_prefix with _ -> false

(** {2 Name builders} *)

let make_lvar_name (name : string) : string = lvar_prefix ^ name
let make_svar_name (name : string) : string = "#" ^ name
