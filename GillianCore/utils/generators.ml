(** Various generators *)

open Names

(** {2 General generator} *)

let fresh_sth (name : string) : (unit -> string) * (unit -> unit) =
  let counter = ref 0 in
  let f () =
    let v = name ^ string_of_int !counter in
    counter := !counter + 1;
    v
  in
  let r () = counter := 0 in
  (f, r)

(** {2 Specific generators} *)

(** Literal location counter *)
let fresh_loc, reset_loc = fresh_sth lloc_prefix

(** Program variable counter *)
let fresh_pvar, reset_pvar = fresh_sth pvar_prefix

(** Logical variable counter *)
let fresh_lvar, reset_lvar = fresh_sth lvar_prefix

(** Logical variable counter *)
let fresh_lvar_bi, reset_lvar_bi = fresh_sth lvar_prefix_bi

let fresh_svar, reset_svar = fresh_sth "#gen__"

let reset () =
  reset_loc ();
  reset_pvar ();
  reset_lvar ();
  reset_lvar_bi ();
  reset_svar ()
