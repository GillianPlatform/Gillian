type t =
  | Var of string
  (* The optional sort is SMT-LIB's [(as f σ)] qualifier: a polymorphic
     symbol whose sort the arguments do not determine — [seq.empty] is the
     only one the encoder emits — is ill-formed without it. *)
  | App of (Sexplib.Sexp.t * Sort.t option * t list)
  | Lambda of (string * Sort.t * t)
  | Exists of (string * Sort.t * t)
  | Forall of (string * Sort.t * t)
  | Let of ((string * t) list * t)
  | Match of (t * (Pattern.t * t) list)

val from_extracted : Extracted.term -> t

val sanitise_var : string -> string

val to_sexp : t -> Sexplib.Sexp.t
