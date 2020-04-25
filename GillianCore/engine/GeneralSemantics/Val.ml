(** Interface for GIL Values *)
module type S = sig
  (** Type of GIL values *)
  type t

  (** Type of substitutions for GIL values *)
  type st

  val pp : Format.formatter -> t -> unit
  (** Printer *)

  val full_pp : Format.formatter -> t -> unit

  val to_literal : t -> Literal.t option
  (** Convert a value to a literal, if possible *)

  val from_literal : Literal.t -> t
  (** Convert a literal to a value, always possible *)

  val to_expr : t -> Expr.t
  (** Convert a value to a logical expression, always possible *)

  val from_expr : Expr.t -> t option
  (** Converts a logical expression to a value, if possible *)

  val from_list : t list -> t
  (** Convert a list of values to a single value, always possible *)

  val from_lvar_name : string -> t
  (** Converts a logical variable name into a value *)

  val to_list : t -> t list option
  (** Convert a value to a list of values, if possible *)

  val is_concrete : t -> bool
  (** Is the value concrete? *)
end
