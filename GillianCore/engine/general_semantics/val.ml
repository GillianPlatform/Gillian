(** @canonical Gillian.General.Val

  Interface for GIL Values *)

(** @canonical Gillian.General.Val.S *)
module type S = sig
  (** Type of GIL values *)
  type t [@@deriving yojson]

  (** Type of substitutions for GIL values *)
  type st

  (** Type of extended substitutions for GIL values *)
  type et

  val equal : t -> t -> bool

  (** Printer *)
  val pp : Format.formatter -> t -> unit

  val full_pp : Format.formatter -> t -> unit
  val full_pp_list : Format.formatter -> t list -> unit

  (** Convert a value to a literal, if possible *)
  val to_literal : t -> Literal.t option

  (** Convert a literal to a value, always possible *)
  val from_literal : Literal.t -> t

  (** Convert a value to a logical expression, always possible *)
  val to_expr : t -> Expr.t

  (** Converts a logical expression to a value, if possible *)
  val from_expr : Expr.t -> t option

  (** Convert a list of values to a single value, always possible *)
  val from_list : t list -> t

  (** Converts a logical variable name into a value *)
  val from_lvar_name : string -> t

  (** Convert a value to a list of values, if possible *)
  val to_list : t -> t list option
end
