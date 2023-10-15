open Gillian.Utils.Prelude
open Gil_syntax

module SVal : sig
  type t [@@deriving yojson]

  val make : chunk:Chunk.t -> value:Gil_syntax.Expr.t -> t
  val pp : Format.formatter -> t -> unit
  val alocs : t -> SS.t
  val lvars : t -> SS.t
  val sure_is_zero : t -> bool
  val substitution : le_subst:(Expr.t -> Expr.t) -> t -> t
  val syntactic_equal : t -> t -> bool
  val reencode : chunk:Chunk.t -> t -> t Monadic.Delayed.t
  val to_gil_expr : chunk:Chunk.t -> t -> Gil_syntax.Expr.t
  val reduce : t -> t Monadic.Delayed.t
  val zero_of_chunk : Chunk.t -> t
  val any_of_chunk : Chunk.t -> t Monadic.Delayed.t
  val leak : t -> Chunk.t * Expr.t
  val leak_chunk : t -> Chunk.t
end

module SVArray : sig
  type t [@@deriving yojson]

  val make : chunk:Chunk.t -> values:Expr.t -> t
  val alocs : t -> SS.t
  val lvars : t -> SS.t
  val reduce : t -> t Monadic.Delayed.t
  val pp : Format.formatter -> t -> unit
  val sure_is_all_zeros : t -> bool
  val syntactic_equal : t -> t -> bool
  val leak : t -> Chunk.t * Expr.t
  val leak_chunk : t -> Chunk.t

  val make_zeros :
    chunk:Chunk.t -> size:Gil_syntax.Expr.t -> t Monadic.Delayed.t

  (** Decodes one sval into an array. For example one U32 into two U16s *)
  val decode_sval_into : chunk:Chunk.t -> SVal.t -> t Monadic.Delayed.t

  val decode_as_sval : chunk:Chunk.t -> t -> SVal.t Monadic.Delayed.t
  val byte_array_of_sval : SVal.t -> t Monadic.Delayed.t
  val singleton : SVal.t -> t

  (** Reencodes an array with another chunk.
      Should really be avoided if possible. *)
  val reencode : chunk:Chunk.t -> t -> t Monadic.Delayed.t

  val array_sub :
    arr:t -> start:Gil_syntax.Expr.t -> size:Gil_syntax.Expr.t -> t

  (** Splits at an offset in the array.
     For example, for an array of 4 u32 numbers, spliting at 2 gives 2 arrays of 2 u32s *)
  val split_at_offset : at:Expr.t -> t -> t * t

  (** Splits at a byte in the array.
     For example, spliting an array containing 1 value of type u32 at byte 2,
     gives 2 arrays of u8s. *)
  val split_at_byte : at:Expr.t -> t -> (t * t) Monadic.Delayed.t

  (** Creates an array of two sval if they have the same chunk,
     otherwise, returns None *)
  val of_two_svals_same_chunk : SVal.t -> SVal.t -> t option

  (** Concatenates two arrays if they have the same chunk,
     otherwise returns None *)
  val concat_same_chunk : t -> t -> t option

  (** [cons_same_chunk el arr] is [concat_same_chunk (singleting el) arr] *)
  val cons_same_chunk : SVal.t -> t -> t option

  (* [append_same_chunk arr el] is [concat_same_chunk arr (singleting el)] *)
  val append_same_chunk : t -> SVal.t -> t option
  val to_gil_expr : size:Expr.t -> chunk:Chunk.t -> t -> Gil_syntax.Expr.t
  val subst : le_subst:(Gil_syntax.Expr.t -> Gil_syntax.Expr.t) -> t -> t
end
