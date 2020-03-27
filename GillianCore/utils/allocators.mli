(** {2 Allocators} *)

val register_resetter : (unit -> unit) -> unit
(** Allocators's resetters should be registers so that Bulk Execution can reset them at every start *)

val reset_all : unit -> unit

module type S = sig
  type t

  val alloc : unit -> t

  val dealloc : t -> unit

  val eq : t -> t -> bool

  val reset : unit -> unit
end

module type S_with_stringify = sig
  include S

  val to_string : t -> string

  val of_string : string -> t
end

(** Creating a BasicIdentifier module also registers its resetter on its own *)
module Basic () : S_with_stringify with type t = int

(** Make_with_prefix is assuming that one already registered the resetter of A *)
module Make_with_prefix
    (A : S_with_stringify) (P : sig
      val prefix : string
    end) : S with type t = string
