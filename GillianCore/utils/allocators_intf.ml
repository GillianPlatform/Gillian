(**/**)

module type S = sig
  type t [@@deriving yojson, eq, ord]

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

(**/**)

module type Intf = sig
  (** Allocators's resetters should be registered so that Bulk Execution can reset them at every start *)
  val register_resetter : (unit -> unit) -> unit

  (** Resets all registered allocators *)
  val reset_all : unit -> unit

  module type S = S
  module type S_with_stringify = S_with_stringify

  (** A basic int allocator
      
    Automatically registers a resetter *)
  module Basic () : S_with_stringify with type t = int

  (** Wraps an allocator [A] with a string prefix
      
    Assumes that [A]'s resetter has already been registered *)
  module Make_with_prefix
      (A : S_with_stringify) (P : sig
        val prefix : string
      end) : S with type t = string
end
