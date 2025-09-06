module type Types = sig
  type case
  type breakpoint
end

module type S = sig
  type id
  type case
  type breakpoint

  type _ Effect.t +=
    | Choice : (id * case list) option -> (id * case) option Effect.t
    | Breakpoint : breakpoint -> (id * case) option Effect.t

  val choose : ?force:bool -> (case * 'res Seq.t) list -> 'res Seq.t
  val choose_const : ?force:bool -> (case * 'res) list -> 'res Seq.t
  val breakpoint : breakpoint -> unit
  val to_list : 'res Seq.t -> 'res list
end

module type Intf = sig
  module type Types = Types
  module type S = S

  module Make (Types : Types) :
    S with type case = Types.case and type breakpoint = Types.breakpoint

  module Dummy : sig
    include S with type case = int and type breakpoint = unit

    val choose : 'res Seq.t list -> 'res Seq.t
    val choose_const : 'res list -> 'res Seq.t
  end
end
