(** GIL identifiers, tagged by kind. *)

type +'a t
type loc = [ `Loc ]
type aloc = [ `ALoc ]
type var = [ `Var ]
type lvar = [ `LVar ]
type any_var = [ var | lvar ]
type any_loc = [ loc | aloc ]
type substable = [ aloc | var | lvar ]
type any = [ loc | aloc | var | lvar ]

val to_string : 'a t -> string
val equal : 'a t -> 'a t -> bool
val compare : 'a t -> 'a t -> int

(** Prints the bare name. *)
val pp : 'a t Fmt.t

module type Set_S = sig
  include Set.S

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module type S = sig
  type tag
  type nonrec t = tag t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
  val equal : t -> t -> bool
  val compare : t -> t -> int

  (** Quotes the name, as [[@@deriving show]] did for the [string] that
      identifiers used to be, so derived printers keep their output. Use
      {!Id.pp} for the bare name. *)
  val pp : t Fmt.t

  val show : t -> string

  (** Unchecked: the caller vouches that the string names an identifier of this
      kind. *)
  val of_string : string -> t

  val to_string : t -> string

  module Set : Set_S with type elt = t
  module Map : Map.S with type key = t
end

module type Allocated = sig
  include S

  val alloc : unit -> t
end

(** Concrete locations *)
module Loc : Allocated with type tag = loc

(** Abstract locations *)
module ALoc : Allocated with type tag = aloc

(** Program variables *)
module Var : sig
  include Allocated with type tag = var

  val return_variable : t
end

(** Logical variables *)
module LVar : sig
  include Allocated with type tag = lvar

  (** Spec variables are user-written logical variables ([#x]). *)
  val is_spec : t -> bool

  (** Internal logical variables are the ones Gillian allocates. *)
  val is_internal : t -> bool
end

module Any_var : S with type tag = any_var
module Any_loc : S with type tag = any_loc
module Substable : S with type tag = substable

(** {2 Recovering the kind of an identifier}

    These go by the naming conventions of {!Utils.Names}. *)

val classify_var : [< any_var ] t -> [ `Var of Var.t | `LVar of LVar.t ]
val classify_loc : [< any_loc ] t -> [ `Loc of Loc.t | `ALoc of ALoc.t ]
val as_lvar : [< any_var ] t -> LVar.t option
val as_lvars : [< any_var ] t list -> LVar.t list option
