(**
  Interface for GIL Stores.
  GIL stores are mappings from GIL variables to GIL values.
  GIL stores are mutable.
*)
module type S = sig
  (** Type of GIL values *)
  type vt

  (** Type of GIL stores *)
  type t

  val bindings : t -> (Var.t * vt) list
  (** Return the set of bindings in a given store *)

  val copy : t -> t
  (** Store copy *)

  val domain : t -> Var.Set.t
  (** Store domain *)

  val filter : t -> (Var.t -> vt -> vt option) -> unit
  (** Store filtering *)

  val fold : t -> (Var.t -> vt -> 'a -> 'a) -> 'a -> 'a
  (** Store fold *)

  val get : t -> Var.t -> vt option
  (** Return value of a given variable, if possible *)

  val get_unsafe : t -> Var.t -> vt
  (** Return value of a given variable or throw *)

  val init : (Var.t * vt) list -> t
  (** Store constructor, with a list of bindings of the form (variable, value) *)

  val iter : t -> (Var.t -> vt -> unit) -> unit
  (** Store iterator *)

  val mem : t -> Var.t -> bool
  (** Store membership *)

  val partition : t -> (vt -> bool) -> Var.Set.t * Var.Set.t
  (** Partition store domain *)

  val projection : t -> Var.t list -> t
  (** Store projection (returns new store) *)

  val put : t -> Var.t -> vt -> unit
  (** Update value of variable in store *)

  val remove : t -> Var.t -> unit
  (** Remove value of variable in store *)

  val pp : Format.formatter -> t -> unit
  (** Store printer *)

  val to_ssubst : t -> SVal.SSubst.t
  (** Converts the store into an ssubst *)

  val symbolics : t -> Var.Set.t
  (** Symbolic indices *)

  val lvars : t -> Var.Set.t
  (** Logical variables *)
end

module Make (Val : Val.S) : S with type vt = Val.t
