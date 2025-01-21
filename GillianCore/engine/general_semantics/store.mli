(** @canonical Gillian.General.Store

  Interface for GIL Stores

  GIL stores are (mutable) mappings from GIL variables to GIL values. *)

(** @canonical Gillian.General.Store.S *)
module type S = sig
  (** Type of GIL values *)
  type vt

  (** Type of GIL stores *)
  type t [@@deriving yojson]

  (** Return the set of bindings in a given store *)
  val bindings : t -> (Var.t * vt) list

  (** Store copy *)
  val copy : t -> t

  (** Store domain *)
  val domain : t -> Var.Set.t

  (** Store filtering *)
  val filter_map_inplace : t -> (Var.t -> vt -> vt option) -> unit

  (** Store fold *)
  val fold : t -> (Var.t -> vt -> 'a -> 'a) -> 'a -> 'a

  (** Return value of a given variable, if possible *)
  val get : t -> Var.t -> vt option

  (** Return value of a given variable or throw *)
  val get_unsafe : t -> Var.t -> vt

  (** Store constructor, with a list of bindings of the form (variable, value) *)
  val init : (Var.t * vt) list -> t

  (** Store iterator *)
  val iter : t -> (Var.t -> vt -> unit) -> unit

  (** Store membership *)
  val mem : t -> Var.t -> bool

  (** Partition store domain *)
  val partition : t -> (vt -> bool) -> Var.Set.t * Var.Set.t

  (** Store projection (returns new store) *)
  val projection : t -> Var.t list -> t

  (** Update value of variable in store *)
  val put : t -> Var.t -> vt -> unit

  (** Remove value of variable in store *)
  val remove : t -> Var.t -> unit

  (** Store printer *)
  val pp : Format.formatter -> t -> unit

  (** Store printer by need *)
  val pp_by_need : Var.Set.t -> Format.formatter -> t -> unit

  (** Converts the store into an ssubst *)
  val to_ssubst : t -> SVal.SESubst.t

  (** Logical variables *)
  val lvars : t -> LVar.Set.t
end

module Make (Val : Val.S) : S with type vt = Val.t
