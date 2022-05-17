module type S = sig
  include State.S

  val get_typ_env : t -> TypEnv.t
  val get_pfs : t -> PFS.t
  val get_lvars_for_exact : t -> Var.Set.t

  val hides :
    ?is_post:bool ->
    used_unifiables:Expr.Set.t ->
    t ->
    exprs_to_hide:Expr.t list ->
    (unit, Expr.t) result
end

module Make (SMemory : SMemory.S) :
  S
    with type st = SVal.SESubst.t
     and type vt = SVal.M.t
     and type store_t = SStore.t
     and type heap_t = SMemory.t
     and type m_err_t = SMemory.err_t
