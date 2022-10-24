module type S = sig
  include State.S

  val make_s :
    init_data:init_data ->
    store:store_t ->
    pfs:PFS.t ->
    gamma:TypEnv.t ->
    spec_vars:SS.t ->
    t

  val init : init_data -> t
  val clear_resource : t -> t
  val get_typ_env : t -> TypEnv.t
  val get_pfs : t -> PFS.t
  val get_lvars_for_exact : t -> Var.Set.t

  val hides :
    is_post:bool ->
    used_unifiables:Expr.Set.t ->
    exprs_to_hide:Expr.t list ->
    t ->
    (unit, Expr.t) result
end

module Make (SMemory : SMemory.S) :
  S
    with type st = SVal.SESubst.t
     and type vt = SVal.M.t
     and type store_t = SStore.t
     and type heap_t = SMemory.t
     and type m_err_t = SMemory.err_t
     and type init_data = SMemory.init_data
