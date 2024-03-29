module type S = sig
  include
    State.S
      with type vt = Expr.t
       and type st = SVal.SESubst.t
       and type store_t = SStore.t

  val make_s :
    init_data:init_data ->
    store:store_t ->
    pfs:PFS.t ->
    gamma:Type_env.t ->
    spec_vars:SS.t ->
    t

  val init : init_data -> t
  val get_init_data : t -> init_data
  val clear_resource : t -> t
  val get_typ_env : t -> Type_env.t
  val get_pfs : t -> PFS.t
  val sure_is_nonempty : t -> bool
  val consume_core_pred : string -> t -> vt list -> action_ret
  val produce_core_pred : string -> t -> vt list -> t list

  (** See {!val:SMemory.S.split_further} *)
  val split_core_pred_further :
    t -> string -> vt list -> err_t -> (vt list list * vt list) option
end

module Make (SMemory : SMemory.S) :
  S
    with type heap_t = SMemory.t
     and type m_err_t = SMemory.err_t
     and type init_data = SMemory.init_data
