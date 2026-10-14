module Make (State : SState.S) : sig
  (** [normalise_assertion ?pred_defs ?gamma ?pvars a] normalises the assertion
      [a] starting from the typing environment [gamma] considering the predicate
      table [pred_defs] and program variables [pvars]. It returns the
      appropriate predicate state and all learned bindings. *)
  val normalise_assertion :
    pred_defs:MP.preds_tbl_t ->
    init_data:State.init_data ->
    ?pvars:Utils.Containers.SS.t ->
    Asrt.t ->
    ((State.t * SVal.SESubst.t) list, string) result
end
