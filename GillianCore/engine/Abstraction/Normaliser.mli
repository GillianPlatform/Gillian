module Make
    (SPState : PState.S
                 with type vt = SVal.M.t
                  and type st = SVal.SESubst.t
                  and type store_t = SStore.t
                  and type preds_t = Preds.SPreds.t
                  and type wands_t = Wands.SWands.t) : sig
  (** [normalise_assertion ?pred_defs ?gamma ?pvars a] normalises the
      assertion [a] starting from the typing environment [gamma]
      considering the predicate table [pred_defs] and program variables [pvars].
      It returns the appropriate predicate state and all learned bindings. *)
  val normalise_assertion :
    pred_defs:UP.preds_tbl_t ->
    init_data:SPState.init_data ->
    ?pvars:Utils.Containers.SS.t ->
    Asrt.t ->
    ((SPState.t * SVal.SESubst.t) list, string) result
end
