module Make
    (SPState : PState.S
                 with type vt = SVal.M.t
                  and type st = SVal.SSubst.t
                  and type store_t = SStore.t
                  and type preds_t = Preds.SPreds.t) : sig
  val normalise_assertion :
    ?pred_defs:UP.preds_tbl_t ->
    ?gamma:TypEnv.t ->
    ?subst:SVal.SSubst.t ->
    ?pvars:Utils.Containers.SS.t ->
    Asrt.t ->
    (SPState.t * SVal.SSubst.t) option
  (** [normalise_assertion ?pred_defs ?gamma ?subst ?pvars a] normalises the
      assertion [a] starting from the typing environment [gamma] and bindings [subst],
      considering the predicate table [pred_defs] and program variables [pvars].
      It returns the appropriate predicate state and all learned bindings. *)
end
