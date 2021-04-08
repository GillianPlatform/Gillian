module type S = sig
  val reset : unit -> unit

  val verify_prog :
    (Annot.t, int) Prog.t -> bool -> SourceFiles.t option -> unit
end

module Make
    (SState : State.S
                with type vt = SVal.M.t
                 and type st = SVal.SESubst.t
                 and type store_t = SStore.t)
    (SPState : PState.S
                 with type vt = SVal.M.t
                  and type st = SVal.SESubst.t
                  and type store_t = SStore.t
                  and type preds_t = Preds.SPreds.t)
    (External : External.S) : S
