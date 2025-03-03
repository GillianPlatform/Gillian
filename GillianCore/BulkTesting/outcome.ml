(** This is stupid, outcome is actually the interpreter. Doesn't really make sense, this should be changed. *)
module type S = sig
  module Val : Val.S
  module ESubst : ESubst.S with type vt = Val.t and type t = Val.et
  module Store : Store.S with type vt = Val.t

  module State : sig
    include
      State.S
        with type vt = Val.t
         and type st = ESubst.t
         and type store_t = Store.t

    val init : init_data -> t
  end

  module ParserAndCompiler :
    ParserAndCompiler.S with type init_data = State.init_data

  module External : External.T(ParserAndCompiler.Annot).S

  type exec_res =
    (State.t, Val.t, (Val.t, State.err_t) Exec_err.t) Exec_res.t list

  type t = exec_res Gillian_result.t

  val pp_what_test_did : Format.formatter -> t -> unit

  val pp_what_branch_did :
    Format.formatter ->
    (State.t, Val.t, (Val.t, State.err_t) Exec_err.t) Exec_res.t ->
    unit
end

module Make
    (ValP : Val.S)
    (ESubstP : ESubst.S with type vt = ValP.t and type t = ValP.et)
    (StoreP : Store.S with type vt = ValP.t)
    (StateP : sig
      include
        State.S
          with type vt = ValP.t
           and type st = ESubstP.t
           and type store_t = StoreP.t

      val init : init_data -> t
    end)
    (PC : ParserAndCompiler.S with type init_data = StateP.init_data)
    (ExternalP : External.T(PC.Annot).S) :
  S
    with module Val = ValP
     and module ESubst = ESubstP
     and module Store = StoreP
     and module State = StateP
     and module ParserAndCompiler = PC
     and module External = ExternalP = struct
  module Val = ValP
  module ParserAndCompiler = PC
  module State = StateP
  module ESubst = ESubstP
  module Store = StoreP
  module External = ExternalP

  type exec_res =
    (State.t, Val.t, (Val.t, State.err_t) Exec_err.t) Exec_res.t list

  type t = exec_res Gillian_result.t

  let pp_what_test_did fmt : t -> unit = function
    | Error e -> Gillian_result.Error.pp fmt e
    | Ok [ RSucc _ ] -> Fmt.pf fmt "finished its execution successfully"
    | Ok [ RFail { proc; proc_idx; errors; _ } ] ->
        Fmt.pf fmt
          "finished its execution with failure in proc %s at command %i with \
           errors: %a"
          proc proc_idx
          (Fmt.Dump.list (Exec_err.pp Val.pp State.pp_err))
          errors
    | Ok _ -> Fmt.pf fmt "finished its execution with several branches"

  let pp_what_branch_did fmt b =
    Exec_res.pp_what_exec_did Val.pp (Exec_err.pp Val.pp State.pp_err) fmt b
end

module Make_Concrete (CMemory : CMemory.S) =
  Make (CVal.M) (CVal.CESubst) (CStore) (CState.Make (CMemory))

module Make_Symbolic (SMemory : SMemory.S) =
  Make (SVal.M) (SVal.SESubst) (SStore) (SState.Make (SMemory))
