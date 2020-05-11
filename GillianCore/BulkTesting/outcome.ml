(** This is stupid, outcome is actually the interpreter. Doesn't really make sense, this should be changed. *)
module type S = sig
  module Val : Val.S

  module Subst : Subst.S with type vt = Val.t and type t = Val.st

  module Store : Store.S with type vt = Val.t

  module State :
    State.S
      with type vt = Val.t
       and type st = Subst.t
       and type store_t = Store.t

  module ParserAndCompiler : ParserAndCompiler.S

  module External : External.S

  type t =
    | ParseAndCompileError of ParserAndCompiler.err
    | FailedExec           of string
    | FinishedExec         of
        (State.t, Val.t, (Val.t, State.err_t) ExecErr.t) ExecRes.t list

  val pp_what_test_did : Format.formatter -> t -> unit

  val pp_what_branch_did :
    Format.formatter ->
    (State.t, Val.t, (Val.t, State.err_t) ExecErr.t) ExecRes.t ->
    unit
end

module Make
    (ValP : Val.S)
    (SubstP : Subst.S with type vt = ValP.t and type t = ValP.st)
    (StoreP : Store.S with type vt = ValP.t)
    (StateP : State.S
                with type vt = ValP.t
                 and type st = SubstP.t
                 and type store_t = StoreP.t)
    (PC : ParserAndCompiler.S)
    (ExternalP : External.S) :
  S
    with module Val = ValP
     and module Subst = SubstP
     and module Store = StoreP
     and module State = StateP
     and module ParserAndCompiler = PC
     and module External = ExternalP = struct
  module Val = ValP
  module ParserAndCompiler = PC
  module State = StateP
  module Subst = SubstP
  module Store = StoreP
  module External = ExternalP

  type t =
    | ParseAndCompileError of ParserAndCompiler.err
    | FailedExec           of string
    | FinishedExec         of
        (State.t, Val.t, (Val.t, State.err_t) ExecErr.t) ExecRes.t list

  let pp_what_test_did fmt = function
    | ParseAndCompileError e ->
        Fmt.pf fmt "failed at parsing time with error %a"
          ParserAndCompiler.pp_err e
    | FailedExec msg ->
        Fmt.pf fmt "failed at execution with message : \"%s\"" msg
    | FinishedExec [ RSucc _ ] ->
        Fmt.pf fmt "finished its execution successfully"
    | FinishedExec [ RFail (proc, i, _, errs) ] ->
        Fmt.pf fmt
          "finished its execution with failure in proc %s at command %i with \
           errors %a"
          proc i
          (Fmt.Dump.list (ExecErr.pp Val.pp State.pp_err))
          errs
    | FinishedExec _ ->
        Fmt.pf fmt "finished its execution with several branches"

  let pp_what_branch_did fmt b =
    ExecRes.pp_what_exec_did Val.pp (ExecErr.pp Val.pp State.pp_err) fmt b
end

module Make_Concrete (CMemory : CMemory.S) =
  Make (CVal.M) (CVal.CSubst) (CStore) (CState.Make (CMemory))
module Make_Symbolic (SMemory : SMemory.S) =
  Make (SVal.M) (SVal.SSubst) (SStore) (SState.Make (SMemory))
