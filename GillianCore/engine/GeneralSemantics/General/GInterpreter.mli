module type S = sig
  module CallStack : CallStack.S

  type vt

  type st

  type store_t

  type state_t

  type state_err_t

  type state_vt

  module Val : Val.S with type t = vt

  module Store : Store.S with type t = store_t and type vt = vt

  module State :
    State.S
      with type t = state_t
       and type vt = vt
       and type st = st
       and type store_t = store_t

  type err_t = (vt, state_err_t) ExecErr.t

  type conf_t = BConfErr of err_t list | BConfCont of State.t

  type result_t = (State.t, state_vt, err_t) ExecRes.t

  type 'a cont_func =
    | Finished of 'a list
    | Continue of (unit -> CallStack.t * State.t * 'a cont_func)

  val pp_err : Format.formatter -> (vt, state_err_t) ExecErr.t -> unit

  val pp_result : Format.formatter -> result_t list -> unit

  val call_graph : CallGraph.t

  val reset : unit -> unit

  val evaluate_lcmds : UP.prog -> LCmd.t list -> State.t -> State.t list

  val init_evaluate_proc :
    (result_t -> 'a) ->
    UP.prog ->
    string ->
    string list ->
    State.t ->
    'a cont_func

  val evaluate_proc :
    (result_t -> 'a) -> UP.prog -> string -> string list -> State.t -> 'a list

  val evaluate_prog : UP.prog -> result_t list
end

(** General GIL Interpreter *)
module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (External : External.S) :
  S
    with type vt = Val.t
     and type st = ESubst.t
     and type store_t = Store.t
     and type state_t = State.t
     and type state_err_t = State.err_t
     and type state_vt = State.vt
