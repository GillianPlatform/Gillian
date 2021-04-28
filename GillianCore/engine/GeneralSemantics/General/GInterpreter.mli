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

  type invariant_frames = (string * State.t) list

  type err_t = (vt, state_err_t) ExecErr.t

  type cconf_t =
    | ConfErr    of string * int * State.t * err_t list
    | ConfCont   of
        State.t * CallStack.t * invariant_frames * int * string list * int * int
    | ConfFinish of Flag.t * State.vt * State.t
        (** Equal to Conf cont + the id of the required spec *)
    | ConfSusp   of
        string
        * State.t
        * CallStack.t
        * invariant_frames
        * int
        * string list
        * int
        * int

  type conf_t = BConfErr of err_t list | BConfCont of State.t

  type result_t = (State.t, state_vt, err_t) ExecRes.t

  type 'a cont_func =
    | Finished of 'a list
    | Continue of (unit -> string option * 'a cont_func)

  type cmd_step = {
    call_stack : CallStack.t;
    proc_body_index : int;
    store : store_t option;
  }
  [@@deriving yojson]

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
