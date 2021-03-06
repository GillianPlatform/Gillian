module type S = sig
  type vt

  type st

  type err_t

  type state_t

  type preds_t

  type t = state_t * preds_t * UP.preds_tbl_t

  type post_res = (Flag.t * Asrt.t list) option

  type search_state = (t * st * UP.t) list * err_t list

  type up_u_res = UPUSucc of (t * st * post_res) list | UPUFail of err_t list

  type gp_ret = GPSucc of (t * vt list) list | GPFail of err_t list

  type u_res = UWTF | USucc of t | UFail of err_t list

  type unfold_info_t = (string * string) list

  val produce_assertion : t -> st -> Asrt.t -> (t list, string) result

  val produce : t -> st -> Asrt.t -> (t list, string) result

  val produce_posts : t -> st -> Asrt.t list -> t list

  val unfold : t -> string -> vt list -> unfold_info_t option -> (st * t) list

  val rec_unfold : ?fuel:int -> t -> string -> vt list -> t list

  val unfold_all : t -> string -> t list

  val unfold_with_vals : t -> vt list -> (st * t) list * bool

  val unfold_concrete_preds : t -> (st option * t) option

  val unify_assertion : t -> st -> UP.step -> u_res

  val unify_up : search_state -> up_u_res

  val unify : ?in_unification:bool -> t -> st -> UP.t -> up_u_res

  val get_pred : ?in_unification:bool -> t -> string -> vt option list -> gp_ret
end

module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (Preds : Preds.S with type vt = Val.t and type st = ESubst.t) :
  S
    with type vt = Val.t
     and type st = ESubst.t
     and type state_t = State.t
     and type preds_t = Preds.t
     and type err_t = State.err_t
