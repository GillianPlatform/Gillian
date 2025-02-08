(** @canonical Gillian.General.State

  Interface for GIL General States.

  They are considered to be mutable. *)

(** @canonical Gillian.General.State.S *)
module type S = sig
  (** Type of GIL values *)
  type vt [@@deriving yojson, show]

  (** Type of GIL general states *)
  type t [@@deriving yojson]

  (** Type of GIL substitutions *)
  type st

  (** Type of GIL stores *)
  type store_t

  type heap_t

  (** Errors *)
  type m_err_t [@@deriving yojson]

  type err_t = (m_err_t, vt) StateErr.t [@@deriving yojson, show]

  exception Internal_State_Error of err_t list * t

  type action_ret = (t * vt list, err_t) Res_list.t
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]
  type init_data

  val execute_action : string -> t -> vt list -> action_ret
  val is_overlapping_asrt : string -> bool

  (** Expression Evaluation *)
  val eval_expr : t -> Expr.t -> vt

  (** Get store of given symbolic state *)
  val get_store : t -> store_t

  (** Set store of given symbolic state *)
  val set_store : t -> store_t -> t

  (** Assume expression *)
  val assume : ?unfold:bool -> t -> vt -> t list

  (** Assume assertion *)
  val assume_a :
    ?matching:bool ->
    ?production:bool ->
    ?time:string ->
    t ->
    Expr.t list ->
    t option

  (** Assume type *)
  val assume_t : t -> vt -> Type.t -> t option

  (** Satisfiability check *)
  val sat_check : t -> vt -> bool

  val sat_check_f : t -> Expr.t list -> st option

  (** Assert assertion *)
  val assert_a : t -> Expr.t list -> bool

  (** Value Equality *)
  val equals : t -> vt -> vt -> bool

  val get_type : t -> vt -> Type.t option

  (** State simplification *)
  val simplify :
    ?save:bool -> ?kill_new_lvars:bool -> ?matching:bool -> t -> st * t list

  (** Value simplification *)
  val simplify_val : t -> vt -> vt

  val fresh_loc : ?loc:vt -> t -> vt

  (** Printers *)
  val pp : Format.formatter -> t -> unit

  val pp_by_need :
    Var.Set.t -> LVar.Set.t -> Id.Sets.LocSet.t -> Format.formatter -> t -> unit

  val pp_err : Format.formatter -> err_t -> unit
  val get_recovery_tactic : t -> err_t list -> vt Recovery_tactic.t

  (** State Copy *)
  val copy : t -> t

  (** Add Spec Var *)
  val add_spec_vars : t -> Id.Sets.SubstSet.t -> t

  (* TODO: Is this right? Or are spec vars LVars? *)

  (** Get Spec Vars *)
  val get_spec_vars : t -> Id.Sets.SubstSet.t

  (** Get all logical variables *)
  val get_lvars : t -> LVar.Set.t

  (** Turns a state into a list of assertions *)
  val to_assertions : ?to_keep:Var.Set.t -> t -> Asrt.t

  val evaluate_slcmd : 'a MP.prog -> SLCmd.t -> t -> (t, err_t) Res_list.t

  (** [match_invariant prog revisited state invariant binders] returns a list of pairs of states.
      In each pair, the first element is the framed off state, and the second one is the invariant,
      i.e. the state obtained by producing the invariant *)
  val match_invariant :
    'a MP.prog ->
    bool ->
    t ->
    Asrt.t ->
    Id.any_var Id.t list ->
    (t * t, err_t) Res_list.t

  val frame_on : t -> (string * t) list -> string list -> (t, err_t) Res_list.t

  val run_spec :
    MP.spec ->
    t ->
    Var.t ->
    vt list ->
    (string * (LVar.t * vt) list) option ->
    (t * Flag.t, err_t) Res_list.t

  val sure_is_nonempty : t -> bool
  val unfolding_vals : t -> Expr.t list -> vt list
  val try_recovering : t -> vt Recovery_tactic.t -> (t list, string) result
  val substitution_in_place : ?subst_all:bool -> st -> t -> t list
  val clean_up : ?keep:Expr.Set.t -> t -> unit
  val match_assertion : t -> st -> MP.step -> (t, err_t) Res_list.t
  val produce_posts : t -> st -> Asrt.t list -> t list
  val produce : t -> st -> Asrt.t -> (t, err_t) Res_list.t
  val update_subst : t -> st -> unit
  val mem_constraints : t -> Expr.t list
  val can_fix : err_t -> bool
  val get_failing_constraint : err_t -> Expr.t
  val get_fixes : err_t -> Asrt.t list
  val get_equal_values : t -> vt list -> vt list
  val get_heap : t -> heap_t
end
