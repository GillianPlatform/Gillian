(**
    Interface for GIL General States.
    They are considered to be mutable.
*)

module type S = sig
  (** Type of GIL values *)
  type vt

  (** Type of GIL general states *)
  type t

  (** Type of GIL substitutions *)
  type st

  (** Type of GIL stores *)
  type store_t

  (** Errors *)
  type m_err_t

  type err_t = (m_err_t, vt) StateErr.err_t

  type fix_t

  exception Internal_State_Error of err_t list * t

  type action_ret = ASucc of (t * vt list) list | AFail of err_t list

  type u_res = UWTF | USucc of t | UFail of err_t list

  val init : UP.preds_tbl_t option -> t
  (** Initialisation *)

  val struct_init :
    UP.preds_tbl_t option ->
    store_t ->
    PFS.t ->
    TypEnv.t ->
    Containers.SS.t ->
    t

  val execute_action : string -> t -> vt list -> action_ret

  val ga_to_setter : string -> string

  val ga_to_getter : string -> string

  val ga_to_deleter : string -> string

  val get_pred_defs : t -> UP.preds_tbl_t option

  val is_overlapping_asrt : string -> bool

  val eval_expr : t -> Expr.t -> vt
  (** Expression Evaluation *)

  val get_store : t -> store_t
  (** Get store of given symbolic state *)

  val set_store : t -> store_t -> t
  (** Set store of given symbolic state *)

  val assume : ?unfold:bool -> t -> vt -> t list
  (** Assume expression *)

  val assume_a :
    ?unification:bool -> ?production:bool -> t -> Formula.t list -> t option
  (** Assume assertion *)

  val assume_t : t -> vt -> Type.t -> t option
  (** Assume type *)

  val sat_check : t -> vt -> bool
  (** Satisfiability check *)

  val sat_check_f : t -> Formula.t list -> st option

  val assert_a : t -> Formula.t list -> bool
  (** Assert assertion *)

  val equals : t -> vt -> vt -> bool
  (** Value Equality *)

  val get_type : t -> vt -> Type.t option
  (** Value Type *)

  val simplify :
    ?save:bool -> ?kill_new_lvars:bool -> ?unification:bool -> t -> st
  (** State simplification *)

  val simplify_val : t -> vt -> vt
  (** Value simplification *)

  val to_loc : t -> vt -> (t * vt) option
  (** Convert value to object location, with possible allocation *)

  val fresh_loc : ?loc:vt -> t -> vt

  val pp : Format.formatter -> t -> unit
  (** Printer *)

  val pp_err : Format.formatter -> err_t -> unit

  val pp_fix : Format.formatter -> fix_t -> unit

  val get_recovery_vals : err_t list -> vt list

  val copy : t -> t
  (** State Copy *)

  val add_spec_vars : t -> Var.Set.t -> t
  (** Add Spec Var *)

  val get_spec_vars : t -> Var.Set.t
  (** Get Spec Vars *)

  val get_lvars : t -> Var.Set.t
  (** Get all logical variables *)

  val to_assertions : ?to_keep:Containers.SS.t -> t -> Asrt.t list
  (** Turns a state into a list of assertions *)

  val evaluate_slcmd : UP.prog -> SLCmd.t -> t -> t list

  val run_spec :
    UP.spec ->
    t ->
    string ->
    vt list ->
    (string * (string * vt) list) option ->
    (t * Flag.t) list

  val unfolding_vals : t -> Formula.t list -> vt list

  val automatic_unfold : t -> vt list -> (t list, string) result

  val substitution_in_place : st -> t -> unit

  val fresh_val : t -> vt

  val clean_up : t -> unit

  val unify_assertion : t -> st -> Asrt.t -> u_res

  val produce_posts : t -> st -> Asrt.t list -> t list

  val produce : t -> st -> Asrt.t -> t option

  val update_subst : t -> st -> unit

  val split_ins : string -> vt list -> vt list * vt list

  val merge_ins : string -> vt list -> vt list -> vt list

  val mem_constraints : t -> Formula.t list

  val can_fix : err_t list -> bool

  val get_failing_constraint : err_t -> Formula.t

  val get_fixes : ?simple_fix:bool -> t -> err_t list -> fix_t list list

  val apply_fixes : t -> fix_t list -> t option * Asrt.t list
end
