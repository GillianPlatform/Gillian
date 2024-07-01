(* preds *)
type t [@@deriving yojson]
type abs_t = string * Expr.t list

val length : t -> int
val init : abs_t list -> t
val to_list : t -> abs_t list
val copy : t -> t
val is_empty : t -> bool
val extend : ?pure:bool -> t -> abs_t -> unit
val pop : t -> (abs_t -> bool) -> abs_t option

(** Given a strategy finds a predicate best matching it (int is non-null and highest possible).
      If consume is true, and the function returns [Some _], the predicate is also removed from
      the preds. *)
val strategic_choice : consume:bool -> t -> (abs_t -> int) -> abs_t option

val remove_by_name : t -> string -> abs_t option
val find_pabs_by_name : t -> string -> abs_t list
val get_lvars : t -> SS.t
val get_alocs : t -> SS.t
val pp : Format.formatter -> t -> unit
val pp_pabs : Format.formatter -> abs_t -> unit

(** [consume_pred ~maintain pred_state name args_opt ins f_eq] removes the predicate best matching the triple
      [(name, args_opt, ins)] using an equality function on values [f_eq].
      It does so in place, and if maintain is true, the predicate is matched
        but not actually removed from the state.  *)
val consume_pred :
  maintain:bool ->
  t ->
  string ->
  Expr.t option list ->
  Containers.SI.t ->
  (Expr.t -> Expr.t -> bool) ->
  abs_t option

val find : t -> (abs_t -> bool) -> abs_t option
val get_all : maintain:bool -> (abs_t -> bool) -> t -> abs_t list
val substitution_in_place : SVal.SESubst.t -> t -> unit

(** Turns a predicate set into a list of assertions *)
val to_assertions : t -> Asrt.t list
