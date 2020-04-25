module type S = sig
  (** Type of GIL values *)
  type vt = SVal.M.t

  (** Type of GIL substitutions *)
  type st = SVal.SSubst.t

  type i_fix_t

  type c_fix_t

  type err_t

  (** Type of GIL general states *)
  type t

  type action_ret =
    | ASucc of (t * vt list * Formula.t list * (string * Type.t) list) list
    | AFail of err_t list

  val init : unit -> t
  (** Initialisation *)

  val execute_action : string -> t -> PFS.t -> TypEnv.t -> vt list -> action_ret
  (** Execute action *)

  val ga_to_setter : string -> string

  val ga_to_getter : string -> string

  val ga_to_deleter : string -> string

  val ga_loc_indexes : string -> int list

  val is_overlapping_asrt : string -> bool

  val copy : t -> t
  (** State Copy *)

  val pp : Format.formatter -> t -> unit
  (** Printer *)

  val substitution_in_place : st -> t -> unit

  val fresh_val : t -> vt

  val clean_up : t -> unit

  val lvars : t -> Containers.SS.t

  val assertions : ?to_keep:Containers.SS.t -> t -> Asrt.t list

  val mem_constraints : t -> Formula.t list

  val pp_i_fix : Format.formatter -> i_fix_t -> unit

  val pp_c_fix : Format.formatter -> c_fix_t -> unit

  val get_recovery_vals : err_t -> vt list

  val pp_err : Format.formatter -> err_t -> unit

  val get_failing_constraint : err_t -> Formula.t

  val get_fixes :
    ?simple_fix:bool ->
    t ->
    PFS.t ->
    TypEnv.t ->
    err_t ->
    (c_fix_t list * Formula.t list * Containers.SS.t * Asrt.t list) list

  val apply_fix : t -> PFS.t -> TypEnv.t -> c_fix_t -> t
end
