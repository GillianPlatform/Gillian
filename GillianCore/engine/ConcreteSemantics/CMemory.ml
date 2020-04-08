module type S = sig
  (** Type of GIL values *)
  type vt = CVal.M.t

  (** Type of GIL substitutions *)
  type st = CVal.CSubst.t

  (** Errors *)
  type err_t

  type fix_t

  (** Type of GIL general states *)
  type t

  type action_ret = ASucc of (t * vt list) | AFail of err_t list

  val init : unit -> t
  (** Initialisation *)

  val execute_action : string -> t -> vt list -> action_ret
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

  val pp_err : Format.formatter -> err_t -> unit
end
