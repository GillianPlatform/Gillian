(** @canonical Gillian.Concrete.Memory_S *)
module type S = sig
  type init_data

  (** Type of GIL values *)
  type vt = CVal.M.t

  (** Type of GIL substitutions *)
  type st = CVal.CSubst.t

  (** Errors *)
  type err_t [@@deriving yojson, show]

  (** Type of GIL general states *)
  type t

  type action_ret = (t * vt list, err_t) result

  (** Initialisation *)
  val init : init_data -> t

  (** Execute action *)
  val execute_action : string -> t -> vt list -> action_ret

  (** State Copy *)
  val copy : t -> t

  (** Printer *)
  val pp : Format.formatter -> t -> unit

  val pp_err : Format.formatter -> err_t -> unit
end
