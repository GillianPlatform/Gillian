open Utils
open Engine
open Gil_syntax

module type S = sig
  type init_data

  (** Type of GIL values *)
  type vt = SVal.M.t

  (** Type of GIL substitutions *)
  type st = SVal.SESubst.t

  type err_t [@@deriving show, yojson]

  (** Type of GIL general states *)
  type t [@@deriving yojson]

  type action_ret = (t * vt list, err_t) result

  (** Initialisation *)
  val init : init_data -> t

  val get_init_data : t -> init_data
  val clear : t -> t

  (** Execute action *)
  val execute_action :
    action_name:string -> t -> vt list -> action_ret Delayed.t

  val consume : core_pred:string -> t -> vt list -> action_ret Delayed.t
  val produce : core_pred:string -> t -> vt list -> t Delayed.t
  val is_overlapping_asrt : string -> bool

  (** State Copy *)
  val copy : t -> t

  (** Printer *)
  val pp : Format.formatter -> t -> unit

  val substitution_in_place : st -> t -> t Delayed.t
  val clean_up : ?keep:Expr.Set.t -> t -> Expr.Set.t * Expr.Set.t
  val lvars : t -> Containers.SS.t
  val alocs : t -> Containers.SS.t
  val assertions : ?to_keep:Containers.SS.t -> t -> Asrt.t
  val mem_constraints : t -> Formula.t list
  val get_recovery_tactic : t -> err_t -> vt Recovery_tactic.t
  val pp_err : Format.formatter -> err_t -> unit
  val get_failing_constraint : err_t -> Formula.t
  val get_fixes : err_t -> Asrt.t list
  val can_fix : err_t -> bool
  val pp_by_need : Containers.SS.t -> Format.formatter -> t -> unit
  val get_print_info : Containers.SS.t -> t -> Containers.SS.t * Containers.SS.t
  val sure_is_nonempty : t -> bool

  (** See {!val:SMemory.S.split_further} *)
  val split_further :
    t -> string -> vt list -> err_t -> (vt list list * vt list) option
end

(* FIXME: Lift should not be necessary, the monad should just match !!! *)
module Lift (MSM : S) :
  SMemory.S
    with type t = MSM.t
     and type err_t = MSM.err_t
     and type init_data = MSM.init_data = struct
  include MSM

  let assertions ?to_keep t =
    Engine.Reduction.reduce_assertion (assertions ?to_keep t)

  let execute_action action_name mem gpc params =
    let open Syntaxes.List in
    let process = execute_action ~action_name mem params in
    let curr_pc = Pc.of_gpc gpc in
    let+ Branch.{ pc; value } = Delayed.resolve ~curr_pc process in
    let gpc = Pc.to_gpc pc in
    Gbranch.{ pc = gpc; value }

  let consume core_pred mem gpc params =
    let open Syntaxes.List in
    let process = consume ~core_pred mem params in
    let curr_pc = Pc.of_gpc gpc in
    let+ Branch.{ pc; value } = Delayed.resolve ~curr_pc process in
    let gpc = Pc.to_gpc pc in
    Gbranch.{ pc = gpc; value }

  let produce core_pred mem gpc params =
    let open Syntaxes.List in
    let process = produce ~core_pred mem params in
    let curr_pc = Pc.of_gpc gpc in
    let+ Branch.{ pc; value } = Delayed.resolve ~curr_pc process in
    let gpc = Pc.to_gpc pc in
    Gbranch.{ pc = gpc; value }

  let substitution_in_place ~pfs ~gamma subst mem :
      (t * Formula.Set.t * (string * Type.t) list) list =
    let process = substitution_in_place subst mem in
    let curr_pc = Pc.make ~matching:false ~pfs ~gamma () in
    match Delayed.resolve ~curr_pc process with
    | [] -> failwith "substitution killed every branch, that cannot happen"
    | leeloo_dallas_multibranch ->
        List.map
          (fun (bch : t Branch.t) ->
            (bch.value, bch.pc.learned, bch.pc.learned_types))
          leeloo_dallas_multibranch

  let of_yojson = MSM.of_yojson
  let to_yojson = MSM.to_yojson
end
