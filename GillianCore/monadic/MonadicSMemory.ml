open Utils
open Engine
open Gil_syntax

module type S = sig
  type init_data

  (** Type of GIL values *)
  type vt = SVal.M.t

  (** Type of GIL substitutions *)
  type st = SVal.SESubst.t

  type c_fix_t
  type err_t [@@deriving show]

  (** Type of GIL general states *)
  type t [@@deriving yojson]

  type action_ret = (t * vt list, err_t) result

  (** Initialisation *)
  val init : init_data -> t

  val clear : t -> t

  (** Execute action *)
  val execute_action :
    action_name:string -> t -> vt list -> action_ret Delayed.t

  val ga_to_setter : string -> string
  val ga_to_getter : string -> string
  val ga_to_deleter : string -> string
  val is_overlapping_asrt : string -> bool

  (** State Copy *)
  val copy : t -> t

  (** Printer *)
  val pp : Format.formatter -> t -> unit

  val substitution_in_place : st -> t -> t Delayed.t
  val clean_up : ?keep:Expr.Set.t -> t -> Expr.Set.t * Expr.Set.t
  val lvars : t -> Containers.SS.t
  val alocs : t -> Containers.SS.t
  val assertions : ?to_keep:Containers.SS.t -> t -> Asrt.t list
  val mem_constraints : t -> Formula.t list
  val pp_c_fix : Format.formatter -> c_fix_t -> unit
  val get_recovery_tactic : t -> err_t -> vt Recovery_tactic.t
  val pp_err : Format.formatter -> err_t -> unit
  val get_failing_constraint : err_t -> Formula.t

  val get_fixes :
    t ->
    PFS.t ->
    Type_env.t ->
    err_t ->
    (c_fix_t list * Formula.t list * (string * Type.t) list * Containers.SS.t)
    list

  val can_fix : err_t -> bool
  val apply_fix : t -> c_fix_t -> (t, err_t) result Delayed.t
  val pp_by_need : Containers.SS.t -> Format.formatter -> t -> unit
  val get_print_info : Containers.SS.t -> t -> Containers.SS.t * Containers.SS.t
  val can_fix : err_t -> bool
end

(* FIXME: Lift should not be necessary, the monad should just match !!! *)
module Lift (MSM : S) :
  SMemory.S with type t = MSM.t and type init_data = MSM.init_data = struct
  include MSM

  let assertions ?to_keep t =
    List.map Engine.Reduction.reduce_assertion (assertions ?to_keep t)

  let execute_action action_name mem gpc params =
    let open Syntaxes.List in
    let process = execute_action ~action_name mem params in
    let curr_pc = Pc.of_gpc gpc in
    let+ Branch.{ pc; value } = Delayed.resolve ~curr_pc process in
    let gpc = Pc.to_gpc pc in
    Gbranch.{ pc = gpc; value }

  let apply_fix heap pfs gamma fix =
    Logging.verbose (fun m -> m "Bi-abduction trying to apply fix");
    let res = apply_fix heap fix in
    let curr_pc = Pc.make ~unification:false ~pfs ~gamma () in
    let results = Delayed.resolve ~curr_pc res in

    List.filter_map
      (function
        | Branch.{ pc; value = Ok x } ->
            let gpc = Pc.to_gpc pc in
            Some Gbranch.{ pc = gpc; value = x }
        | _ -> None)
      results

  let substitution_in_place ~pfs ~gamma subst mem :
      (t * Formula.Set.t * (string * Type.t) list) list =
    let process = substitution_in_place subst mem in
    let curr_pc = Pc.make ~unification:false ~pfs ~gamma () in
    match Delayed.resolve ~curr_pc process with
    | [] -> failwith "substitution killed every branch, that cannot happen"
    | leeloo_dallas_multibranch ->
        List.map
          (fun (bch : t Branch.t) ->
            (bch.value, bch.pc.learned, bch.pc.learned_types))
          leeloo_dallas_multibranch

  let of_yojson = MSM.of_yojson
  let to_yojson = MSM.to_yojson

  let err_t_of_yojson _ =
    failwith
      "Please implement err_t_of_yojson to enable logging this type to a \
       database"

  let err_t_to_yojson _ = `String "MonadicSMemory.Lift: dummy yojson!"
end
