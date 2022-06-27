open Utils
open Engine
open Gil_syntax

module type S = sig
  (** Type of GIL values *)
  type vt = SVal.M.t

  (** Type of GIL substitutions *)
  type st = SVal.SESubst.t

  type c_fix_t
  type err_t

  (** Type of GIL general states *)
  type t [@@deriving yojson]

  type action_ret = Success of (t * vt list) | Failure of err_t

  (** Initialisation *)
  val init : unit -> t

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
  val fresh_val : t -> vt
  val clean_up : ?keep:Expr.Set.t -> t -> Expr.Set.t * Expr.Set.t
  val lvars : t -> Containers.SS.t
  val alocs : t -> Containers.SS.t
  val assertions : ?to_keep:Containers.SS.t -> t -> Asrt.t list
  val mem_constraints : t -> Formula.t list
  val pp_c_fix : Format.formatter -> c_fix_t -> unit
  val get_recovery_vals : t -> err_t -> vt list
  val pp_err : Format.formatter -> err_t -> unit
  val get_failing_constraint : err_t -> Formula.t

  (* FIXME: This is not working *)
  val get_fixes :
    ?simple_fix:bool ->
    t ->
    PFS.t ->
    TypEnv.t ->
    err_t ->
    (c_fix_t list * Formula.t list * Containers.SS.t * Asrt.t list) list

  val apply_fix : t -> PFS.t -> TypEnv.t -> c_fix_t -> t
  val pp_by_need : Containers.SS.t -> Format.formatter -> t -> unit
  val get_print_info : Containers.SS.t -> t -> Containers.SS.t * Containers.SS.t
end

module Lift (MSM : S) : SMemory.S with type t = MSM.t = struct
  include MSM

  let assertions ?to_keep t =
    List.map Engine.Reduction.reduce_assertion (assertions ?to_keep t)

  type action_ret =
    | ASucc of (t * vt list * Formula.t list * (string * Type.t) list) list
    | AFail of err_t list

  let execute_action ?(unification = false) action_name mem pfs gamma params =
    let process = execute_action ~action_name mem params in
    let pfs = PFS.copy pfs in
    let gamma = TypEnv.copy gamma in
    (* let _ = Simplifications.simplify_pfs_and_gamma ~unification ~kill_new_lvars:true ~save_spec_vars:(Containers.SS.empty, true) pfs gamma in *)
    let curr_pc = Pc.make ~unification ~pfs ~gamma () in
    let results = Delayed.resolve ~curr_pc process in
    let split res =
      let rec aux acc_succ acc_fail res =
        match res with
        | [] -> (acc_succ, acc_fail)
        | br :: rest -> (
            match Branch.value br with
            | Failure err -> aux acc_succ (err :: acc_fail) rest
            | Success s ->
                aux
                  ((Branch.learned br, Branch.learned_types br, s) :: acc_succ)
                  acc_fail rest)
      in
      aux [] [] res
    in
    let successes, failures = split results in
    let is_empty list = Int.equal (List.compare_length_with list 0) 0 in
    if not (is_empty failures) then AFail failures
    else
      let asucs =
        List.map
          (fun (fset, glis, (t, vtl)) ->
            (t, vtl, List.of_seq (Formula.Set.to_seq fset), glis))
          successes
      in
      ASucc asucs

  let substitution_in_place ~pfs ~gamma subst mem :
      (t * Formula.Set.t * (string * Type.t) list) list =
    let process = substitution_in_place subst mem in
    let curr_pc = Pc.make ~pfs ~gamma () in
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
