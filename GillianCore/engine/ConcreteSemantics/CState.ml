open Literal
open Containers

(*********************)
(*                   *)
(*  C States  *)
(*                   *)
(*********************)

module Make
    (CMemory : CMemory.S with type vt = CVal.M.t and type st = CVal.M.st) :
  State.S
    with type st = CVal.CESubst.t
     and type vt = Literal.t
     and type store_t = CStore.t = struct
  type vt = CVal.M.t

  type st = CVal.CESubst.t

  type store_t = CStore.t

  type t = CMemory.t * CStore.t * vt list

  type fix_t

  type m_err_t = CMemory.err_t

  type err_t = (m_err_t, vt) StateErr.err_t

  exception Internal_State_Error of err_t list * t

  type action_ret = ASucc of (t * vt list) list | AFail of err_t list

  type u_res = UWTF | USucc of t | UFail of err_t list

  let lift_merrs (errs : m_err_t list) : err_t list =
    List.map (fun x -> StateErr.EMem x) errs

  let init (pred_defs : UP.preds_tbl_t option) : t =
    (CMemory.init (), CStore.init [], [])

  let get_pred_defs (state : t) : UP.preds_tbl_t option = None

  let execute_action (action : string) (state : t) (args : vt list) : action_ret
      =
    let heap, store, locs = state in
    match CMemory.execute_action action heap args with
    | CMemory.ASucc (heap, vs) -> ASucc [ ((heap, store, locs), vs) ]
    | CMemory.AFail errs       -> AFail (lift_merrs errs)

  let ga_to_setter (a_id : string) = CMemory.ga_to_setter a_id

  let ga_to_getter (a_id : string) = CMemory.ga_to_getter a_id

  let ga_to_deleter (a_id : string) = CMemory.ga_to_deleter a_id

  let is_overlapping_asrt (a : string) : bool = CMemory.is_overlapping_asrt a

  let lift l = l

  let unlift l = Some l

  let eval_expr state e =
    let _, store, _ = state in
    CExprEval.evaluate_expr store e

  let get_store state =
    let _, store, _ = state in
    store

  let set_store state store =
    let heap, _, locs = state in
    (heap, store, locs)

  let to_loc (state : t) (loc : vt) : (t * vt) option =
    match loc with
    | Literal.Loc _ -> Some (state, loc)
    | _             -> None

  let get_locs state =
    let _, _, locs = state in
    locs

  let assume ?(unfold = false) (state : t) (l : Literal.t) : t list =
    match l with
    | Bool true  -> [ state ]
    | Bool false -> []
    | _          -> raise (Failure "assume. illegal argument to assume")

  let assume_a
      ?(unification = false)
      ?(production = false)
      (state : t)
      (ps : Formula.t list) : t option =
    let les : Expr.t option list = List.map Formula.to_expr ps in
    let bs : CVal.M.t option list =
      List.map (Option.map (eval_expr state)) les
    in
    if
      List.for_all
        (function
          | Some (Bool true) -> true
          | _                -> false)
        bs
    then Some state
    else None

  let assume_t (state : t) (v : vt) (t : Type.t) : t option =
    if Literal.type_of v = t then Some state else None

  let assert_a (state : t) (ps : Formula.t list) : bool =
    Option.fold ~some:(fun _ -> true) ~none:false (assume_a state ps)

  let sat_check (state : t) (l : Literal.t) : bool =
    match l with
    | Bool b -> b
    | _      -> raise (Failure "SAT Check: non-boolean argument")

  (* Implentation MISSING!!! *)
  let sat_check_f (state : t) (f : Formula.t list) : st option = None

  let pp fmt state =
    let heap, store, locs = state in
    let pp_heap fmt heap =
      if !Config.no_heap then Fmt.string fmt "HEAP NOT PRINTED"
      else CMemory.pp fmt heap
    in
    Fmt.pf fmt
      "-----------------------------------------@\n\
       @[<v 2>STORE:@\n\
       %a@]@\n\
       @[<v 2>HEAP:@\n\
       %a@]@\n"
      CStore.pp store pp_heap heap

  let copy state =
    let cheap, cstore, vts = state in
    (CMemory.copy cheap, CStore.copy cstore, vts)

  let equals state v1 v2 = v1 = v2

  let get_type state v = Some (Literal.type_of v)

  let simplify
      ?(save = false)
      ?(kill_new_lvars : bool option)
      ?(unification = false)
      (state : t) : st =
    CVal.CESubst.init []

  let simplify_val state v = v

  let struct_init
      (pred_defs : UP.preds_tbl_t option)
      (store : store_t)
      (pfs : PFS.t)
      (gamma : TypEnv.t)
      (svars : SS.t) : t =
    raise (Failure "ERROR. struct_init not supported at the concrete level")

  let add_spec_vars state xs =
    raise (Failure "ERROR: add_spec_var called for concrete executions")

  let get_spec_vars state =
    raise (Failure "ERROR: get_spec_vars called for concrete executions")

  let get_lvars state =
    raise (Failure "ERROR: get_lvars called for concrete executions")

  let to_assertions ?(to_keep : SS.t option) (state : t) : Asrt.t list =
    raise (Failure "ERROR: to_assertions called for concrete executions")

  let run_spec
      (spec : UP.spec)
      (state : t)
      (x : string)
      (args : vt list)
      (subst : (string * (string * vt) list) option) : (t * Flag.t) list =
    raise (Failure "ERROR: run_spec called for non-abstract execution")

  let unify (state : t) (subst : st) (up : UP.t) : bool =
    raise (Failure "ERROR: unify called for non-abstract execution")

  let unfolding_vals (state : t) (fs : Formula.t list) : vt list =
    raise (Failure "ERROR: unfolding_vals called for non-abstract execution")

  let evaluate_slcmd
      ?(revisited_invariant = false)
      (prog : UP.prog)
      (slcmd : SLCmd.t)
      (state : t) : (t list, string) result =
    raise (Failure "ERROR: evaluate_slcmd called for non-abstract execution")

  let substitution_in_place (subst : st) (state : t) : unit = ()

  let fresh_val (state : t) =
    raise (Failure "fresh_val not implemented in concrete state")

  let fresh_loc ?(loc : vt option) (state : t) =
    raise (Failure "fresh_loc not implemented in concrete state")

  let get_locs (state : t) =
    raise (Failure "Unsupported: get locs in a concrete state")

  let get_locs_and_props (state : t) =
    raise (Failure "Unsupported: get locs and props in a concrete state")

  let clean_up (state : t) = raise (Failure "Cleanup of concrete state.")

  let unify_assertion (state : t) (subst : st) (step : UP.step) : u_res =
    raise (Failure "Unify assertion from concrete state.")

  let produce_posts (state : t) (subst : st) (asrts : Asrt.t list) : t list =
    raise (Failure "produce_posts from concrete state.")

  let produce (state : t) (subst : st) (asrt : Asrt.t) : t option =
    raise (Failure "produce_post from non-abstract symbolic state.")

  let update_subst (state : t) (subst : st) : unit = ()

  let mem_constraints (state : t) : Formula.t list =
    raise (Failure "DEATH. mem_constraints")

  let split_ins (action : string) (ins : vt list) : vt list * vt list =
    raise (Failure "DEATH. split_ins")

  let merge_ins (action : string) (l_ins : vt list) (o_ins : vt list) : vt list
      =
    raise (Failure "merge_ins. DEATH")

  let pp_fix _ _ = raise (Failure "str_of_fix from non-symbolic state.")

  let get_recovery_vals _ =
    raise (Failure "get_recovery_vals from non-symbolic state.")

  let automatic_unfold _ _ : (t list, string) result =
    Error "Automatic unfold not supported in concrete execution"

  let pp_err fmt (err : err_t) : unit =
    match err with
    | EMem m_err        -> CMemory.pp_err fmt m_err
    | EType (v, t1, t2) ->
        Fmt.pf fmt "EType(%a, %a, %s)" CVal.M.pp v
          (Fmt.option ~none:(Fmt.any "None") (Fmt.of_to_string Type.str))
          t1 (Type.str t2)
    | _                 ->
        raise
          (Exceptions.Unsupported
             "Concrete printer: non-memory and non-type error")

  let can_fix (errs : err_t list) : bool = false

  let get_failing_constraint (err : err_t) : Formula.t = True

  let get_fixes ?simple_fix:(sf = true) (state : t) (errs : err_t list) :
      fix_t list list =
    raise (Failure "Concrete: get_fixes not implemented in CState.Make")

  let apply_fixes (state : t) (fixes : fix_t list) : t option * Asrt.t list =
    raise (Failure "Concrete: apply_fixes not implemented in CState.Make")
end
