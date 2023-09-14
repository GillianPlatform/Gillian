open Literal

(*********************)
(*                   *)
(*  C States  *)
(*                   *)
(*********************)

module type S = sig
  include
    State.S
      with type st = CVal.CESubst.t
       and type vt = Literal.t
       and type store_t = CStore.t

  val init : init_data -> t
end

module Make
    (CMemory : CMemory.S with type vt = CVal.M.t and type st = CVal.M.st) : sig
  include
    State.S
      with type st = CVal.CESubst.t
       and type vt = Literal.t
       and type store_t = CStore.t
       and type init_data = CMemory.init_data

  val init : init_data -> t
end = struct
  type vt = CVal.M.t [@@deriving yojson, show]
  type st = CVal.CESubst.t
  type store_t = CStore.t
  type heap_t = CMemory.t
  type t = CMemory.t * CStore.t * vt list
  type fix_t
  type m_err_t = CMemory.err_t [@@deriving show]
  type err_t = (m_err_t, vt) StateErr.t [@@deriving show]
  type init_data = CMemory.init_data
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]

  exception Internal_State_Error of err_t list * t

  type action_ret = (t * vt list, err_t) result list

  let init init_data : t = (CMemory.init init_data, CStore.init [], [])
  let get_pred_defs (_ : t) : UP.preds_tbl_t option = None

  let execute_action
      ?unification:_
      (action : string)
      (state : t)
      (args : vt list) : action_ret =
    let heap, store, locs = state in
    match CMemory.execute_action action heap args with
    | Ok (heap, vs) -> [ Ok ((heap, store, locs), vs) ]
    | Error err -> [ Error (StateErr.EMem err) ]

  let ga_to_setter _ = failwith "ga_to_setter for CState"
  let ga_to_getter _ = failwith "ga_to_getter for CState"
  let ga_to_deleter _ = failwith "ga_to_deleter for CState"
  let is_overlapping_asrt _ = failwith "is_overlapping_assert for CState"

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
    | _ -> None

  let assume ?unfold:_ (state : t) (l : Literal.t) : t list =
    match l with
    | Bool true -> [ state ]
    | Bool false -> []
    | _ -> raise (Failure "assume. illegal argument to assume")

  let assume_a
      ?unification:_
      ?production:_
      ?time:_
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
          | _ -> false)
        bs
    then Some state
    else None

  let assume_t (state : t) (v : vt) (t : Type.t) : t option =
    if Literal.type_of v = t then Some state else None

  let assert_a (state : t) (ps : Formula.t list) : bool =
    Option.fold ~some:(fun _ -> true) ~none:false (assume_a state ps)

  let sat_check (_ : t) (l : Literal.t) : bool =
    match l with
    | Bool b -> b
    | _ -> raise (Failure "SAT Check: non-boolean argument")

  (* Implentation MISSING!!! *)
  let sat_check_f (_ : t) (_ : Formula.t list) : st option = None

  let pp fmt state =
    let heap, store, _ = state in
    let pp_heap fmt heap =
      if !Config.no_heap then Fmt.string fmt "HEAP NOT PRINTED"
      else CMemory.pp fmt heap
    in
    Fmt.pf fmt
      "-----------------------------------------@\n\
       @[<v 2>STORE:@\n\
       %a@]@\n\
       @[<v 2>MEMORY:@\n\
       %a@]@\n"
      CStore.pp store pp_heap heap

  (* TODO: By-need formatter *)
  let pp_by_need _ _ _ fmt state = pp fmt state

  let copy state =
    let cheap, cstore, vts = state in
    (CMemory.copy cheap, CStore.copy cstore, vts)

  let equals _ v1 v2 = v1 = v2
  let get_type _ v = Some (Literal.type_of v)

  let simplify ?save:_ ?kill_new_lvars:_ ?unification:_ (state : t) :
      st * t list =
    (CVal.CESubst.init [], [ state ])

  let simplify_val _ v = v

  let add_spec_vars _ _ =
    raise (Failure "ERROR: add_spec_var called for concrete executions")

  let get_spec_vars _ =
    raise (Failure "ERROR: get_spec_vars called for concrete executions")

  let get_lvars _ =
    raise (Failure "ERROR: get_lvars called for concrete executions")

  let to_assertions ?to_keep:_ (_ : t) : Asrt.t list =
    raise (Failure "ERROR: to_assertions called for concrete executions")

  let run_spec
      (_ : UP.spec)
      (_ : t)
      (_ : string)
      (_ : vt list)
      (_ : (string * (string * vt) list) option) =
    raise (Failure "ERROR: run_spec called for non-abstract execution")

  let unfolding_vals (_ : t) (_ : Formula.t list) : vt list =
    raise (Failure "ERROR: unfolding_vals called for non-abstract execution")

  let evaluate_slcmd (_ : 'a UP.prog) (_ : SLCmd.t) (_ : t) :
      (t, err_t) Res_list.t =
    raise (Failure "ERROR: evaluate_slcmd called for non-abstract execution")

  let unify_invariant _ _ _ _ _ =
    raise (Failure "ERROR: unify_invariant called for concrete execution")

  let frame_on _ _ _ =
    raise (Failure "ERROR: framing called for concrete execution")

  let substitution_in_place ?subst_all:_ (_ : st) (_ : t) : t list = []

  let fresh_loc ?loc:_ (_ : t) =
    raise (Failure "fresh_loc not implemented in concrete state")

  let clean_up ?keep:_ _ = raise (Failure "Cleanup of concrete state.")

  let unify_assertion (_ : t) (_ : st) (_ : UP.step) =
    raise (Failure "Unify assertion from concrete state.")

  let produce_posts (_ : t) (_ : st) (_ : Asrt.t list) : t list =
    raise (Failure "produce_posts from concrete state.")

  let produce (_ : t) (_ : st) (_ : Asrt.t) =
    raise (Failure "produce_post from non-abstract symbolic state.")

  let update_subst (_ : t) (_ : st) : unit = ()

  let mem_constraints (_ : t) : Formula.t list =
    raise (Failure "DEATH. mem_constraints")

  let pp_fix _ _ = raise (Failure "str_of_fix from non-symbolic state.")

  let get_recovery_tactic _ =
    raise (Failure "get_recovery_tactic from non-symbolic state.")

  let try_recovering _ _ : (t list, string) result =
    Error "try_recovering not supported in concrete execution"

  let pp_err fmt (err : err_t) : unit =
    match err with
    | EMem m_err -> CMemory.pp_err fmt m_err
    | EType (v, t1, t2) ->
        Fmt.pf fmt "EType(%a, %a, %s)" CVal.M.pp v
          (Fmt.option ~none:(Fmt.any "None") (Fmt.of_to_string Type.str))
          t1 (Type.str t2)
    | _ ->
        raise
          (Exceptions.Unsupported
             "Concrete printer: non-memory and non-type error")

  let can_fix (_ : err_t) : bool = false
  let get_failing_constraint (_ : err_t) : Formula.t = True

  let get_fixes (_ : t) (_ : err_t) : fix_t list list =
    raise (Failure "Concrete: get_fixes not implemented in CState.Make")

  let apply_fixes (_ : t) (_ : fix_t list) : t list =
    raise (Failure "Concrete: apply_fixes not implemented in CState.Make")

  let get_equal_values _ vs = vs

  let get_heap state =
    let heap, _, _ = state in
    heap

  let of_yojson _ =
    failwith
      "Please implement of_yojson to enable logging this type to a database"

  let to_yojson _ =
    failwith
      "Please implement to_yojson to enable logging this type to a database"

  let err_t_of_yojson _ =
    failwith
      "Please implement err_t_of_yojson to enable logging this type to a \
       database"

  let err_t_to_yojson _ =
    failwith
      "Please implement err_t_to_yojson to enable logging this type to a \
       database"
end
