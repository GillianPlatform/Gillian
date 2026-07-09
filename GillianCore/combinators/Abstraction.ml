open Utils
open Monadic
open Gil_syntax
open Engine
open Engine.Symbolic
module DR = Delayed_result
module L = Logging
module W = Matching_walker

type wand = Wands.wand
type upred = Preds.abs_t
type 'a abs = { mem : 'a; preds : upred list; wands : wand list }

let pp_abs pp_state fmt (astate : 'a abs) : unit =
  let { mem; preds; wands; _ } = astate in
  Fmt.pf fmt "%a@\n@[<v 2>PREDICATES:@\n%a@]@\n@[<v 2>WANDS:@\n%a@]@\n" pp_state
    mem
    (Fmt.list ~sep:(Fmt.any "@\n") Preds.pp_pabs)
    preds
    (Fmt.list ~sep:(Fmt.any "@\n") Wands.pp_wand)
    wands

let preds_to_yojson : upred list -> Yojson.Safe.t =
  [%to_yojson: (string * Expr.t list) list]

let preds_of_yojson : Yojson.Safe.t -> (upred list, string) result =
  [%of_yojson: (string * Expr.t list) list]

let wands_to_yojson : wand list -> Yojson.Safe.t = [%to_yojson: Wands.wand list]

let wands_of_yojson : Yojson.Safe.t -> (wand list, string) result =
  [%of_yojson: Wands.wand list]

let abs_to_yojson state_to_yojson (astate : 'a abs) : Yojson.Safe.t =
  (* TODO: Serialize other components of pstate *)
  let { mem; preds; wands; _ } = astate in
  `Assoc
    [
      ("state", state_to_yojson mem);
      ("preds", preds_to_yojson preds);
      ("wands", wands_to_yojson wands);
    ]

let abs_of_yojson state_of_yojson (yojson : Yojson.Safe.t) :
    ('a abs, string) result =
  (* TODO: Deserialize other components of pstate *)
  let open Syntaxes.Result in
  let rec aux = function
    | Some mem, Some preds, Some wands, [] -> Ok { mem; preds; wands }
    | None, preds, wands, ("state", state_yojson) :: rest ->
        let* state = state_of_yojson state_yojson in
        aux (Some state, preds, wands, rest)
    | state, None, wands, ("preds", preds_yojson) :: rest ->
        let* preds = preds_of_yojson preds_yojson in
        aux (state, Some preds, wands, rest)
    | state, preds, None, ("wands", wands_yojson) :: rest ->
        let* wands = wands_of_yojson wands_yojson in
        aux (state, preds, Some wands, rest)
    | _ -> Error "Cannot parse yojson into abstract state"
  in
  match yojson with
  | `Assoc sections -> aux (None, None, None, sections)
  | _ -> Error "Cannot parse yojson into abstract state"

module Make (S : MonadicSMemory.S) = struct
  type t = S.t abs [@@deriving yojson, show]

  (** Errors of the abstraction layer. Each abstraction-specific constructor
      mirrors the [StateErr.t] error that [PState]/[Matcher] raise for the same
      situation today, so that [can_fix]/[get_fixes]/[get_recovery_tactic]/
      [get_failing_constraint] behave identically once predicate reasoning
      happens below the state:
      - [MissingUPred] ≙ [EAsrt (vs, true_)] (the [true_] is what lets the
        recovery mechanism trigger);
      - [MissingWand] ≙ [EPure false_];
      - [AsrtFailure] ≙ [EAsrt];
      - [OtherErr] ≙ [EOther];
      - [SubError] ≙ [EMem]. *)
  type err_t =
    | MissingUPred of { name : string; vs : Expr.t list }
    | MissingWand of { lname : string; rname : string }
    | AsrtFailure of Expr.t list * Expr.t
    | OtherErr of string
    | SubError of S.err_t
  [@@deriving yojson, show]

  type init_data = S.init_data

  let pp_err = pp_err_t
  let get_init_data s = S.get_init_data s.mem

  let clear (t : t) =
    let pred_defs = MP.get_pred_defs () in
    let preds =
      List.filter
        (fun (p, _) ->
          let def = Hashtbl.find pred_defs p in
          def.pred.pred_pure)
        t.preds
    in
    { wands = []; preds; mem = S.clear t.mem }

  let mem_constraints t = S.mem_constraints t.mem

  type pred =
    | User of string
    | Wand of { lhs : string; rhs : string }
    | SubPred of string
  [@@deriving yojson]

  let is_overlapping_asrt s = S.is_overlapping_asrt s

  type action = Fold | Unfold | GUnfold | Package | SubAction of string

  let action_from_str str =
    if str = SLCmd.fold_action then Fold
    else if str = SLCmd.unfold_action then Unfold
    else if str = SLCmd.gunfold_action then GUnfold
    else if str = SLCmd.package_action then Package
    else SubAction str

  let action_to_str = function
    | Fold -> SLCmd.fold_action
    | Unfold -> SLCmd.unfold_action
    | GUnfold -> SLCmd.gunfold_action
    | Package -> SLCmd.package_action
    | SubAction a -> a

  let pred_from_str str : pred =
    match (Asrt.as_user_pred_name str, Asrt.as_wand_name str) with
    | Some name, _ -> User name
    | _, Some (lhs, rhs) -> Wand { lhs; rhs }
    | _ -> SubPred str

  let pred_to_str pred : string =
    match pred with
    | User name -> Asrt.user_pred_name name
    | Wand { lhs; rhs } -> Asrt.wand_name lhs rhs
    | SubPred p -> p

  let get_def name = MP.get_pred_def (MP.get_pred_defs ()) name

  let can_fix = function
    | MissingUPred _ -> true
    | MissingWand _ -> false
    | AsrtFailure (_, pf) -> Reduction.reduce_lexpr pf <> Expr.false_
    | OtherErr _ -> false
    | SubError e -> S.can_fix e

  let get_fixes = function
    (* User predicates and wands are never abduced (the bi-abduction fix
       machinery refuses such fixes); an empty fix list makes the branch die,
       exactly like [EAsrt (vs, true_)] does today. *)
    | MissingUPred _ | MissingWand _ | OtherErr _ -> []
    | AsrtFailure (_, pf) -> (
        match Reduction.reduce_lexpr pf with
        | Lit (Bool _) -> []
        | pf -> [ [ Asrt.Pure pf ] ])
    | SubError e -> S.get_fixes e

  let get_recovery_tactic (s : t) (e : err_t) =
    match e with
    | MissingUPred { vs; _ } -> Recovery_tactic.try_unfold vs
    | AsrtFailure (vs, _) -> Recovery_tactic.try_unfold vs
    | MissingWand _ | OtherErr _ -> Recovery_tactic.none
    | SubError e -> S.get_recovery_tactic s.mem e

  let get_failing_constraint = function
    | MissingUPred _ | MissingWand _ | AsrtFailure _ | OtherErr _ -> Expr.true_
    | SubError e -> S.get_failing_constraint e

  let split_further
      (s : t)
      (core_pred : string)
      (ins : Expr.t list)
      (err : err_t) =
    match (pred_from_str core_pred, err) with
    | SubPred core_pred, SubError err -> S.split_further s.mem core_pred ins err
    | _ -> None

  (* {2 The memory-level matching state}

     Everything below implements user-predicate and wand reasoning at the
     memory level, in direct style over an explicit matching state (memory +
     path condition), entering/leaving the [Delayed] monad only at the
     [MonadicSMemory.S] boundary. Every operation mirrors the corresponding
     [SState]/[Matcher] behavior; the [Matching_walker] provides the shared
     assertion-level machinery. *)

  type mstate = { st : t; pc : Pc.t }

  let full_pfs (pc : Pc.t) = Monadic.FOSolver.build_full_pfs pc
  let full_gamma (pc : Pc.t) = Monadic.FOSolver.build_full_gamma pc

  (* Extends the pc with formulas that have already been reduced, without
     re-reduction or typeof-rehoming — mirrors [PFS.extend] in
     [SState.assume_a]. *)
  let learn (pc : Pc.t) (fs : Expr.t list) : Pc.t =
    { pc with learned = Expr.Set.add_seq (List.to_seq fs) pc.learned }

  let with_matching (pc : Pc.t) (matching : bool) : Pc.t = { pc with matching }
  let copy_mstate (ms : mstate) : mstate = { ms with pc = Pc.copy ms.pc }

  (* Mirrors [SState.assume_a ~matching:true]. *)
  let assume_pure ~production ?(time = "") (ms : mstate) (ps : Expr.t list) :
      mstate option =
    let pfs = full_pfs ms.pc in
    let gamma = full_gamma ms.pc in
    try
      let ps = List.map (Reduction.reduce_lexpr ~pfs ~gamma) ps in
      if
        production
        || FOSolver.check_satisfiability
             ~time:("Abstraction: assume_a: " ^ time)
             ~matching:true
             (ps @ PFS.to_list pfs)
             gamma
      then Some { ms with pc = learn ms.pc ps }
      else (
        L.verbose (fun m ->
            m "Abstraction: assume_a: Couldn't assume %a"
              (Fmt.Dump.list Expr.pp) ps);
        None)
    with Reduction.ReductionException (e, msg) ->
      L.verbose (fun m ->
          m
            "Abstraction: assume_a: Couldn't assume due to an error reducing \
             %a - %s\n\
             ps: %a"
            Expr.pp e msg (Fmt.Dump.list Expr.pp) ps);
      None

  (* Mirrors [SState.assert_a]. *)
  let assert_pure (ms : mstate) (ps : Expr.t list) : bool =
    FOSolver.check_entailment Containers.SS.empty (full_pfs ms.pc) ps
      (full_gamma ms.pc)

  (* Mirrors [SState.assume_t]. *)
  let assume_type (ms : mstate) (v : Expr.t) (t : Type.t) : mstate option =
    match Typing.reverse_type_lexpr true (full_gamma ms.pc) [ (v, t) ] with
    | None -> None
    | Some gamma' ->
        Some { ms with pc = Pc.extend_types ms.pc (Type_env.to_list gamma') }

  (* Mirrors [SState.get_type]. *)
  let get_type (ms : mstate) (le : Expr.t) : Type.t option =
    let pfs = full_pfs ms.pc in
    let gamma = full_gamma ms.pc in
    let le = Reduction.reduce_lexpr ~gamma ~pfs le in
    let t, _ = Typing.type_lexpr gamma le in
    t

  (* Mirrors [SState.simplify_val]. *)
  let simplify_val (ms : mstate) (v : Expr.t) : Expr.t =
    Reduction.reduce_lexpr ~gamma:(full_gamma ms.pc) ~pfs:(full_pfs ms.pc) v

  (* Mirrors [SState.equals]. *)
  let equals (ms : mstate) (e1 : Expr.t) (e2 : Expr.t) : bool =
    FOSolver.is_equal ~pfs:(full_pfs ms.pc) ~gamma:(full_gamma ms.pc) e1 e2

  (* Mirrors [SState.get_equal_values]. *)
  let get_equal_values (ms : mstate) (les : Expr.t list) : Expr.t list =
    les @ List.concat_map (Reduction.get_equal_expressions (full_pfs ms.pc)) les

  (* Mirrors [SState.unfolding_vals]. *)
  let unfolding_vals (fs : Expr.t list) : Expr.t list =
    let map to_str to_expr =
      List.map to_str fs
      |> List.fold_left Containers.SS.union Containers.SS.empty
      |> Containers.SS.elements |> List.map to_expr
    in
    let lvars = map Expr.lvars (fun x -> Expr.LVar x) in
    let alocs = map Expr.alocs (fun x -> Expr.ALoc x) in
    let clocs = map Expr.clocs (fun x -> Expr.Lit (Loc x)) in
    clocs @ alocs @ lvars

  (* Runs a sub-memory [Delayed] computation under the current pc with the
     given matching flag (mirroring the per-call [Gpc.make] of
     [SState.consume_core_pred]/[produce_core_pred]/[execute_action]), and
     restores the ambient matching flag on the resulting pcs. *)
  let resolve_with_matching ~(matching : bool) (ms : mstate) (d : 'a Delayed.t)
      : ('a * mstate) list =
    let curr_pc = with_matching ms.pc matching in
    Delayed.resolve ~curr_pc d
    |> List.map (fun (b : 'a Branch.t) ->
           ( Branch.value b,
             { ms with pc = with_matching (Branch.pc b) ms.pc.matching } ))

  (* Internal automation (recovery, eager concrete unfolding) is only active
     in over-approximating verification — mirroring the gates of the engine's
     recovery machinery. In UX/act, errors must flow up unswallowed so that
     bi-abduction can process them. *)
  let recovery_enabled () =
    (not !Config.under_approximation)
    && !Config.unfolding
    && Exec_mode.is_verification_exec !Config.current_exec_mode

  let concrete_unfolding_enabled () =
    (not !Config.under_approximation)
    && Exec_mode.is_verification_exec !Config.current_exec_mode

  (* Mirrors the pfs-based augmentation of [SState.get_recovery_tactic]. *)
  let augment_recovery_tactic (pc : Pc.t) (tactic : Expr.t Recovery_tactic.t) :
      Expr.t Recovery_tactic.t =
    if Recovery_tactic.is_none tactic then tactic
    else
      PFS.fold_left
        (fun (acc : Expr.t Recovery_tactic.t) -> function
          | BinOp ((ALoc _ as loc), Equal, LVar x)
          | BinOp (LVar x, Equal, (ALoc _ as loc)) ->
              if Names.is_spec_var_name x then
                let try_fold =
                  Option.map
                    (fun l -> if List.mem loc l then Expr.LVar x :: l else l)
                    acc.try_fold
                in
                let try_unfold =
                  Option.map
                    (fun l -> if List.mem loc l then Expr.LVar x :: l else l)
                    acc.try_unfold
                in
                { Recovery_tactic.try_fold; try_unfold }
              else acc
          | _ -> acc)
        tactic (full_pfs pc)

  (* Mirrors [SState.get_recovery_tactic] (which folds [StateErr] errors over
     the memory tactics and augments with pfs aliases). *)
  let recovery_tactic_of_errs (ms : mstate) (errs : err_t list) :
      Expr.t Recovery_tactic.t =
    List.fold_left
      (fun acc e -> Recovery_tactic.merge (get_recovery_tactic ms.st e) acc)
      Recovery_tactic.none errs
    |> augment_recovery_tactic ms.pc

  let extend_preds ~pure (st : t) (pa : upred) : t =
    if pure && List.mem pa st.preds then (
      let name, params = pa in
      L.verbose (fun fmt ->
          fmt "Pure predicate already there, not producing: %s(%a)" name
            Fmt.(list ~sep:comma Expr.pp)
            params);
      st)
    else { st with preds = pa :: st.preds }

  module Predicate_selection_strategies = struct
    let print_local_info (i : int) (name : string) (args : Expr.t list) : unit =
      L.verbose (fun m ->
          m "Strategy %d: Examining %s(@[<h>%a@])" i name
            Fmt.(list ~sep:comma Expr.pp)
            args)

    let get_pred_def ~pred_defs (name : string) : Pred.t =
      match Hashtbl.find_opt pred_defs name with
      | Some pred -> pred.MP.pred
      | None -> failwith "ERROR: get_pred_with_vs: Predicate doesn't exist."

    (* Strategy 1: The values that we are looking for are in the in-parameters *)
    let strategy_1
        ~pred_defs
        ~(ms : mstate)
        ~values
        ((name, args) : string * Expr.t list) : int =
      print_local_info 1 name args;
      let pred_def = get_pred_def ~pred_defs name in
      let one_level_list_expander args =
        List.concat_map
          (fun (x : Expr.t) ->
            match x with
            | EList ls -> ls
            | _ -> [ x ])
          args
      in
      let in_args = one_level_list_expander (Pred.in_args pred_def args) in
      let vs = get_equal_values ms values in
      let vs = vs @ List.concat_map Expr.base_elements vs in
      let vs = List.sort_uniq compare vs in
      let vs_inter = List_utils.intersect vs in_args in
      let es_inter =
        List.fold_left (fun ac e -> Expr.Set.add e ac) Expr.Set.empty vs_inter
      in
      let es_inter =
        Expr.Set.filter
          (fun e ->
            match e with
            | Lit _ -> false
            | _ -> true)
          es_inter
      in
      Expr.Set.cardinal es_inter

    (* Strategy 2: Predicate has all literals as in-parameters *)
    let strategy_2 ~pred_defs ((name, args) : string * Expr.t list) : int =
      print_local_info 2 name args;
      let pred_def = get_pred_def ~pred_defs name in
      let in_args = Pred.in_args pred_def args in
      let all_literals =
        List.for_all
          (fun (x : Expr.t) ->
            match x with
            | Lit _ -> true
            | _ -> false)
          in_args
      in
      if all_literals then 1 else 0

    (* Strategy 3: The values that we are looking for are in the out-parameters *)
    let strategy_3 ~pred_defs ~values ((name, args) : string * Expr.t list) :
        int =
      print_local_info 3 name args;
      let pred_def = get_pred_def ~pred_defs name in
      let out_args = Pred.out_args pred_def args in
      let vs_inter = List_utils.intersect values out_args in
      let es_inter =
        List.fold_left (fun ac e -> Expr.Set.add e ac) Expr.Set.empty vs_inter
      in
      let es_inter =
        Expr.Set.filter
          (fun e ->
            match e with
            | Lit _ -> false
            | _ -> true)
          es_inter
      in
      Expr.Set.cardinal es_inter

    (* Strategy 4: Predicate has spec-var parameters. The state-level version
       intersects with [State.get_spec_vars]; the memory has no spec-var set,
       so we approximate with spec-var-named lvars. *)
    let strategy_4 ((name, args) : string * Expr.t list) : int =
      print_local_info 4 name args;
      let lvars_args =
        List.fold_left Containers.SS.union Containers.SS.empty
          (List.map Expr.lvars args)
      in
      Containers.SS.cardinal
        (Containers.SS.filter Names.is_spec_var_name lvars_args)
  end

  let consume_pred_with_vs
      ~(auto_level : [ `Low | `High ])
      (ms : mstate)
      (values : Expr.t list) : (upred * upred list) option =
    let pred_defs = MP.get_pred_defs () in
    let wrap_strategy f (name, args) =
      let pred = Predicate_selection_strategies.get_pred_def ~pred_defs name in
      if pred.pred_abstract then 0 else f (name, args)
    in
    let open Predicate_selection_strategies in
    let strategies =
      match auto_level with
      | `High ->
          [
            strategy_1 ~ms ~values ~pred_defs;
            strategy_2 ~pred_defs;
            strategy_3 ~pred_defs ~values;
            strategy_4;
          ]
      | `Low -> [ strategy_1 ~ms ~values ~pred_defs; strategy_2 ~pred_defs ]
    in
    let strategies = List.map wrap_strategy strategies in
    let preds_ref = Preds.init ms.st.preds in
    List.find_map (Preds.strategic_choice ~consume:true preds_ref) strategies
    |> Option.map (fun chosen -> (chosen, Preds.to_list preds_ref))

  let select_guarded_predicate_to_fold (ms : mstate) (values : Expr.t list) :
      upred option =
    let pred_defs = MP.get_pred_defs () in
    let wrap_strat f (name, args) =
      if Option.is_some (Pred.pred_name_from_close_token_name name) then
        f (name, args)
      else 0
    in
    let strategies =
      let open Predicate_selection_strategies in
      List.map wrap_strat
        [
          strategy_1 ~ms ~values ~pred_defs;
          strategy_2 ~pred_defs;
          strategy_3 ~pred_defs ~values;
          strategy_4;
        ]
    in
    let preds_ref = Preds.init ms.st.preds in
    let close_token =
      List.find_map (Preds.strategic_choice ~consume:false preds_ref) strategies
    in
    match close_token with
    | None -> None
    | Some (close_token, args) ->
        let actual_pred =
          Option.get (Pred.pred_name_from_close_token_name close_token)
        in
        Some (actual_pred, args)

  (* {2 The recursive knot}

     [mem_ops] is the memory-level instantiation of the matching walker; its
     resource callbacks are the consume/produce dispatchers below, which
     themselves use the walker (through [mem_ops]) for auto-folding and
     unfolding — this is where the mutual recursion between assertion-level
     matching and predicate reasoning is tied. *)

  let rec mem_ops : (mstate, err_t) W.ops =
    {
      assume_pure =
        (fun ~production ?time ms fs -> assume_pure ~production ?time ms fs);
      assert_pure;
      assume_type;
      get_type;
      simplify_val;
      consume_core_pred =
        (fun ~no_auto_fold name ms ins ->
          consume_dispatch ~no_auto_fold name ms ins);
      produce_core_pred =
        (fun name ms vs -> produce_dispatch name ms vs |> List.map Result.ok);
      consume_upred_hook = None;
      consume_wand_hook = None;
      produce_upred_hook = None;
      produce_wand_hook = None;
      copy = copy_mstate;
      update_store =
        (fun _ _ _ -> L.fail "update_store in memory-level matching");
      mk_asrt_err = (fun vs pf -> AsrtFailure (vs, pf));
      mk_other_err = (fun msg -> OtherErr msg);
      can_fix;
      unfolding_vals = (fun _ fs -> unfolding_vals fs);
      get_recovery_tactic = (fun ms errs -> recovery_tactic_of_errs ms errs);
      try_recovering = (fun ms tactic -> try_recovering ms tactic);
      unfold_concrete_preds = (fun ms -> unfold_concrete_preds ms);
      pp = (fun fmt ms -> pp fmt ms.st);
      pp_err = pp_err_t;
      log =
        {
          with_assertion_parent = (fun _ _ _ f -> f None);
          with_match_parent = (fun _ _ _ _ f -> f ());
          log_success =
            (fun _ _ _ _ ->
              L.verbose (fun m -> m "Memory-level matching: success"));
          log_failure =
            (fun _ _ _ _ ->
              L.verbose (fun m -> m "Memory-level matching: a step failed"));
          log_recovery = (fun _ _ _ -> None);
        };
    }

  and consume_dispatch
      ~(no_auto_fold : bool)
      (core_pred : string)
      (ms : mstate)
      (ins : Expr.t list) : (mstate * Expr.t list, err_t) Res_list.t =
    match pred_from_str core_pred with
    | SubPred core_pred ->
        resolve_with_matching ~matching:true ms
          (S.consume ~core_pred ms.st.mem ins)
        |> List.map (fun (res, ms') ->
               match res with
               | Ok (mem', outs) ->
                   Ok ({ ms' with st = { ms'.st with mem = mem' } }, outs)
               | Error e -> Error (SubError e))
    | User pname ->
        let pred = get_def pname in
        let n_outs = pred.pred.pred_num_params - pred.pred.ins_number in
        let vs = List.map Option.some ins @ List.init n_outs (fun _ -> None) in
        consume_upred ~no_auto_fold ms pname vs
    | Wand { lhs; rhs } -> consume_wand ms ~lname:lhs ~rname:rhs ins

  (** Consumes a user predicate. If the predicate is not "verbatim" in our set
      of preds, and it is not abstract and we are not in manual mode, we attempt
      to fold it. Mirrors [Matcher.consume_pred]. *)
  and consume_upred
      ~(no_auto_fold : bool)
      (ms : mstate)
      (pname : string)
      (vs : Expr.t option list) : (mstate * Expr.t list, err_t) Res_list.t =
    L.tmi (fun m ->
        m "Abstraction.consume_upred %s. args: @[<h>%a@]" pname
          Fmt.(list ~sep:comma (Dump.option Expr.pp))
          vs);
    let pred = get_def pname in
    let pred_def = pred.pred in
    let pred_pure = pred_def.pred_pure in
    let preds_ref = Preds.init ms.st.preds in
    match
      Preds.consume_pred ~maintain:pred_pure preds_ref pname vs
        (Containers.SI.of_list (Pred.ins_indexes pred_def))
        (equals ms)
    with
    | Some (_, vs_found) ->
        (* It was in our set of preds! *)
        L.verbose (fun m ->
            m "Returning the following vs: @[<h>%a@]"
              Fmt.(list ~sep:comma Expr.pp)
              vs_found);
        let ms =
          { ms with st = { ms.st with preds = Preds.to_list preds_ref } }
        in
        Res_list.return (ms, Pred.out_args pred_def vs_found)
    | None
      when (not !Config.manual_proof)
           && (not pred_def.pred_abstract)
           && not no_auto_fold ->
        (* Recursive Case - Folding required *)
        let () =
          L.verbose (fun fmt ->
              fmt "Auto-folding predicate: %s\n" pred_def.pred_name)
        in
        L.verbose (fun m -> m "Recursive case - attempting to fold.");
        let open Res_list.Syntax in
        let vs_ins = Pred.in_args pred_def vs in
        let vs_ins = List.map Option.get vs_ins in
        let** folded =
          fold ~in_matching:true ~state:ms ~match_kind:(W.Fold pname) pred
            vs_ins
        in
        consume_upred ~no_auto_fold folded pname vs
    | _ ->
        let values = List.filter_map Fun.id vs in
        Res_list.error_with (MissingUPred { name = pname; vs = values })

  (* Mirrors [Matcher.consume_wand]; the query outs are unknown at the
     core-predicate boundary (the walker matches them afterwards). *)
  and consume_wand (ms : mstate) ~lname ~rname (ins : Expr.t list) :
      (mstate * Expr.t list, err_t) Res_list.t =
    if !Config.under_approximation then L.fail "Wand in under-approx";
    L.verbose (fun m -> m "Matching wand assertion");
    let pred_defs = MP.get_pred_defs () in
    let lpred = MP.get_pred_def pred_defs lname in
    let rpred = MP.get_pred_def pred_defs rname in
    let largs, r_ins = List_utils.split_at ins lpred.pred.pred_num_params in
    let n_routs = rpred.pred.pred_num_params - rpred.pred.ins_number in
    let query : Wands.query =
      { lname; rname; largs; r_ins; r_outs = List.init n_routs (fun _ -> None) }
    in
    L.tmi (fun m -> m "Abstraction.consume_wand @[<h>%a@]" Wands.pp_query query);
    let wands_ref = Wands.init ms.st.wands in
    match
      Wands.consume_wand ~pred_defs ~semantic_eq:(equals ms) wands_ref query
    with
    | Some wand ->
        L.verbose (fun m ->
            m "Returning the following wand (before checking outs equality): %a"
              Wands.pp_wand wand);
        let ms =
          { ms with st = { ms.st with wands = Wands.to_list wands_ref } }
        in
        let _, wand_outs = Wands.wand_ins_outs ~pred_defs wand in
        Res_list.return (ms, wand_outs)
    | None ->
        L.verbose (fun m ->
            m "Could not find any match for the required wand!!!");
        Res_list.error_with (MissingWand { lname; rname })

  and produce_dispatch (core_pred : string) (ms : mstate) (vs : Expr.t list) :
      mstate list =
    match pred_from_str core_pred with
    | SubPred core_pred ->
        resolve_with_matching ~matching:false ms
          (S.produce ~core_pred ms.st.mem vs)
        |> List.map (fun (mem', ms') ->
               { ms' with st = { ms'.st with mem = mem' } })
    | User name -> produce_upred ms name vs
    | Wand { lhs; rhs } -> produce_wand ms lhs rhs vs

  (* Mirrors the legacy user-predicate produce arm: the predicate facts are
     assumed (an unsatisfiable state vanishes), and pure predicates are
     deduplicated. *)
  and produce_upred (ms : mstate) (name : string) (args : Expr.t list) :
      mstate list =
    L.verbose (fun fmt -> fmt "Predicate assertion.");
    let pred_def = get_def name in
    let facts =
      (* We're in charge of substituting the facts definition with the
         arguments of the predicate *)
      let params =
        List.map (fun p -> Expr.PVar (fst p)) pred_def.pred.pred_params
      in
      let substitutor =
        List.fold_left2
          (fun acc param arg ->
            fun le ->
             Expr.subst_expr_for_expr ~to_subst:param ~subst_with:arg (acc le))
          Fun.id params args
      in
      List.map substitutor pred_def.pred.pred_facts
    in
    let ms_opt =
      match facts with
      | [] -> Some ms
      | facts ->
          assume_pure ~production:!Config.delay_entailment ms
            [ Expr.conjunct facts ]
    in
    match ms_opt with
    | None ->
        (* The facts are unsatisfiable: this branch is not possible. *)
        L.verbose (fun m ->
            m "Produce upred: facts are unsatisfiable, vanishing.");
        []
    | Some ms ->
        [
          {
            ms with
            st = extend_preds ~pure:pred_def.pred.pred_pure ms.st (name, args);
          };
        ]

  and produce_wand
      (ms : mstate)
      (lhs : string)
      (rhs : string)
      (args : Expr.t list) : mstate list =
    if !Config.under_approximation then
      L.fail "Wand assertions are not supported in under-approximation mode";
    L.verbose (fun m -> m "Wand assertion.");
    let lhs_def = get_def lhs in
    let lhs_nargs = lhs_def.pred.pred_num_params in
    let largs, rargs = List_utils.split_at args lhs_nargs in
    [
      {
        ms with
        st =
          {
            ms.st with
            wands =
              Wands.{ lhs = (lhs, largs); rhs = (rhs, rargs) } :: ms.st.wands;
          };
      };
    ]

  (* Mirrors [Matcher.fold]. *)
  and fold
      ?(in_matching = false)
      ?(additional_bindings = [])
      ~match_kind
      ~(state : mstate)
      (pred : MP.pred)
      (args : Expr.t list) : (mstate, err_t) Res_list.t =
    let pred_name = pred.pred.pred_name in
    L.verbose (fun fmt -> fmt "Folding predicate: %s\n" pred_name);
    if pred.pred.pred_abstract then
      Fmt.failwith "Impossible: Folding abstract predicate %s" pred_name;
    L.verbose (fun m -> m "Predicate matching plan: %a" MP.pp pred.def_mp);
    let params = List.map (fun (x, _) -> x) pred.pred.pred_params in
    let param_bindings =
      if List.compare_lengths params args = 0 then List.combine params args
      else
        try List.combine (Pred.in_params pred.pred) args
        with Invalid_argument _ ->
          Fmt.failwith "invalid number of parameter while folding: %s%a"
            pred.pred.pred_name
            Fmt.(parens @@ hbox @@ list ~sep:comma Expr.pp)
            args
    in
    let param_bindings =
      List.map (fun (x, v) -> (Expr.PVar x, v)) param_bindings
    in
    let subst = SVal.SESubst.init (additional_bindings @ param_bindings) in
    let match_result =
      W.match_ mem_ops ~in_matching state subst pred.def_mp match_kind
    in
    let open Res_list.Syntax in
    let** ms', subst', _ = match_result in
    let arg_vs =
      if List.compare_lengths params args = 0 then args
      else
        let out_params = Pred.out_params pred.pred in
        let vs_outs =
          List.map
            (fun x ->
              match SVal.SESubst.get subst' (PVar x) with
              | Some v_x -> v_x
              | None ->
                  failwith "DEATH. Didnt learn all the outs while folding.")
            out_params
        in
        L.verbose (fun m ->
            m "Out parameters : @[<h>%a@]" Fmt.(list ~sep:comma Expr.pp) vs_outs);
        Pred.combine_ins_outs pred.pred args vs_outs
    in
    (* We extend the list of predicates with our newly folded predicate. *)
    let ms' =
      {
        ms' with
        st = extend_preds ~pure:pred.pred.pred_pure ms'.st (pred_name, arg_vs);
      }
    in
    (* If the predicate has a guard, we also produce it in our state,
       otherwise we return the just current state *)
    match pred.pred.pred_guard with
    | None -> Res_list.return ms'
    | Some guard -> W.produce mem_ops ms' subst' guard

  (* Mirrors [Matcher.unfold]. The state-level version additionally registered
     the binding names as spec vars and simplified the resulting states; both
     are state-level concerns handled above the memory (PState's action shim
     and the SMT-backed candidate matching, respectively). *)
  and unfold
      ?(additional_bindings = [])
      (ms : mstate)
      (pname : string)
      (args : Expr.t list) : (SVal.SESubst.t * mstate, err_t) Res_list.t =
    let pred_defs = MP.get_pred_defs () in
    let pred = MP.get_pred_def pred_defs pname in
    let params = List.map (fun (x, _) -> Expr.PVar x) pred.pred.pred_params in
    let open Res_list.Syntax in
    let** ms =
      match pred.pred.pred_guard with
      | None -> Res_list.return ms
      | Some _ ->
          let in_params = Pred.in_params pred.pred in
          let in_params = List.map (fun x -> Expr.PVar x) in_params in
          let in_args = Pred.in_args pred.pred args in
          let subst = SVal.SESubst.init (List.combine in_params in_args) in
          let++ s, _, _ =
            W.match_ mem_ops ~in_matching:true ms subst
              (Option.get pred.guard_mp) W.PredicateGuard
          in
          s
    in
    let subst_i = SVal.SESubst.init (List_utils.right_combine params args) in
    L.verbose (fun m ->
        m "unfold with unfold_info with additional bindings@\n%a@\n"
          Fmt.(Dump.list (pair string string))
          additional_bindings);
    let () =
      (* Mirrors [extend_subst_with_bindings]: the additional bindings are
         evaluated (reduced) in the current state. *)
      let bindings =
        List.map
          (fun (x, y) -> (Expr.LVar y, simplify_val ms (Expr.LVar x)))
          additional_bindings
      in
      SVal.SESubst.extend subst_i bindings
    in
    let definitions =
      List.map (fun (_, def) -> def) pred.pred.pred_definitions
    in
    let open Syntaxes.List in
    match definitions with
    | [] ->
        Fmt.failwith "Cannot Unfold Predicate %s with No Definitions"
          pred.pred.pred_name
    | first_def :: rest_defs -> (
        L.verbose (fun m ->
            m "Going to produce %d definitions with subst@\n%a"
              (List.length (first_def :: rest_defs))
              SVal.SESubst.pp subst_i);
        let rest_results =
          let* def = rest_defs in
          W.produce mem_ops (copy_mstate ms) (SVal.SESubst.copy subst_i) def
        in
        let first_results = W.produce mem_ops ms subst_i first_def in
        let* result = first_results @ rest_results in
        match result with
        | Error err ->
            (* If a production fails, it means this branch is not possible, we
               log and ignore. *)
            L.verbose (fun m -> m "Warning: %a" pp_err_t err);
            Res_list.vanish
        | Ok ms' -> Res_list.return (SVal.SESubst.copy subst_i, ms'))

  and fold_guarded_with_vals (ms : mstate) (vs : Expr.t list) :
      string option * (mstate, string) Res_list.t =
    L.verbose (fun m ->
        m "@[<v 2>Starting fold_guarded_with_vals: @[<h>%a@]@\n"
          Fmt.(list ~sep:comma Expr.pp)
          vs);
    if !Config.manual_proof then (None, Res_list.error_with "Manual proof")
    else
      match select_guarded_predicate_to_fold ms vs with
      | Some (pname, v_args) ->
          L.verbose (fun m -> m "FOUND STH TO FOLD: %s!!!!\n" pname);
          let pred = get_def pname in
          let rets =
            fold ~in_matching:true ~match_kind:(W.Fold pname)
              ~state:(copy_mstate ms) pred v_args
          in
          let rets =
            Res_list.map_error
              (fun _ -> "fold_guarded_with_vals: Failed to fold")
              rets
          in
          (Some pname, rets)
      | None ->
          L.verbose (fun m -> m "No predicate found to fold!");
          (None, Res_list.error_with "No predicate found to fold!")

  and unfold_with_vals'
      ~(auto_level : [ `High | `Low ])
      (ms : mstate)
      (vs : Expr.t list) : (string * (SVal.SESubst.t * mstate) list) option =
    L.verbose (fun m ->
        m "@[<v 2>Starting unfold_with_vals: @[<h>%a@]@\n"
          Fmt.(list ~sep:comma Expr.pp)
          vs);
    if !Config.manual_proof then None
    else
      match consume_pred_with_vs ~auto_level ms vs with
      | Some ((pname, v_args), remaining_preds) -> (
          L.verbose (fun m -> m "FOUND STH TO UNFOLD: %s!!!!\n" pname);
          (* The strategic choice consumes the selected predicate. *)
          let ms = { ms with st = { ms.st with preds = remaining_preds } } in
          let rets = unfold (copy_mstate ms) pname v_args in
          let only_successes, only_errors = Res_list.split rets in
          match only_errors with
          | [] ->
              L.verbose (fun m ->
                  m "Unfold complete: %s(@[<h>%a@]): %d" pname
                    Fmt.(list ~sep:comma Expr.pp)
                    v_args (List.length rets));
              Some (pname, only_successes)
          | _ :: _ ->
              L.verbose (fun m ->
                  m "Unfolding failed in unfold_with_vals: %a"
                    Fmt.(list ~sep:(any "\n") pp_err_t)
                    only_errors);
              None)
      | None ->
          L.verbose (fun m -> m "NOTHING TO UNFOLD!!!!\n");
          None

  (* Mirrors [Matcher.unfold_concrete_preds], with a pc-aware concreteness
     check (the state-level version relied on [State.simplify] having
     substituted the predicate arguments first). *)
  and unfold_concrete_preds (ms : mstate) :
      (SVal.SESubst.t option * mstate) option =
    match ms.st.preds with
    | [] -> Some (None, ms)
    | _ -> unfold_concrete_preds' ms

  and unfold_concrete_preds' (ms : mstate) :
      (SVal.SESubst.t option * mstate) option =
    let pred_defs = MP.get_pred_defs () in
    let is_unfoldable_lit lit =
      match (lit : Literal.t) with
      | Loc _ | LList _ -> false
      | _ -> true
    in
    let should_unfold (pname, vs) =
      (* Find a predicate with only concrete args and without a guard. *)
      let pred = MP.get_pred_def pred_defs pname in
      Option.is_none pred.pred.pred_guard
      && Pred.in_args pred.pred vs
         |> List.for_all (fun in_arg ->
                match Expr.to_literal (simplify_val ms in_arg) with
                | None -> false
                | Some lit -> is_unfoldable_lit lit)
    in
    let preds_ref = Preds.init ms.st.preds in
    let pred_to_unfold = Preds.pop preds_ref should_unfold in
    match pred_to_unfold with
    | Some (name, vs) -> (
        let ms =
          { ms with st = { ms.st with preds = Preds.to_list preds_ref } }
        in
        let next_states = unfold ms name vs in
        match next_states with
        | [] -> None
        | [ Ok (subst, ms'') ] ->
            L.verbose (fun m ->
                m "unfold_concrete_preds WORKED. Unfolded: %s(@[<h>%a])" name
                  Fmt.(list ~sep:comma Expr.pp)
                  vs);
            Some (Some subst, ms'')
        | next_states -> (
            let oks =
              List.filter_map
                (function
                  | Ok x -> Some x
                  | _ -> None)
                next_states
            in
            match oks with
            | _ :: _ ->
                failwith
                  "Impossible: pred with concrete ins unfolded to multiple \
                   states."
            | [] ->
                let errs = List.map Result.get_error next_states in
                Fmt.failwith
                  "Impossible: pred with concrete ins and no guard failed to \
                   unfold with errors: %a"
                  Fmt.(Dump.list pp_err_t)
                  errs))
    | None -> Some (None, ms)

  (* Mirrors [Matcher.try_recovering]. *)
  and try_recovering (ms : mstate) (tactic : Expr.t Recovery_tactic.t) :
      (mstate list * W.recovery_tactic, string) result =
    let open Syntaxes.Result in
    if !Config.under_approximation then
      L.fail "Recovery tactics not handled in UX mode";
    L.verbose (fun m -> m "Attempting to recover");
    let- fold_error =
      match tactic.try_fold with
      | Some fold_values -> (
          let pname, res = fold_guarded_with_vals ms fold_values in
          let pname = Option.value ~default:"!UNKNOWN!" pname in
          let successes, errors = Res_list.split res in
          match errors with
          | [] -> Ok (successes, W.Try_fold (pname, fold_values))
          | _ ->
              let error_string = Fmt.str "%a" Fmt.(Dump.list string) errors in
              Error error_string)
      | None ->
          L.verbose (fun m -> m "No fold recovery tactic");
          Error "None"
    in
    (* This matches the legacy behaviour *)
    let unfold_values = Option.value ~default:[] tactic.try_unfold in
    match unfold_with_vals' ~auto_level:`High ms unfold_values with
    | None ->
        Fmt.error "try_fold: %s\ntry_unfold: Automatic unfold failed" fold_error
    | Some (pname, next_states) ->
        let sp = List.map snd next_states in
        Ok (sp, W.Try_unfold (pname, unfold_values))

  and rec_unfold
      ?(fuel = 10)
      (ms : mstate)
      (pname : string)
      (args : Expr.t list) : (mstate, err_t) Res_list.t =
    if fuel = 0 then failwith "RECURSIVE UNFOLD: OUT OF FUEL"
    else
      let open Res_list.Syntax in
      let** _, ms = unfold ms pname args in
      let preds_ref = Preds.init ms.st.preds in
      match Preds.remove_by_name preds_ref pname with
      | Some (pname, vs) ->
          let ms =
            { ms with st = { ms.st with preds = Preds.to_list preds_ref } }
          in
          rec_unfold ~fuel:(fuel - 1) ms pname vs
      | None -> Res_list.return ms

  and unfold_all (ms : mstate) (pname : string) : (mstate, err_t) Res_list.t =
    let preds_ref = Preds.init ms.st.preds in
    match Preds.remove_by_name preds_ref pname with
    | None -> Res_list.return ms
    | Some (pname, vs) ->
        let ms =
          { ms with st = { ms.st with preds = Preds.to_list preds_ref } }
        in
        rec_unfold ms pname vs

  let unfold_with_vals
      ~(auto_level : [ `High | `Low ])
      (ms : mstate)
      (vs : Expr.t list) : (SVal.SESubst.t * mstate) list option =
    unfold_with_vals' ~auto_level ms vs |> Option.map snd

  (* The eager concrete-unfolding post-pass, run after every boundary
     operation. Replaces the two engine call sites (post-spec-application and
     post-recovery, the latter still wired through
     [mem_ops.unfold_concrete_preds]). *)
  let unfold_concrete_pass (ms : mstate) : mstate list =
    if not (concrete_unfolding_enabled ()) then [ ms ]
    else
      let rec pass ~bound ms =
        if bound <= 0 then [ ms ]
        else
          match unfold_concrete_preds ms with
          | None -> [] (* the unfolding of a concrete predicate vanished *)
          | Some (None, ms) -> [ ms ]
          | Some (Some _, ms') -> pass ~bound:(bound - 1) ms'
      in
      pass ~bound:(List.length ms.st.preds + 1) ms

  (* {2 Wand packaging} — port of [Matcher.Wand_packaging]. The lhs state and
     the current state carry {i independent} path conditions, which is why the
     machinery is written in direct style. *)
  module Wand_packaging = struct
    let non_empty_message =
      "The magic wand didn't swallow the whole footprint of its lhs. This \
       means it is very probably uninteresting. However, it doesn't mean that \
       the package does not hold! This is an error because it is probably a \
       mistake but it would not be unsound to continue."

    type package_state = {
      lhs_state : mstate;
      current_state : mstate;
      subst : SVal.SESubst.t;
    }

    let copy_package_state pstate =
      {
        lhs_state = copy_mstate pstate.lhs_state;
        current_state = copy_mstate pstate.current_state;
        subst = SVal.SESubst.copy pstate.subst;
      }

    let get_defs (pred : Pred.t) largs =
      if pred.pred_abstract || Option.is_some pred.pred_guard then
        [
          [
            Asrt.pred pred.pred_name (Pred.in_args pred largs)
              (Pred.out_args pred largs);
          ];
        ]
      else
        let unfolded_pred =
          Hashtbl.find_opt Unfolded_preds.tbl pred.pred_name
        in
        let pred = Option.value ~default:pred unfolded_pred in
        List.map snd pred.pred_definitions

    let make_lhs_states ~pred_defs ~(empty_state : mstate) (lname, largs) =
      let open Syntaxes.List in
      let lhs_pred = (MP.get_pred_def pred_defs lname).pred in
      let subst =
        let params =
          List.map (fun (x, _) -> Expr.PVar x) lhs_pred.pred_params
        in
        let bindings = List.combine params largs in
        SVal.SESubst.init bindings
      in
      let* lhs_def = get_defs lhs_pred largs in
      let subst = SVal.SESubst.copy subst in
      let ms = copy_mstate empty_state in
      let* produced = W.produce mem_ops ms subst lhs_def in
      match produced with
      | Error _ -> []
      | Ok ms -> [ ms ]

    let match_assertion ms subst step =
      (* We are in OX mode, matching must not branch. If it does, something is
         very wrong. Mainly because the substitution is performed in place.
         This function simplifies the return type of match-assertion: it
         returns a single outcome if it's a success. *)
      let res, _ =
        W.match_assertion_safely mem_ops ~no_auto_fold:true ms subst step
      in
      let successes, errors = Res_list.split res in
      match (successes, errors) with
      | [ x ], [] -> Ok x
      | [], errs -> Error errs
      | _ ->
          Fmt.failwith
            "Impossible: match-assertion branched in OX mode: %d successes and \
             %d errors"
            (List.length successes) (List.length errors)

    type split_answer = {
      init_subst : SVal.SESubst.t;
      mp : MP.t;
      fold_outs_info : SVal.SESubst.t * MP.step * string list * Expr.t list;
    }

    let matchables expr =
      let lvars =
        Expr.lvars expr |> Containers.SS.to_seq
        |> Seq.map (fun x -> Expr.LVar x)
      in
      let alocs =
        Expr.alocs expr |> Containers.SS.to_seq
        |> Seq.map Expr.loc_from_loc_name
      in
      Seq.append lvars alocs

    let subst_in_expr_opt (ms : mstate) subst e =
      SVal.SESubst.subst_in_expr_opt subst e |> Option.map (simplify_val ms)

    (* See [Matcher.Wand_packaging.try_split_step]. *)
    let try_split_step ~subst ~(ms : mstate) ~errs (step : MP.step) :
        split_answer option =
      let open Syntaxes.Option in
      match (step, errs) with
      | (CorePred (cp_name, ins, outs), _), _
        when Option.is_some (Asrt.as_user_pred_name cp_name) ->
          let name = Option.get (Asrt.as_user_pred_name cp_name) in
          let MP.{ pred; def_mp; _ } =
            MP.get_pred_def (MP.get_pred_defs ()) name
          in
          let* () =
            if pred.pred_abstract || Option.is_some pred.pred_guard then None
            else Some ()
          in
          let in_params =
            Pred.in_params pred |> List.map (fun x -> Expr.PVar x)
          in
          (* The in/out split is carried by the assertion's syntax. *)
          let in_args =
            ins
            |> List.map (SVal.SESubst.subst_in_expr_opt subst)
            |> List.map Option.get
          in
          let init_subst =
            List.combine in_params in_args |> SVal.SESubst.init
          in
          let out_params = Pred.out_params pred in
          let out_args = outs in
          Some
            {
              mp = def_mp;
              init_subst;
              fold_outs_info = (subst, step, out_params, out_args);
            }
      | (CorePred (core_pred, ins, outs), _), [ err ] ->
          (* What we do here is simulate the idea that the core predicate is
             actually a folded core-predicate *)
          let kb =
            List.to_seq ins
            |> Seq.map (fun x -> subst_in_expr_opt ms subst x |> Option.get)
            |> Seq.fold_left
                 (fun acc x -> MP.KB.add_seq (matchables x) acc)
                 MP.KB.empty
          in
          let init_subst =
            MP.KB.to_seq kb |> Seq.map (fun x -> (x, x)) |> SVal.SESubst.of_seq
          in
          let out_params =
            List.mapi (fun i _ -> "out___" ^ string_of_int i) outs
          in
          (* Now we build our assertion *)
          let+ new_ins_l, new_outs_learn =
            let vs_ins =
              List.map (fun x -> subst_in_expr_opt ms subst x |> Option.get) ins
            in
            split_further ms.st core_pred vs_ins err
          in
          let out_amount = List.length outs in
          let cp_amount = List.length new_ins_l in
          let all_new_outs =
            Array.init (cp_amount * out_amount) (fun _ ->
                Expr.LVar (LVar.alloc ()))
          in
          let pvar_subst =
            let seq =
              Seq.concat
              @@ Seq.init cp_amount (fun i ->
                     Seq.init out_amount (fun j ->
                         let id = Fmt.str "%d:%d" i j in
                         (Expr.PVar id, all_new_outs.((i * out_amount) + j))))
            in
            SVal.SESubst.of_seq seq
          in
          let new_cps =
            List.mapi
              (fun cp_i ins ->
                let outs =
                  List.init out_amount (fun o_i ->
                      all_new_outs.((cp_i * out_amount) + o_i))
                in
                Asrt.CorePred (core_pred, ins, outs))
              new_ins_l
          in
          let learning_equalities =
            List.map2
              (fun old_out new_out ->
                Asrt.Pure
                  (Expr.Infix.( == ) (Expr.PVar old_out)
                     (SVal.SESubst.subst_in_expr pvar_subst ~partial:false
                        new_out)))
              out_params new_outs_learn
          in
          let atoms = List.rev_append new_cps learning_equalities in
          let mp =
            let steps = MP.s_init_atoms kb atoms |> Result.get_ok in
            MP.of_step_list steps
          in
          { init_subst; mp; fold_outs_info = (subst, step, out_params, outs) }
      | _ -> None

    let match_ins_outs_lists
        (state : package_state)
        (subst : SVal.SESubst.t)
        (step : MP.step)
        (obtained : Expr.t list)
        (expected : Expr.t list) : (package_state, err_t list) Result.t =
      L.verbose (fun m ->
          m
            "About to match ins-outs after splitting!@\n\
             SUBST: %a@\n\
             OBTAINED: %a@\n\
             EXPECTED: %a@\n"
            SVal.SESubst.pp subst (Fmt.Dump.list Expr.pp) obtained
            (Fmt.Dump.list Expr.pp) expected);
      let open Syntaxes.List in
      let outs = snd step in
      let pvar_subst =
        List.mapi (fun i v -> (Expr.PVar (string_of_int i), v)) obtained
        |> SVal.SESubst.init
      in
      let outs =
        let+ u, e = outs in
        let se = SVal.SESubst.subst_in_expr pvar_subst ~partial:true e in
        let se = try Reduction.reduce_lexpr ~matching:true se with _ -> se in
        (u, se)
      in
      List.iter (fun (u, v) -> SVal.SESubst.put subst u v) outs;
      let expected =
        let+ e = expected in
        match SVal.SESubst.subst_in_expr_opt subst e with
        | None -> Fmt.failwith "Did not learn %a!" Expr.pp e
        | Some e -> e
      in
      List.fold_left2
        (fun acc vd od ->
          let open Syntaxes.Result in
          let* acc = acc in
          let equality = Expr.BinOp (vd, Equal, od) in
          if
            assert_pure acc.lhs_state [ equality ]
            || assert_pure acc.current_state [ equality ]
          then Ok acc
          else Error [ AsrtFailure ([], equality) ])
        (Ok state) obtained expected

    let rec package_case_step
        { lhs_state; current_state; subst }
        (step : MP.step) : (package_state list, err_t list) Result.t =
      let open Syntaxes.Result in
      L.verbose (fun m ->
          m "Wand about to consume RHS step: %a" Asrt.pp_atom (fst step));
      (* Substitutions are modified in place, so we copy them just in case *)
      (* First we try to consume from the lhs_state *)
      let- lhs_errs =
        let subst = SVal.SESubst.copy subst in
        let+ new_lhs_state =
          match_assertion (copy_mstate lhs_state) subst step
        in
        [ { lhs_state = new_lhs_state; current_state; subst } ]
      in
      (* If it fails, we try splitting the step and we try again *)
      let- split_errs =
        let split_option =
          try_split_step ~ms:lhs_state ~subst ~errs:lhs_errs step
        in
        match split_option with
        | Some { mp; init_subst; fold_outs_info } ->
            L.verbose (fun m ->
                m "We found a way to split, here is the MP:@\n%a" MP.pp mp);
            let temporary_state =
              {
                lhs_state = copy_mstate lhs_state;
                current_state = copy_mstate current_state;
                subst = init_subst;
              }
            in
            let* states = package_mp mp temporary_state in
            let old_subst, step, out_params, expected = fold_outs_info in
            Result_utils.map_bind
              (fun state ->
                let obtained =
                  List.map
                    (fun x ->
                      match SVal.SESubst.get state.subst (PVar x) with
                      | Some x -> x
                      | None -> Fmt.failwith "Did not learn %s ??" x)
                    out_params
                in
                let+ state =
                  match_ins_outs_lists state old_subst step obtained expected
                in
                { state with subst = old_subst })
              states
        | None -> Error []
      in
      L.verbose (fun m ->
          m
            "Wand: failed to consume from LHS! Going to try and consume from \
             current state!");
      let- current_errs =
        let matching_outcome = match_assertion current_state subst step in
        match matching_outcome with
        | Ok new_current_state ->
            Ok [ { lhs_state; current_state = new_current_state; subst } ]
        | Error errs when !Config.unfolding && List.exists can_fix errs -> (
            (* We go with the usual tactic of trying to unfold.
               Careful, after that we need *all* cases to be successful!
               That is the insight from the Viper paper correcting their old
               error "Sound automation of magic wands" *)
            let tactics = recovery_tactic_of_errs current_state errs in
            L.verbose (fun m ->
                m
                  "Trying to recover failing on the current state, obtained \
                   recovery tactics: %a"
                  (Recovery_tactic.pp Expr.pp)
                  tactics);
            let unfold_values = Option.value ~default:[] tactics.try_unfold in
            match
              unfold_with_vals ~auto_level:`High current_state unfold_values
            with
            | None -> Error []
            | Some current_states ->
                L.verbose (fun m -> m "Successfully unfolded! Let's continue!");
                let should_copy =
                  match current_states with
                  | _ :: _ :: _ -> true
                  | _ -> false
                in
                Result_utils.map_bind
                  (fun (_, current_state) ->
                    let lhs_state, subst =
                      if should_copy then
                        (copy_mstate lhs_state, SVal.SESubst.copy subst)
                      else (lhs_state, subst)
                    in
                    let+ new_current_state =
                      match_assertion current_state subst step
                    in
                    { lhs_state; current_state = new_current_state; subst })
                  current_states)
        | Error errs ->
            L.verbose (fun m -> m "Cannot recover!");
            Error errs
      in
      L.verbose (fun m -> m "Couldn't consume from anywhere!!");
      Error (lhs_errs @ split_errs @ current_errs)

    and package_mp mp (state : package_state) :
        (package_state list, err_t list) Result.t =
      let open List_res.Syntax in
      match mp with
      | MP.LabelStep _ -> L.fail "Labeled in wand RHS, can't handle that yet!"
      | Finished (Some _) -> L.fail "Finished with posts in wand RHS!"
      | Finished None -> List_res.return state
      | Choice (left_mp, right_mp) ->
          let ( let- ) = Result_utils.bind_error in
          let state_copy = copy_package_state state in
          L.verbose (fun m -> m "Trying the left-hand-side first");
          let- left_errs = package_mp left_mp state in
          L.verbose (fun m ->
              m "Left-hand-side failed, trying right-hand-side!");
          let- right_errs = package_mp right_mp state_copy in
          Error (left_errs @ right_errs)
      | ConsumeStep (step, rest_mp) ->
          let* state = package_case_step state step in
          package_mp rest_mp state

    let package_wand (ms : mstate) (wand : Wands.wand) :
        (mstate, err_t) List_res.t =
      let open Syntaxes.Result in
      if !Config.under_approximation then
        Fmt.failwith "Wand packaging not handled in UX mode";
      (* First, we create a state that matches the lhs, trying to unfold the
         content if possible. The lhs state starts from the same pure context
         but diverges from the current state afterwards. *)
      let pred_defs = MP.get_pred_defs () in
      let empty_state = { st = clear ms.st; pc = Pc.copy ms.pc } in
      let lhs_states = make_lhs_states ~empty_state ~pred_defs wand.lhs in
      let rpred = MP.get_pred_def pred_defs (fst wand.rhs) in
      let rhs_mp =
        if Option.is_some rpred.pred.pred_guard then
          L.fail "Magic Wand rhs is guarded!";
        rpred.def_mp
      in
      let subst =
        let rparams =
          List.map (fun (x, _) -> Expr.PVar x) rpred.pred.pred_params
        in
        let all_bindings = List.combine rparams (snd wand.rhs) in
        let in_bindings = Pred.in_args rpred.pred all_bindings in
        SVal.SESubst.init in_bindings
      in
      let start_states =
        match lhs_states with
        | [] -> failwith "wand lhs is False!"
        | first :: rest ->
            let rest_pack_states =
              List.map
                (fun lhs_state ->
                  {
                    lhs_state;
                    current_state = copy_mstate ms;
                    subst = SVal.SESubst.copy subst;
                  })
                rest
            in
            let first_pack_state =
              { lhs_state = first; current_state = ms; subst }
            in
            first_pack_state :: rest_pack_states
      in
      L.verbose (fun m ->
          m "About to start consuming rhs of wand. Currently %d search states"
            (List.length start_states));
      let final_states =
        List.fold_left
          (fun acc state ->
            let* acc = acc in
            let+ case = package_mp rhs_mp state in
            case @ acc)
          (Ok []) start_states
      in
      let* states = final_states in
      let all_res =
        List.map
          (fun state ->
            (* The remnant check only considers the raw memory, mirroring the
               state-level packaging exactly. *)
            if S.sure_is_nonempty state.lhs_state.st.mem then (
              L.normal (fun m ->
                  m
                    "Error: AN LHS STATE WAS NOT ENTIRELY DEPLETED, THE MAGIC \
                     WAND IS NON INTERESTING");
              Error [ OtherErr non_empty_message ])
            else Ok state.current_state)
          states
      in
      Result_utils.all all_res
  end

  let package_wand ms wand =
    match Wand_packaging.package_wand ms wand with
    | Ok [] -> failwith "WAND VANISHED???"
    | r -> r

  (* {2 Internalized recovery}

     The engine no longer drives fold/unfold recovery: it happens here, when a
     sub-memory action or a consumption fails with a fixable error. *)

  (* Mirrors the interpreter's action-failure retry loop
     (recovery from the pre-action state, tactic merged from the action
     parameters and the error, fuel-limited). *)
  let rec execute_sub_action
      ~fuel
      action_name
      (ms : mstate)
      (args : Expr.t list) : (mstate * Expr.t list, err_t) Res_list.t =
    resolve_with_matching ~matching:false ms
      (S.execute_action ~action_name ms.st.mem args)
    |> List.concat_map (fun (res, ms') ->
           match res with
           | Ok (mem', vs) ->
               [ Ok ({ ms' with st = { ms'.st with mem = mem' } }, vs) ]
           | Error e when fuel > 0 && recovery_enabled () && S.can_fix e -> (
               let tactic_from_params =
                 Recovery_tactic.try_unfold
                   (List.concat_map Expr.base_elements args)
               in
               let tactic =
                 Recovery_tactic.merge tactic_from_params
                   (S.get_recovery_tactic ms.st.mem e)
                 |> augment_recovery_tactic ms.pc
               in
               L.verbose (fun m ->
                   m "Action %s failed; attempting recovery with tactic:\n%a"
                     action_name
                     (Recovery_tactic.pp Expr.pp)
                     tactic);
               match try_recovering ms tactic with
               | Error msg ->
                   L.normal (fun m -> m "Recovery tactic failed: %s" msg);
                   [ Error (SubError e) ]
               | Ok (recovered, _) ->
                   List.concat_map
                     (fun ms'' ->
                       execute_sub_action ~fuel:(fuel - 1) action_name ms'' args)
                     recovered)
           | Error e -> [ Error (SubError e) ])

  (* Step-local consume recovery: replaces the engine's whole-plan retry for
     resource steps. *)
  let rec consume_with_recovery
      ~fuel
      core_pred
      (ms : mstate)
      (ins : Expr.t list) : (mstate * Expr.t list, err_t) Res_list.t =
    consume_dispatch ~no_auto_fold:false core_pred ms ins
    |> List.concat_map (function
         | Ok _ as ok -> [ ok ]
         | Error e when fuel > 0 && recovery_enabled () && can_fix e -> (
             let tactic = recovery_tactic_of_errs ms [ e ] in
             L.verbose (fun m ->
                 m "Consume of %s failed; attempting recovery with tactic:\n%a"
                   core_pred
                   (Recovery_tactic.pp Expr.pp)
                   tactic);
             match try_recovering ms tactic with
             | Error msg ->
                 L.normal (fun m -> m "Recovery tactic failed: %s" msg);
                 [ Error e ]
             | Ok (recovered, _) ->
                 List.concat_map
                   (fun ms' ->
                     match unfold_concrete_preds ms' with
                     | None ->
                         [
                           Error (OtherErr "Unfolding concrete value failed???");
                         ]
                     | Some (_, ms'') ->
                         consume_with_recovery ~fuel:(fuel - 1) core_pred ms''
                           ins)
                   recovered)
         | Error e -> [ Error e ])

  (* {2 The MonadicSMemory boundary} *)

  let post_pass (results : (mstate * 'a, err_t) Res_list.t) :
      (mstate * 'a, err_t) Res_list.t =
    List.concat_map
      (function
        | Ok (ms, v) ->
            unfold_concrete_pass ms |> List.map (fun ms' -> Ok (ms', v))
        | Error _ as e -> [ e ])
      results

  let to_branches ~(entry_pc : Pc.t) (results : (mstate * 'a, err_t) Res_list.t)
      : (t * 'a, err_t) result Branch.t list =
    List.map
      (function
        | Ok (ms, v) -> Branch.make ~pc:ms.pc ~value:(Ok (ms.st, v))
        | Error e -> Branch.make ~pc:entry_pc ~value:(Error e))
      results

  let consume ~core_pred (s : t) (ins : Expr.t list) :
      (t * Expr.t list, err_t) DR.t =
    Delayed.of_resolver (fun ~curr_pc ->
        let ms = { st = s; pc = curr_pc } in
        consume_with_recovery ~fuel:10 core_pred ms ins
        |> post_pass
        |> to_branches ~entry_pc:curr_pc)

  let produce ~(core_pred : string) (s : t) (ins_and_outs : Expr.t list) :
      t Delayed.t =
    Delayed.of_resolver (fun ~curr_pc ->
        let ms = { st = s; pc = curr_pc } in
        produce_dispatch core_pred ms ins_and_outs
        |> List.concat_map unfold_concrete_pass
        |> List.map (fun ms -> Branch.make ~pc:ms.pc ~value:ms.st))

  (* The predicate-manipulating SL commands arrive as reserved actions with
     the [SLCmd] encoding; the state (PState) has already evaluated the
     store-dependent sub-expressions. *)
  let execute_pred_action (act : action) (ms : mstate) (args : Expr.t list) :
      (mstate * Expr.t list, err_t) Res_list.t =
    let open Res_list.Syntax in
    let no_rets = List.map (Result.map (fun ms -> (ms, []))) in
    match SLCmd.of_action (action_to_str act) args with
    | Some (Fold (pname, les, fold_info)) ->
        let pred = get_def pname in
        let additional_bindings =
          Option.fold
            ~some:(fun (_, bindings) ->
              List.map (fun (x, e) -> (Expr.LVar x, e)) bindings)
            ~none:[] fold_info
        in
        fold ~additional_bindings ~match_kind:W.LogicCommand ~state:ms pred les
        |> no_rets
    | Some (Unfold (pname, les, additional_bindings, b)) ->
        (* 1) We retrieve the definition of the predicate to unfold and make
           sure it is not abstract and hence can be unfolded. *)
        let pred = get_def pname in
        if pred.pred.pred_abstract then
          Fmt.failwith "Impossible: Unfold of abstract predicate %s" pname;
        (* 2) The in-parameters are sufficient to trigger the unfold. *)
        let vs_ins = Pred.in_args pred.pred les in
        let vs = List.map Option.some les in
        (* 3) We consume the predicate from the state. *)
        let cons_res = consume_upred ~no_auto_fold:false ms pname vs in
        let () =
          match (cons_res, !Config.under_approximation) with
          | [], false ->
              Fmt.failwith
                "HORROR - unfold vanished while consuming folded predicate: %s"
                pname
          | _ -> ()
        in
        let** ms', vs' = cons_res in
        L.verbose (fun m ->
            m "@[<h>Returned values: %a@]" Fmt.(list ~sep:comma Expr.pp) vs');
        let vs = Pred.combine_ins_outs pred.pred vs_ins vs' in
        L.verbose (fun m -> m "@[<h>Unfold about to happen with rec %b@]" b);
        if b then rec_unfold ms' pname vs |> no_rets
        else
          let** _, ms'' = unfold ?additional_bindings ms' pname vs in
          Res_list.return (ms'', [])
    | Some (GUnfold pname) -> unfold_all ms pname |> no_rets
    | Some (Package { lhs; rhs }) ->
        let++ ms' = package_wand ms { lhs; rhs } |> Res_list.of_list_res in
        (* The packaged wand is added to the resulting state (mirrors the
           state-level [Wands.extend] after packaging). *)
        ( {
            ms' with
            st = { ms'.st with wands = Wands.{ lhs; rhs } :: ms'.st.wands };
          },
          [] )
    | Some _ | None ->
        Fmt.failwith "Invalid predicate-action encoding for %s"
          (action_to_str act)

  let execute_action ~(action_name : string) (s : t) (args : Expr.t list) =
    Delayed.of_resolver (fun ~curr_pc ->
        let ms = { st = s; pc = curr_pc } in
        let results =
          match action_from_str action_name with
          | SubAction action_name ->
              execute_sub_action ~fuel:10 action_name ms args
          | (Fold | Unfold | GUnfold | Package) as act ->
              execute_pred_action act ms args
        in
        results |> post_pass |> to_branches ~entry_pc:curr_pc)

  let is_exclusively_owned (_ : t) (_ : Expr.t list) =
    failwith
      "Cannot determine with certainty if an abstract memory is exclusively \
       owned. Use [Pred_state.is_exclusively_owned] instead."

  let is_empty (_ : t) =
    failwith "Cannot determine with certainty if an abstract memory is empty."

  let init data = { mem = S.init data; preds = []; wands = [] }

  let wand_to_asrt
      ~pred_defs
      ({ lhs = lname, largs; rhs = rname, rargs } : wand) : Asrt.atom =
    (* The wand's semantic outs are the rhs out-args; its ins are the lhs args
       together with the rhs in-args. *)
    let rpred = MP.get_pred_def pred_defs rname in
    let r_ins = Pred.in_args rpred.pred rargs in
    let r_outs = Pred.out_args rpred.pred rargs in
    Asrt.wand (lname, largs) (rname, r_ins) r_outs

  let upred_to_asrt ~pred_defs ((name, args) : upred) : Asrt.atom =
    let pred_def = MP.get_pred_def pred_defs name in
    let ins = Pred.in_args pred_def.pred args in
    let outs = Pred.out_args pred_def.pred args in
    Asrt.pred name ins outs

  let assertions ?to_keep (s : t) : Asrt.t =
    let pred_defs = MP.get_pred_defs () in
    let sub_asrt = S.assertions ?to_keep s.mem in
    let upred_asrts = List.map (upred_to_asrt ~pred_defs) s.preds in
    let wand_asrts = List.map (wand_to_asrt ~pred_defs) s.wands in
    sub_asrt @ upred_asrts @ wand_asrts

  let lvars (s : t) : Containers.SS.t =
    let open Containers in
    let pred_lvars =
      List.fold_left
        (fun ac (_, vs) ->
          List.fold_left (fun ac e -> SS.union ac (Expr.lvars e)) ac vs)
        SS.empty s.preds
    in
    let wand_lvars =
      let lvars_val_list el =
        List.fold_left
          (fun acc expr -> SS.union acc (Expr.lvars expr))
          SS.empty el
      in
      List.fold_left
        (fun acc ({ lhs = _, largs; rhs = _, rargs } : wand) ->
          acc
          |> SS.union (lvars_val_list largs)
          |> SS.union (lvars_val_list rargs))
        SS.empty s.wands
    in
    S.lvars s.mem |> SS.union pred_lvars |> SS.union wand_lvars

  let alocs (s : t) : Containers.SS.t =
    let open Containers in
    let pred_alocs =
      List.fold_left
        (fun ac (_, vs) ->
          List.fold_left (fun ac e -> SS.union ac (Expr.alocs e)) ac vs)
        SS.empty s.preds
    in
    let wand_alocs =
      let alocs_val_list el =
        List.fold_left (fun acc v -> SS.union acc (Expr.alocs v)) SS.empty el
      in
      List.fold_left
        (fun acc ({ lhs = _, largs; rhs = _, rargs } : wand) ->
          acc
          |> SS.union (alocs_val_list largs)
          |> SS.union (alocs_val_list rargs))
        SS.empty s.wands
    in
    S.alocs s.mem |> SS.union pred_alocs |> SS.union wand_alocs

  let sure_is_nonempty (s : t) =
    S.sure_is_nonempty s.mem
    || (match s.wands with
       | [] -> false
       | _ -> true)
    || List.exists
         (fun (name, _) ->
           let pred_def = MP.get_pred_def (MP.get_pred_defs ()) name in
           not pred_def.pred.pred_pure)
         s.preds

  let pp_by_need vars ft t = pp_abs (S.pp_by_need vars) ft t
  let get_print_info vars s = S.get_print_info vars s.mem

  let substitution (subst : Subst.t) (s : t) =
    let open Delayed.Syntax in
    let subst_in_val (v : Expr.t) : Expr.t =
      Subst.subst_in_expr subst ~partial:true v
    in
    let subst_wand ({ lhs = lname, largs; rhs = rname, rargs } : wand) =
      Wands.
        {
          lhs = (lname, List.map subst_in_val largs);
          rhs = (rname, List.map subst_in_val rargs);
        }
    in
    let subst_upred (s, vs) = (s, List.map subst_in_val vs) in
    let+ mem = S.substitution subst s.mem in
    let preds = List.map subst_upred s.preds in
    let wands = List.map subst_wand s.wands in
    { mem; preds; wands }
end

module _ (S : MonadicSMemory.S) : MonadicSMemory.S with type t = S.t abs =
  Make (S)
