(** A higher-order, representation-generic version of the matching-plan
    machinery: producing and consuming assertion atoms, walking matching plans
    (with choice backtracking and label steps), and the top-level matching entry
    point with its recovery loop.

    The machinery is polymorphic in the state ['s] it operates over and the
    error type ['err] it emits; every state operation is received through an
    {!ops} record. It is instantiated twice:
    - at the {i state} level by [Matcher.Make] (instantiated by [SState] over
      itself);
    - at the {i memory} level by the [Combinators.Abstraction] combinator (['s]
      = memory + path condition), where user-defined predicates and wands are
      consumed/produced like any other core predicate.

    The machinery is written in direct style (lists of results, explicit states)
    rather than in a monad: the memory-level instantiation threads several
    independent path conditions at once (wand packaging), which a single ambient
    monadic path condition cannot express. *)

module L = Logging

type match_kind =
  | Postcondition of string
  | Fold of string
  | FunctionCall of string
  | Invariant
  | LogicCommand
  | PredicateGuard
[@@deriving yojson]

type recovery_tactic =
  | Try_fold of string * Expr.t list
  | Try_unfold of string * Expr.t list
[@@deriving yojson]

type post_res = (Flag.t * Asrt.t list) option
type 's cons_pure_result = Success of 's | Abort of Expr.t | Vanish

(** Logging hooks: the state-level instantiation emits the structured matching
    reports the verification debugger consumes; the memory-level instantiation
    only logs text. *)
type ('s, 'err) log_hooks = {
  with_assertion_parent :
    's ->
    SVal.SESubst.t ->
    MP.step ->
    (L.Report_id.t option -> ('s, 'err) Res_list.t * L.Report_id.t option) ->
    ('s, 'err) Res_list.t * L.Report_id.t option;
  with_match_parent :
    's ->
    SVal.SESubst.t ->
    MP.t ->
    match_kind ->
    (unit -> ('s * SVal.SESubst.t * post_res, 'err) Res_list.t) ->
    ('s * SVal.SESubst.t * post_res, 'err) Res_list.t;
  log_success :
    's ->
    SVal.SESubst.t ->
    post_res ->
    ('s * SVal.SESubst.t * MP.t) list ->
    unit;
  log_failure : 's -> SVal.SESubst.t -> MP.step option -> 'err list -> unit;
  log_recovery : 's -> recovery_tactic -> int -> L.Report_id.t option;
}

type ('s, 'err) ops = {
  (* Pure / typing layer. [assume_pure] and [assert_pure] operate with the
     matching flag set (they mirror [State.assume_a ~matching:true] and
     [State.assert_a]). *)
  assume_pure :
    production:bool -> ?time:string -> 's -> Expr.t list -> 's option;
  assert_pure : 's -> Expr.t list -> bool;
  assume_type : 's -> Expr.t -> Type.t -> 's option;
  get_type : 's -> Expr.t -> Type.t option;
  simplify_val : 's -> Expr.t -> Expr.t;
  (* Resource layer. *)
  consume_core_pred :
    no_auto_fold:bool ->
    string ->
    's ->
    Expr.t list ->
    ('s * Expr.t list, 'err) Res_list.t;
  produce_core_pred : string -> 's -> Expr.t list -> ('s, 'err) Res_list.t;
  (* Structure / store. [update_store] is only meaningful at the state level
     (it is reached when producing a [ret = e] pure atom of a post-condition);
     the memory-level instantiation fails on it. *)
  copy : 's -> 's;
  update_store : 's -> string -> Expr.t -> 's;
  (* Errors. *)
  mk_asrt_err : Expr.t list -> Expr.t -> 'err;
  mk_other_err : string -> 'err;
  can_fix : 'err -> bool;
  unfolding_vals : 's -> Expr.t list -> Expr.t list;
  (* Recovery hooks for the fuel-limited retry loop of {!match_}. *)
  get_recovery_tactic : 's -> 'err list -> Expr.t Recovery_tactic.t;
  (* [tried] is the set of predicate candidates already unfolded by previous
     iterations of the retry loop; the implementation must not select those
     again and must return the updated set. The legacy state-level
     instantiation ignores it (it pops tried candidates from its mutable
     predicate set instead, which persists in the recovery base); immutable
     predicate stores honour it, which is what makes the loop progress (and
     terminate) exactly like the legacy one. *)
  try_recovering :
    's ->
    tried:(string * Expr.t list) list ->
    Expr.t Recovery_tactic.t ->
    ('s list * (string * Expr.t list) list * recovery_tactic, string) result;
  unfold_concrete_preds : 's -> (SVal.SESubst.t option * 's) option;
  (* Printing / logging. *)
  pp : Format.formatter -> 's -> unit;
  pp_err : Format.formatter -> 'err -> unit;
  log : ('s, 'err) log_hooks;
}

type ('s, 'err) internal_mp_res =
  ('s * SVal.SESubst.t * post_res, 'err) List_res.t

let cons_pure (ops : ('s, 'err) ops) (s : 's) (f : Expr.t) : 's cons_pure_result
    =
  if !Config.under_approximation then
    match ops.assume_pure ~production:false s [ f ] with
    | Some s -> Success s
    | None -> Vanish
  else if ops.assert_pure s [ f ] then Success s
  else Abort f

let subst_in_expr_opt
    (ops : ('s, 'err) ops)
    (s : 's)
    (subst : SVal.SESubst.t)
    (e : Expr.t) : Expr.t option =
  let v = SVal.SESubst.subst_in_expr_opt subst e in
  Option.map (ops.simplify_val s) v

let subst_in_expr (subst : SVal.SESubst.t) (le : Expr.t) : Expr.t =
  SVal.SESubst.subst_in_expr subst ~partial:false le

let complete_subst (subst : SVal.SESubst.t) (lab : string * Containers.SS.t) :
    unit =
  let _, existentials = lab in
  Containers.SS.iter
    (fun x ->
      let lvar = Expr.LVar x in
      if not (SVal.SESubst.mem subst lvar) then SVal.SESubst.put subst lvar lvar)
    existentials

(* TODO: why is this not EPure (Expr.false_) ? *)
let resource_fail (ops : ('s, 'err) ops) =
  Res_list.error_with (ops.mk_asrt_err [] Expr.false_)

let rec produce_assertion
    (ops : ('s, 'err) ops)
    (s : 's)
    (subst : SVal.SESubst.t)
    (a : Asrt.atom) : ('s, 'err) Res_list.t =
  let other_state_err msg = [ Error (ops.mk_other_err msg) ] in

  L.verbose (fun m ->
      m
        "-------------------------@\n\
         Produce simple assertion: @[<h>%a@]@\n\
         With subst: %a\n\
        \           -------------------------@\n"
        Asrt.pp_atom a SVal.SESubst.pp subst);

  L.verbose (fun m -> m "STATE: %a" ops.pp s);

  match (a : Asrt.atom) with
  | Emp ->
      L.verbose (fun fmt -> fmt "Emp assertion.");
      [ Ok s ]
  | CorePred _ -> produce_core_pred_atom ops s subst a
  | Types les -> (
      L.verbose (fun fmt -> fmt "Types assertion.");
      let state' =
        List.fold_left
          (fun state (le, t) ->
            Option.bind state (fun state ->
                let v = subst_in_expr subst le in
                ops.assume_type state v t))
          (Some s) les
      in
      match state' with
      | None -> []
      | Some s' -> [ Ok s' ])
  | Pure (BinOp (PVar x, Equal, le)) | Pure (BinOp (le, Equal, PVar x)) -> (
      L.verbose (fun fmt -> fmt "Pure assertion.");
      match SVal.SESubst.get subst (PVar x) with
      | Some v_x ->
          let v_le = subst_in_expr subst le in
          let opt_res =
            Option.map
              (fun s' -> [ Ok s' ])
              (ops.assume_pure ~production:!Config.delay_entailment s
                 [ BinOp (v_x, Equal, v_le) ])
          in
          Option.value
            ~default:
              (other_state_err
                 "Produce Simple Assertion: Subst does not cover the pure \
                  formula")
            opt_res
      | None ->
          if x = Names.return_variable then
            let v = subst_in_expr subst le in
            Res_list.return (ops.update_store s x v)
          else
            other_state_err
              ("Produce Simple Assertion: Trying to produce un-substituted \
                PVar " ^ x))
  | Pure f -> (
      L.verbose (fun fmt -> fmt "Pure assertion.");
      let f' = SVal.SESubst.subst_in_expr subst ~partial:false f in
      match ops.assume_pure ~production:!Config.delay_entailment s [ f' ] with
      | None ->
          let msg =
            Fmt.str "Produce Simple Assertion: Cannot assume pure formula %a."
              Expr.pp f'
          in
          other_state_err msg
      | Some s' -> Res_list.return s')

and produce_core_pred_atom
    (ops : ('s, 'err) ops)
    (s : 's)
    (subst : SVal.SESubst.t)
    (a : Asrt.atom) : ('s, 'err) Res_list.t =
  match a with
  | CorePred (a_id, ins, outs) ->
      L.verbose (fun fmt -> fmt "Memory producer.");
      let vs = List.map (subst_in_expr subst) (ins @ outs) in
      ops.produce_core_pred a_id s vs
  | _ -> raise (Failure "Impossible: produce_core_pred_atom on non-CorePred")

and produce_asrt_list
    (ops : ('s, 'err) ops)
    (s : 's)
    (subst : SVal.SESubst.t)
    (sas : Asrt.t) : ('s, 'err) Res_list.t =
  let open Res_list.Syntax in
  let other_state_err msg = Res_list.error_with (ops.mk_other_err msg) in
  let () =
    SVal.SESubst.iter subst (fun v value ->
        SVal.SESubst.put subst v (ops.simplify_val s value))
  in
  let** s =
    List.fold_left
      (fun intermediate_states asrt ->
        let** intermediate_state = intermediate_states in
        try produce_assertion ops intermediate_state subst asrt
        with e ->
          let admissible =
            ops.assume_pure ~time:"Produce: final check" ~production:false
              intermediate_state [ Expr.true_ ]
          in
          if !Config.delay_entailment && Option.is_none admissible then (
            L.verbose (fun fmt ->
                fmt "Production exception due to delayed entailment, survived.");
            other_state_err "Production Exception")
          else raise e)
      (Res_list.return s) sas
  in
  let s = ops.copy s in
  let admissible =
    L.verbose (fun fmt -> fmt "Produce: final check");
    try
      ops.assume_pure ~time:"Produce: final check" ~production:false s
        [ Expr.true_ ]
    with _ -> None
  in
  L.verbose (fun fmt -> fmt "Concluded final check");
  match admissible with
  | None ->
      L.normal (fun fmt -> fmt "final state non admissible");
      Res_list.vanish
  | Some s -> Res_list.return s

let produce
    (ops : ('s, 'err) ops)
    (s : 's)
    (subst : SVal.SESubst.t)
    (a : Asrt.t) : ('s, 'err) Res_list.t =
  L.verbose (fun m ->
      m "@[-----------------@\n-----------------@\nProduce assertion: @[%a@]@]"
        Asrt.pp a);
  let sas = MP.simplify_asrts a in
  produce_asrt_list ops s subst sas

let produce_posts
    (ops : ('s, 'err) ops)
    (s : 's)
    (subst : SVal.SESubst.t)
    (asrts : Asrt.t list) : 's list =
  let open Syntaxes.List in
  L.verbose (fun m ->
      m
        "@[<v 2>Produce posts: There are %d postconditions to produce. And \
         here they are:@\n\
         %a@]"
        (List.length asrts)
        Fmt.(list ~sep:(any "@\n") Asrt.pp)
        asrts);
  let* asrt = asrts in
  let subst = SVal.SESubst.copy subst in
  let s = ops.copy s in
  produce ops s subst asrt
  |> List.filter_map (function
       | Error err ->
           L.verbose (fun m -> m "Warning: %a" ops.pp_err err);
           None (* Ignoring errors *)
       | Ok s ->
           SVal.SESubst.iter subst (fun e v ->
               match e with
               | PVar x -> ignore (ops.update_store s x v)
               | _ -> ());
           Some s)

let rec match_ins_outs_lists
    (ops : ('s, 'err) ops)
    (s : 's)
    (subst : SVal.SESubst.t)
    (step : MP.step)
    (vos : Expr.t list)
    (eos : Expr.t list) : 's cons_pure_result =
  let ( let+ ) x f = List.map f x in
  let outs = snd step in
  L.verbose (fun fmt ->
      fmt "Outs: %a"
        Fmt.(
          brackets
            (list ~sep:semi (parens (pair ~sep:comma Expr.pp Expr.full_pp))))
        outs);
  L.verbose (fun fmt ->
      fmt "Obtained values: %a" Fmt.(brackets (list ~sep:semi Expr.pp)) vos);
  L.verbose (fun fmt ->
      fmt "Obtained exprs: %a" Fmt.(brackets (list ~sep:semi Expr.pp)) eos);
  (* Substitution of the program variables *)
  let pvar_subst_bindings =
    List.mapi (fun i v -> (Expr.PVar (string_of_int i), v)) vos
  in
  let pvar_subst = SVal.SESubst.init pvar_subst_bindings in
  L.verbose (fun fmt -> fmt "Parameter subst\n%a" SVal.SESubst.pp pvar_subst);
  let outs : MP.outs option =
    try
      Some
        (List.map
           (fun (u, e) ->
             let se = SVal.SESubst.subst_in_expr pvar_subst ~partial:true e in
             (u, try Reduction.reduce_lexpr ~matching:true se with _ -> se))
           outs)
    with _ -> None
  in
  match outs with
  | None -> Abort Expr.true_
  | Some outs -> (
      L.verbose (fun fmt ->
          fmt "Substed outs: %a"
            Fmt.(
              brackets
                (list ~sep:semi (parens (pair ~sep:comma Expr.pp Expr.full_pp))))
            outs);
      List.iter (fun (u, v) -> SVal.SESubst.put subst u v) outs;
      let eos =
        let+ e = eos in
        match SVal.SESubst.subst_in_expr_opt subst e with
        | None ->
            let msg =
              Fmt.str
                "INTERNAL ERROR: Not all ins known, I don't know this one: %a"
                Expr.full_pp e
            in
            L.fail msg
        | Some e -> e
      in
      try
        List.fold_left2
          (fun ac vd od ->
            match ac with
            | Abort _ | Vanish -> ac
            | Success s ->
                let pf = Expr.BinOp (vd, Equal, od) in
                cons_pure ops s pf)
          (Success s) vos eos
      with Invalid_argument _ ->
        Fmt.failwith "Invalid amount of args for the following MP step : %a"
          MP.pp_step step)

and match_assertion'
    (ops : ('s, 'err) ops)
    ?(no_auto_fold = false)
    (s : 's)
    (subst : SVal.SESubst.t)
    (step : MP.step) : ('s, 'err) Res_list.t * L.Report_id.t option =
  ops.log.with_assertion_parent s subst step (fun assertion_id ->
      ignore assertion_id;
      let p, _ = step in
      let res_list =
        match (p : Asrt.atom) with
        | CorePred _ -> consume_core_pred_step ops ~no_auto_fold s subst step
        (* Conjunction should not be here *)
        | Pure (BinOp (_, And, _)) ->
            raise (Failure "Match assertion: And: should have been reduced")
        (* Other pure assertions *)
        | Pure f -> (
            let outs = snd step in
            let discharges =
              List.fold_left
                (fun discharges (u, out) ->
                  let open Syntaxes.Result in
                  let* discharges in
                  (* Perform the substitution in the out *)
                  let* out =
                    SVal.SESubst.subst_in_expr_opt subst out
                    |> Result_utils.of_option ~none:()
                  in
                  (* Special case: learning len x when we know x *)
                  let discharges =
                    match u with
                    | Expr.UnOp (LstLen, u') -> (
                        match SVal.SESubst.get subst u' with
                        | None -> discharges
                        | Some out' ->
                            let new_discharges =
                              Expr.BinOp (out, Equal, UnOp (LstLen, out'))
                            in
                            new_discharges :: discharges)
                    | _ -> discharges
                  in
                  (* And add to e-subst *)
                  match SVal.SESubst.get subst u with
                  | None ->
                      SVal.SESubst.put subst u out;
                      Ok discharges
                  | Some out' when Expr.equal out out' -> Ok discharges
                  | Some out' ->
                      let new_discharge = Expr.BinOp (out, Equal, out') in
                      Ok (new_discharge :: discharges))
                (Ok []) outs
            in
            let discharges =
              match discharges with
              | Error () ->
                  Fmt.failwith
                    "INTERNAL ERROR: Matching failure: do not know all ins for \
                     %a"
                    Expr.pp f
              | Ok discharges -> discharges
            in
            (* To match a pure formula we must know all ins *)
            let opf = SVal.SESubst.subst_in_expr_opt subst f in
            match opf with
            | None ->
                Fmt.failwith "Matching failure: do not know all ins for %a"
                  Expr.pp f
            | Some pf -> (
                let discharges_pf =
                  List.fold_left Expr.Infix.( && ) Expr.true_ discharges
                in
                let discharges_pf =
                  Reduction.reduce_lexpr ~matching:true discharges_pf
                in
                let to_asrt = Expr.Infix.( && ) pf discharges_pf in
                match cons_pure ops s to_asrt with
                | Success s' -> Res_list.return s'
                | Vanish -> Res_list.vanish
                | Abort _ ->
                    let vs = ops.unfolding_vals s [ pf ] in
                    let error = ops.mk_asrt_err vs pf in
                    Res_list.error_with error))
        | Types les -> (
            let corrections =
              List.fold_left
                (fun (ac : Expr.t list) (le, t) ->
                  let v_le = (subst_in_expr_opt ops s subst) le in
                  let v_le : Expr.t =
                    match v_le with
                    | Some v_le -> v_le
                    | None -> raise (Failure "DEATH. match assertion Types")
                  in
                  match ops.get_type s v_le with
                  | Some t' ->
                      if not (Type.equal t t') then Expr.false_ :: ac else ac
                  | None ->
                      BinOp (UnOp (TypeOf, v_le), Equal, Lit (Type t)) :: ac)
                [] les
            in
            match corrections with
            | [] -> Res_list.return s
            | _ ->
                if !Config.under_approximation then
                  (* In under-approx we try to assume the types hold *)
                  match ops.assume_pure ~production:false s corrections with
                  | None -> Res_list.vanish
                  | Some s' -> Res_list.return s'
                else
                  let les, _ = List.split les in
                  let les =
                    List.filter_map (subst_in_expr_opt ops s subst) les
                  in
                  let conjunct = Expr.conjunct corrections in
                  let error = ops.mk_asrt_err les conjunct in
                  Res_list.error_with error)
        (* LTrue, LFalse, LEmp, LStar *)
        | _ -> raise (Failure "Illegal Assertion in Matching Plan")
      in
      (res_list, assertion_id))

and consume_core_pred_step
    (ops : ('s, 'err) ops)
    ~(no_auto_fold : bool)
    (s : 's)
    (subst : SVal.SESubst.t)
    (step : MP.step) : ('s, 'err) Res_list.t =
  let open Res_list.Syntax in
  match (fst step : Asrt.atom) with
  | CorePred (a_id, e_ins, e_outs) -> (
      let vs_ins = List.map (subst_in_expr_opt ops s subst) e_ins in
      let failure = List.exists (fun x -> x = None) vs_ins in
      if failure then (
        L.verbose (fun m ->
            m "I don't know all ins for %a????" Asrt.pp_atom (fst step));
        if !Config.under_approximation then [] else resource_fail ops)
      else
        let vs_ins = List.map Option.get vs_ins in
        L.verbose (fun m ->
            m "Executing consume: %s with ins: @[<h>%a@]" a_id
              Fmt.(list ~sep:comma Expr.pp)
              vs_ins);
        (* The outs already known through the substitution are forwarded as a
           candidate-selection hint (they are still checked below, by
           [match_ins_outs_lists]). *)
        let outs_hint = List.map (subst_in_expr_opt ops s subst) e_outs in
        let** s'', vs_outs =
          Consume_hints.with_hint outs_hint (fun () ->
              ops.consume_core_pred ~no_auto_fold a_id s vs_ins)
        in
        (* Separate outs into direct matchables and others *)
        match match_ins_outs_lists ops s'' subst step vs_outs e_outs with
        | Success s''' -> Res_list.return s'''
        | Abort fail_pf ->
            (* TODO: why is this not EPure (fail_pf) ? *)
            let error = ops.mk_asrt_err [] fail_pf in
            Res_list.error_with error
        | Vanish -> Res_list.vanish)
  | _ -> raise (Failure "Impossible: consume_core_pred_step on non-CorePred")

and match_assertion
    (ops : ('s, 'err) ops)
    ?(no_auto_fold = false)
    (s : 's)
    (subst : SVal.SESubst.t)
    (step : MP.step) : ('s, 'err) Res_list.t =
  match_assertion' ops ~no_auto_fold s subst step |> fst

and match_assertion_safely
    (ops : ('s, 'err) ops)
    ?(no_auto_fold = false)
    (s : 's)
    (subst : SVal.SESubst.t)
    (step : MP.step) : ('s, 'err) Res_list.t * L.Report_id.t option =
  try match_assertion' ops ~no_auto_fold s subst step
  with err ->
    let () =
      L.verbose (fun m ->
          m
            "WARNING: UNCAUGHT EXCEPTION IN MATCH ASSERTION: %s@\n\
             Here's the backtrace: %s"
            (Printexc.to_string err)
            (Printexc.get_backtrace ()))
    in
    let res_list =
      if !Config.under_approximation then
        let () =
          L.verbose (fun m -> m "UX mode: vanishing despite exception!")
        in
        Res_list.vanish
      else
        match fst step with
        | Pure pf ->
            let vs = ops.unfolding_vals s [ pf ] in
            Res_list.error_with (ops.mk_asrt_err vs pf)
        | asrt ->
            let other_error =
              ops.mk_other_err
                (Fmt.str "Uncaught exception while matching assertions %a"
                   Asrt.pp_atom asrt)
            in
            Res_list.error_with other_error
    in
    (res_list, None)

and match_mp'
    (ops : ('s, 'err) ops)
    (s_states :
      (('s * SVal.SESubst.t * MP.t) * L.Report_id.t option) list * 'err list) :
    ('s, 'err) internal_mp_res =
  let s_states, errs_so_far = s_states in
  L.verbose (fun m ->
      m "Match MP: There are %d states left to consider." (List.length s_states));
  let ux = !Config.under_approximation in
  match s_states with
  | [] ->
      (* There are no more states to explore: in OX, it means we failed to
         match, in UX, it means there are no valid paths we know about, we
         vanish. *)
      if ux then List_res.vanish else Error errs_so_far
  | ((astate, subst, mp), prev_id) :: rest_search_states -> (
      let () = L.set_previous ~force_none:true prev_id in
      match mp with
      | LabelStep (label, rest_mp) ->
          L.verbose (fun m ->
              m
                "Reached LabelStep, about to complete substitution with vars: \
                 %a"
                Fmt.(Dump.iter Containers.SS.iter nop string)
                (snd label));
          complete_subst subst label;
          let current_state = ((astate, subst, rest_mp), prev_id) in
          match_mp' ops (current_state :: rest_search_states, errs_so_far)
      | Choice (left_mp, right_mp) ->
          L.verbose (fun m -> m "Reached a choice with 2 MPs: about to branch");
          let astate_copy = ops.copy astate in
          let subst_copy = SVal.SESubst.copy subst in
          let left_state = ((astate, subst, left_mp), prev_id) in
          let right_state = ((astate_copy, subst_copy, right_mp), prev_id) in
          match_mp' ops
            (left_state :: right_state :: rest_search_states, errs_so_far)
      | Finished posts ->
          (* We're done with matching of this case.
             In OX, we may stop, as we proved implication.
             In UX, we explore more as to extend coverage. *)
          let remaining_states =
            List.map (fun (triple, _) -> triple) rest_search_states
          in
          let () = ops.log.log_success astate subst posts remaining_states in
          if ux then
            let other_paths =
              match match_mp' ops (rest_search_states, errs_so_far) with
              | Error _ -> failwith "match_mp' failed in UX!"
              | Ok other_paths -> other_paths
            in
            Ok ((astate, subst, posts) :: other_paths)
          else List_res.return (astate, subst, posts)
      | ConsumeStep (step, rest_mp) -> (
          let res_list, assertion_id =
            match_assertion_safely ops astate subst step
          in
          let successes, errors = Res_list.split res_list in
          match (!Config.under_approximation, successes, errors) with
          (* We start by handling the crash cases that should never happen *)
          | true, [], _ ->
              (* Vanished in UX *)
              match_mp' ops (rest_search_states, errs_so_far)
          | false, _, _ :: _ ->
              (* Matching failed in OX. We try the next case *)
              let () = ops.log.log_failure astate subst (Some step) errors in
              match_mp' ops (rest_search_states, errors @ errs_so_far)
          | _, [ state ], [] ->
              match_mp' ops
                ( ((state, subst, rest_mp), assertion_id) :: rest_search_states,
                  errs_so_far )
          | false, [], [] ->
              L.verbose (fun m -> m "Consumer yielded 0 branches in OX mode!!!");
              match_mp' ops (rest_search_states, errs_so_far)
          | false, states, [] -> (
              L.verbose (fun m ->
                  m "Consumer yielded >1 branches in OX mode: %d branches!!!"
                    (List.length states));
              (* We have obtained several branches. So there is a disjunction
                 in the PFS. All branches need to successfully unify against
                 this *)
              let all_next : ('s, 'err) internal_mp_res =
                List.concat_map
                  (fun state ->
                    let state = ops.copy state in
                    let subst = SVal.SESubst.copy subst in
                    Res_list.of_list_res
                    @@ match_mp' ops
                         ( [ ((state, subst, rest_mp), assertion_id) ],
                           errs_so_far ))
                  states
                |> Res_list.to_list_res
              in
              match all_next with
              | Ok res -> Ok res
              | Error errs ->
                  match_mp' ops (rest_search_states, errs @ errs_so_far))
          | true, first :: rem, _ ->
              let rem =
                List.map
                  (fun state ->
                    ((state, SVal.SESubst.copy subst, rest_mp), assertion_id))
                  rem
              in
              match_mp' ops
                (((first, subst, rest_mp), assertion_id) :: rem, errs_so_far)))

and match_mp
    (ops : ('s, 'err) ops)
    ?prev_id
    (s_states : ('s * SVal.SESubst.t * MP.t) list * 'err list) :
    ('s, 'err) internal_mp_res =
  let s_states =
    let states, errs = s_states in
    let states = states |> List.map (fun state -> (state, prev_id)) in
    (states, errs)
  in
  match_mp' ops s_states

and match_
    (ops : ('s, 'err) ops)
    ?(in_matching = false)
    (astate : 's)
    (subst : SVal.SESubst.t)
    (mp : MP.t)
    (match_kind : match_kind) :
    ('s * SVal.SESubst.t * post_res, 'err) Res_list.t =
  let astate_i = ops.copy astate in
  let subst_i = SVal.SESubst.copy subst in
  let can_fix errs = List.exists ops.can_fix errs in

  let rec handle_ret ?prev_id ~fuel ~tried ret =
    L.set_previous ~force_none:true prev_id;
    match ret with
    | Ok successes ->
        L.verbose (fun fmt -> fmt "Matcher.match_: Success (possibly empty)");
        Res_list.just_oks successes
    | Error errs
      when fuel > 0 && !Config.unfolding
           && Exec_mode.is_verification_exec !Config.current_exec_mode
           && (not in_matching) && can_fix errs -> (
        L.verbose (fun fmt -> fmt "Matcher.match_: Failure");
        if !Config.under_approximation then
          L.fail "MATCHING ABORTED IN UX MODE???";
        let tactics = ops.get_recovery_tactic astate_i errs in
        L.verbose (fun m ->
            m
              "Match. Unable to match. About to attempt the following recovery \
               tactic:\n\
               %a"
              (Recovery_tactic.pp Expr.pp)
              tactics);
        match ops.try_recovering astate_i ~tried tactics with
        | Error msg ->
            L.normal (fun m -> m "Match. Recovery tactic failed: %s" msg);
            Res_list.just_errors errs
        | Ok (sp, tried, tactic) -> (
            let open Syntaxes.List in
            let recovery_report_id =
              let id = ref None in
              fun () ->
                match !id with
                | Some id -> id
                | None ->
                    let num_results = List.length sp in
                    let id' = ops.log.log_recovery astate tactic num_results in
                    let () = id := Some id' in
                    id'
            in
            let* astate = sp in
            match ops.unfold_concrete_preds astate with
            | None ->
                let error =
                  ops.mk_other_err "Unfolding concrete value failed???"
                in
                Res_list.error_with error
            | Some (_, astate) ->
                let subst'' = SVal.SESubst.copy subst_i in
                let prev_id = recovery_report_id () in
                let new_ret =
                  match_mp ops ?prev_id ([ (astate, subst'', mp) ], [])
                in
                handle_ret ?prev_id ~fuel:(fuel - 1) ~tried new_ret))
    | Error errors ->
        L.verbose (fun fmt -> fmt "Matcher.match: Failure");
        Res_list.just_errors errors
  in
  ops.log.with_match_parent astate subst mp match_kind (fun () ->
      let ret = match_mp ops ([ (astate, subst, mp) ], []) in
      handle_ret ~fuel:10 ~tried:[] ret)
