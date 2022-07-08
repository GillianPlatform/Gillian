open Containers
module DL = Debugger_log

module type S = sig
  type st
  type heap_t
  type state
  type m_err

  module SPState :
    PState.S
      with type t = state
       and type vt = SVal.M.t
       and type st = st
       and type store_t = SStore.t
       and type heap_t = heap_t
       and type m_err_t = m_err
       and type preds_t = Preds.SPreds.t

  module SAInterpreter :
    GInterpreter.S
      with type vt = SVal.M.t
       and type st = st
       and type store_t = SStore.t
       and type state_t = state
       and type heap_t = heap_t
       and type state_err_t = SPState.err_t

  module SUnifier : Unifier.S with type st = SVal.SESubst.t

  type t
  type prog_t = (Annot.t, int) Prog.t
  type proc_tests = (string * t) list [@@deriving to_yojson]

  val start_time : float ref
  val reset : unit -> unit
  val verify_prog : prog_t -> bool -> SourceFiles.t option -> unit

  val verify_up_to_procs :
    prog_t -> SAInterpreter.result_t SAInterpreter.cont_func

  val postprocess_files : SourceFiles.t option -> unit

  module Debug : sig
    val get_tests_for_prog : prog_t -> proc_tests

    val analyse_result :
      t -> Logging.ReportId.t -> SAInterpreter.result_t -> bool
  end
end

module Make
    (SState : SState.S
                with type vt = SVal.M.t
                 and type st = SVal.SESubst.t
                 and type store_t = SStore.t)
    (SPState : PState.S
                 with type vt = SState.vt
                  and type st = SState.st
                  and type state_t = SState.t
                  and type store_t = SState.store_t
                  and type preds_t = Preds.SPreds.t)
    (External : External.S) =
struct
  module L = Logging
  module SSubst = SVal.SESubst

  module SAInterpreter =
    GInterpreter.Make (SVal.M) (SVal.SESubst) (SStore) (SPState) (External)

  module Normaliser = Normaliser.Make (SPState)

  type st = SPState.st
  type state = SPState.t
  type heap_t = SPState.heap_t
  type m_err = SPState.m_err_t

  module SPState = SPState

  module SUnifier =
    Unifier.Make (SVal.M) (SVal.SESubst) (SStore) (SState) (Preds.SPreds)

  let print_success_or_failure success =
    if success then Fmt.pr "%a" (Fmt.styled `Green Fmt.string) "Success\n"
    else Fmt.pr "%a" (Fmt.styled `Red Fmt.string) "Failure\n";
    Format.print_flush ()

  let yojson_of_expr_set set =
    `List (List.map Expr.to_yojson (Expr.Set.elements set))

  type t = {
    name : string;
    id : int * int;
    params : string list;
    pre_state : SPState.t;
    post_up : UP.t;
    flag : Flag.t option;
    spec_vars : Expr.Set.t; [@to_yojson yojson_of_expr_set]
  }
  [@@deriving to_yojson]

  type prog_t = (Annot.t, int) Prog.t
  type proc_tests = (string * t) list

  let proc_tests_to_yojson tests =
    `List
      (tests
      |> List.map (fun (name, test) -> `List [ `String name; to_yojson test ]))

  let global_results = VerificationResults.make ()
  let start_time = ref 0.

  let reset () =
    VerificationResults.reset global_results;
    SAInterpreter.reset ()

  module Hides_derivations = struct
    (** For a given definition of a predicate, this function derives the
        corresponding logical variables it hides. *)
    let add_hides
        ~prog
        ~pred_ins
        ~preds
        ~pred_name
        ~subst_params
        ~known_params
        orig_def =
      let orig_info, orig_def, orig_hides = orig_def in
      let subst_def =
        List.fold_left
          (fun def (pv, lv) ->
            Asrt.subst_expr_for_expr ~to_subst:pv ~subst_with:lv def)
          orig_def subst_params
      in
      let subst_params = List.map snd subst_params in
      let ( let* ) = Result.bind in
      let def =
        let info =
          Option.map (fun (s, vars) -> (s, SS.of_list vars)) orig_info
        in
        (subst_def, (info, None, orig_hides))
      in
      L.verbose (fun fmt -> fmt "Examining definition: %a" Asrt.pp (fst def));
      let* def_up =
        UP.init known_params UP.KB.empty pred_ins [ def ]
        |> Result.map_error (fun _ ->
               "Creation of unification plans for predicates failed.")
      in
      let a, _ = def in
      let* state =
        match Normaliser.normalise_assertion ~pred_defs:preds a with
        | Ok [ (state, _) ] -> Ok state
        | Ok _ ->
            Error
              "Creation of unification plans for predicates failed: \
               normalisation resulted in more than one state"
        | Error msg ->
            Fmt.error "Creation of unification plans for predicates failed: %s"
              msg
      in
      (* FOLD/UNFOLD/UNIFY *)
      let () =
        L.verbose (fun fmt -> fmt "EXACT: hiding fold:\n%a" SPState.pp state)
      in
      let fold_predicate = SLCmd.Fold (pred_name, subst_params, None) in
      let* fstate =
        match SPState.evaluate_slcmd prog fold_predicate state with
        | Ok [ fstate ] -> Ok fstate
        | Ok _ -> Error "EXACT: ERROR: fold resulting in multiple states"
        | Error _ -> Error "EXACT: ERROR: Impossible fold"
      in
      let () =
        L.verbose (fun fmt -> fmt "EXACT: hiding unfold:\n%a" SPState.pp fstate)
      in
      let unfold_predicate =
        SLCmd.Unfold (pred_name, subst_params, None, false)
      in
      let* fustate =
        match SPState.evaluate_slcmd prog unfold_predicate fstate with
        | Ok [ fustate ] -> Ok fustate
        | Ok _ -> Error "EXACT: ERROR: unfold resulting in multiple states"
        | Error _ -> Error "EXACT: ERROR: Impossible unfold"
      in
      L.verbose (fun fmt -> fmt "EXACT: Hiding: Before:\n%a" SPState.pp state);
      L.verbose (fun fmt -> fmt "EXACT: Hiding: After:\n%a" SPState.pp fustate);
      let state, predicates, _, variants = SPState.expose fustate in
      let subst = SVal.SESubst.init (List.map (fun x -> (x, x)) subst_params) in
      let unification_result =
        SUnifier.unify
          (state, predicates, preds, variants)
          subst def_up LogicCommand
      in
      let* subst =
        match unification_result with
        | UPUSucc [ (_, subst, _) ] -> Ok subst
        | UPUSucc _ ->
            Error
              "EXACT: ERROR: initial definition unified against in multiple \
               ways"
        | UPUFail _ ->
            Error "EXACT: ERROR: cannot unify against initial definition"
      in
      L.verbose (fun fmt -> fmt "EXACT: Obtained subst: %a" SSubst.pp subst);
      let def_lvars =
        Expr.Set.of_list
          (List.map (fun x -> Expr.LVar x) (SS.elements (Asrt.lvars a)))
      in
      SSubst.filter_in_place subst (fun k v ->
          match (Expr.equal k v, Expr.Set.mem k def_lvars) with
          | _, false -> None
          | true, _ -> None
          | _ -> (
              match k with
              | Expr.LVar x when not (Names.is_spec_var_name x) -> None
              | _ -> Some v));
      L.verbose (fun fmt -> fmt "EXACT: Filtered subst: %a" SSubst.pp subst);
      let subst = SSubst.to_list subst in
      let hidden =
        List.map
          (fun (before, after) ->
            let we_good_bro =
              Expr.UnOp (UNot, Expr.BinOp (before, Equal, after))
            in
            (before, SPState.sat_check fustate we_good_bro))
          subst
      in
      let hidden =
        List.filter_map
          (fun (before, b) ->
            match (b, before) with
            | false, _ -> None
            | true, Expr.LVar x -> Some x
            | true, _ -> Fmt.failwith "EXACT: Error: non-LVar in ESubst")
          hidden
      in
      L.verbose (fun fmt ->
          fmt "EXACT: Hidden variables: %a" Fmt.(list ~sep:comma string) hidden);
      Ok (orig_info, orig_def, hidden)

    (** Same as add_hides, but fails in case of error *)
    let add_hides_exn
        ~prog
        ~pred_ins
        ~preds
        ~pred_name
        ~subst_params
        ~known_params
        pred_def =
      match
        add_hides ~prog ~pred_ins ~preds ~pred_name ~subst_params ~known_params
          pred_def
      with
      | Ok x -> x
      | Error msg -> failwith msg

    (** For a given predicate, returns a new predicate where the hides have been derived *)
    let derive_predicate_hiding ~preds ~prog ~pred_ins (pred : Pred.t) =
      let module KB = UP.KB in
      L.verbose (fun fmt -> fmt "Examinining predicate: %s" pred.pred_name);
      let pred_params = pred.pred_params in
      let defs = pred.pred_definitions in
      let subst_params =
        List.map
          (fun (pv, _) -> (Expr.PVar pv, Expr.LVar ("#_" ^ pv)))
          pred_params
      in
      let known_params =
        KB.of_list
          (List.map (fun i -> snd (List.nth subst_params i)) pred.pred_ins)
      in
      let new_defs =
        List.map
          (add_hides_exn ~prog ~pred_ins ~preds ~pred_name:pred.pred_name
             ~subst_params ~known_params)
          defs
      in
      { pred with pred_definitions = new_defs }

    (** Given a program and its unification plans, modifies the program in place
        to add the hides to every predicate definition. *)
    let derive_predicates_hiding
        (prog : prog_t)
        (preds : (string, UP.pred) Hashtbl.t) : unit =
      if not !Config.Verification.exact then ()
      else
        let () =
          L.verbose (fun fmt -> fmt "EXACT: Examining hiding in predicates")
        in
        let prog : UP.prog =
          {
            preds;
            specs = Hashtbl.create 1;
            lemmas = Hashtbl.create 1;
            coverage = Hashtbl.create 1;
            prog;
          }
        in
        let module KB = UP.KB in
        let pred_ins =
          Hashtbl.fold
            (fun name (pred_with_up : UP.pred) pred_ins ->
              Hashtbl.add pred_ins name pred_with_up.pred.pred_ins;
              pred_ins)
            preds
            (Hashtbl.create Config.medium_tbl_size)
        in
        Hashtbl.filter_map_inplace
          (fun _pred_name up_pred ->
            Some
              UP.
                {
                  up_pred with
                  pred =
                    derive_predicate_hiding ~preds ~prog ~pred_ins up_pred.pred;
                })
          preds
  end

  let testify
      (func_or_lemma_name : string)
      (preds : (string, UP.pred) Hashtbl.t)
      (pred_ins : (string, int list) Hashtbl.t)
      (name : string)
      (params : string list)
      (id : int)
      (pre : Asrt.t)
      (posts : Asrt.t list)
      (variant : Expr.t option)
      (hides : string list option)
      (flag : Flag.t option)
      (label : (string * SS.t) option)
      (to_verify : bool) : (t option * (Asrt.t * Asrt.t list) option) list =
    let test_of_normalised_state id' (ss_pre, subst) =
      (* Step 2 - spec_vars = lvars(pre)\dom(subst) -U- alocs(range(subst)) *)
      let lvars =
        SS.fold
          (fun x acc ->
            if Names.is_spec_var_name x then Expr.Set.add (Expr.LVar x) acc
            else acc)
          (Asrt.lvars pre) Expr.Set.empty
      in
      let subst_dom = SSubst.domain subst None in
      let alocs =
        SSubst.fold subst
          (fun _ v_val acc ->
            match v_val with
            | ALoc _ -> Expr.Set.add v_val acc
            | _ -> acc)
          Expr.Set.empty
      in
      let spec_vars = Expr.Set.union (Expr.Set.diff lvars subst_dom) alocs in
      (* Step 3 - postconditions to symbolic states *)
      L.verbose (fun m ->
          m
            "Processing one postcondition of %s with label %a and spec_vars: \
             @[<h>%a@].@\n\
             Original Pre:@\n\
             %a\n\
             Symb State Pre:@\n\
             %a@\n\
             Subst:@\n\
             %a@\n\
             Posts (%d):@\n\
             %a"
            name
            Fmt.(
              option ~none:(any "None") (fun ft (s, e) ->
                  Fmt.pf ft "[ %s; %a ]" s (iter ~sep:comma SS.iter string) e))
            label
            Fmt.(iter ~sep:comma Expr.Set.iter Expr.pp)
            spec_vars Asrt.pp pre SPState.pp ss_pre SSubst.pp subst
            (List.length posts)
            Fmt.(list ~sep:(any "@\n") Asrt.pp)
            posts);
      let subst =
        SSubst.filter subst (fun e _ ->
            match e with
            | PVar _ -> false
            | _ -> true)
      in
      let posts =
        List.filter_map
          (fun p ->
            let substituted = SSubst.substitute_asrt subst ~partial:true p in
            let reduced = Reduction.reduce_assertion substituted in
            if Simplifications.admissible_assertion reduced then Some reduced
            else None)
          posts
      in
      if not to_verify then
        let pre' = Asrt.star (SPState.to_assertions ss_pre) in
        (None, Some (pre', posts))
      else
        (* Step 4 - create a unification plan for the postconditions and s_test *)
        let () =
          L.verbose (fun fmt -> fmt "Creating UPs for posts of %s" name)
        in
        let pvar_params =
          List.fold_left
            (fun acc x -> Expr.Set.add (Expr.PVar x) acc)
            Expr.Set.empty params
        in
        let known_unifiables =
          Expr.Set.add (PVar Names.return_variable)
            (Expr.Set.union pvar_params spec_vars)
        in
        let existentials =
          Option.fold ~none:Expr.Set.empty
            ~some:(fun (_, exs) ->
              SS.fold
                (fun x acc -> Expr.Set.add (LVar x) acc)
                exs Expr.Set.empty)
            label
        in
        let known_unifiables = Expr.Set.union known_unifiables existentials in
        let hides =
          match (flag, hides) with
          | None, Some hides -> hides
          | None, None when !Config.Verification.exact ->
              failwith "Lemma must declare hides logicals in exact verification"
          | _, _ -> []
        in
        let simple_posts =
          List.map
            (fun post ->
              let post_lvars = Asrt.lvars post in
              let lstr_pp = Fmt.(list ~sep:comma string) in
              let () =
                L.verbose (fun fmt ->
                    fmt "OX hiding: %a\nPost lvars: %a" lstr_pp hides lstr_pp
                      (SS.elements post_lvars))
              in
              let inter = SS.inter post_lvars (SS.of_list hides) in
              match SS.is_empty inter with
              | true -> (post, (label, None, hides))
              | false ->
                  failwith
                    ("Error: Exact lemma with impossible hiding: "
                   ^ SS.min_elt inter))
            posts
        in
        let post_up =
          UP.init known_unifiables Expr.Set.empty pred_ins simple_posts
        in
        L.verbose (fun m -> m "END of STEP 4@\n");
        match post_up with
        | Error _ ->
            let msg =
              Printf.sprintf "Warning: testify failed for %s. Cause: post_up \n"
                name
            in
            Printf.printf "%s" msg;
            L.verbose (fun m -> m "%s" msg);
            (None, None)
        | Ok post_up ->
            let pre' = Asrt.star (SPState.to_assertions ss_pre) in
            let ss_pre =
              match flag with
              (* Lemmas should not have stores when being proven *)
              | None ->
                  let empty_store = SStore.init [] in
                  SPState.set_store ss_pre empty_store
              | Some _ -> ss_pre
            in
            let test =
              {
                name;
                id = (id, id');
                params;
                pre_state = ss_pre;
                post_up;
                flag;
                spec_vars;
              }
            in
            (Some test, Some (pre', posts))
    in
    try
      (* Step 1 - normalise the precondition *)
      match
        Normaliser.normalise_assertion ~pred_defs:preds
          ~pvars:(SS.of_list params) pre
      with
      | Error _ -> [ (None, None) ]
      | Ok normalised_assertions ->
          let variants = Hashtbl.create 1 in
          let () = Hashtbl.add variants func_or_lemma_name variant in
          let normalised_assertions =
            List.map
              (fun (state, subst) ->
                (SPState.set_variants state (Hashtbl.copy variants), subst))
              normalised_assertions
          in
          let result =
            List.mapi test_of_normalised_state normalised_assertions
          in
          result
    with Failure msg ->
      let new_msg =
        Printf.sprintf
          "WARNING: testify failed for %s. Cause: normalisation with msg: %s.\n"
          name msg
      in
      Printf.printf "%s" new_msg;
      L.normal (fun m -> m "%s" new_msg);
      [ (None, None) ]

  let testify_sspec
      (spec_name : string)
      (preds : UP.preds_tbl_t)
      (pred_ins : (string, int list) Hashtbl.t)
      (name : string)
      (params : string list)
      (id : int)
      (sspec : Spec.st) : (t option * Spec.st option) list =
    let ( let+ ) x f = List.map f x in
    let+ stest, sspec' =
      testify spec_name preds pred_ins name params id sspec.ss_pre
        sspec.ss_posts sspec.ss_variant None (Some sspec.ss_flag)
        (Spec.label_vars_to_set sspec.ss_label)
        sspec.ss_to_verify
    in
    let sspec' =
      Option.map
        (fun (pre, posts) -> { sspec with ss_pre = pre; ss_posts = posts })
        sspec'
    in
    (stest, sspec')

  let testify_spec
      (spec_name : string)
      (preds : UP.preds_tbl_t)
      (pred_ins : (string, int list) Hashtbl.t)
      (spec : Spec.t) : t list * Spec.t =
    if not spec.spec_to_verify then ([], spec)
    else
      let () =
        List.iter
          (fun (sspec : Spec.st) ->
            if sspec.ss_posts = [] then
              failwith
                ("Specification without post-condition for function "
               ^ spec.spec_name))
          spec.spec_sspecs
      in
      L.verbose (fun m ->
          m
            ("-------------------------------------------------------------------------@\n"
           ^^ "Creating symbolic tests for procedure %s: %d cases\n"
           ^^ "-------------------------------------------------------------------------"
            )
            spec.spec_name
            (List.length spec.spec_sspecs));
      let _, tests, spec_sspecs =
        List.fold_left
          (fun (id, tests, sspecs) sspec ->
            let tests_and_specs =
              testify_sspec spec_name preds pred_ins spec.spec_name
                spec.spec_params id sspec
            in
            let new_tests, new_specs =
              List.fold_left
                (fun (nt, ns) (t, s) ->
                  let nt =
                    match t with
                    | Some test -> test :: nt
                    | None -> nt
                  in
                  let ns =
                    match s with
                    | Some spec -> spec :: ns
                    | None -> ns
                  in
                  (nt, ns))
                ([], []) tests_and_specs
            in
            (id + 1, new_tests @ tests, new_specs @ sspecs))
          (0, [], []) spec.spec_sspecs
      in
      let new_spec = { spec with spec_sspecs } in
      L.verbose (fun m -> m "Simplified SPECS:@\n@[%a@]@\n" Spec.pp new_spec);
      (tests, new_spec)

  let testify_lemma
      (preds : UP.preds_tbl_t)
      (pred_ins : (string, int list) Hashtbl.t)
      (lemma : Lemma.t) : t list * Lemma.t =
    let tests_and_specs =
      List.concat_map
        (fun Lemma.
               { lemma_hyp; lemma_concs; lemma_spec_variant; lemma_spec_hides } ->
          List.map
            (fun t -> (t, lemma_spec_hides))
            (testify lemma.lemma_name preds pred_ins lemma.lemma_name
               lemma.lemma_params 0 lemma_hyp lemma_concs lemma_spec_variant
               lemma_spec_hides None None true))
        lemma.lemma_specs
    in
    let tests, specs =
      List.fold_left
        (fun (test_acc, spec_acc) ((test_opt, spec_opt), lemma_spec_hides) ->
          let test_acc =
            match test_opt with
            | Some t -> t :: test_acc
            | None -> test_acc
          in
          let spec_acc =
            match spec_opt with
            | Some (lemma_hyp, lemma_concs) ->
                Lemma.
                  {
                    lemma_hyp;
                    lemma_concs;
                    lemma_spec_variant = lemma.lemma_variant;
                    lemma_spec_hides;
                  }
                :: spec_acc
            | None -> spec_acc
          in
          (test_acc, spec_acc))
        ([], []) tests_and_specs
    in
    let () =
      match specs with
      | [] ->
          raise
            (Failure
               (Printf.sprintf "Could not testify lemma %s" lemma.lemma_name))
      | _ -> ()
    in
    (tests, { lemma with lemma_specs = specs })

  let analyse_result (subst : SSubst.t) (test : t) (state : SPState.t) : bool =
    (* TODO: ASSUMING SIMPLIFICATION DOES NOT BRANCH HERE *)
    let _, states = SPState.simplify state in
    assert (List.length states = 1);
    let state = List.hd states in

    let subst = SSubst.copy subst in

    (* Adding spec vars in the post to the subst - these are effectively the existentials of the post *)
    List.iter
      (fun x ->
        if not (SSubst.mem subst (LVar x)) then
          SSubst.add subst (LVar x) (LVar x))
      (SS.elements (SPState.get_spec_vars state));

    L.verbose (fun m ->
        m "Analyse result: About to unify one postcondition of %s. post: %a"
          test.name UP.pp test.post_up);
    match SPState.unify state subst test.post_up Unifier.Postcondition with
    | true ->
        L.verbose (fun m ->
            m "Analyse result: Postcondition unified successfully");
        VerificationResults.set_result global_results test.name test.id true;
        true
    | false ->
        L.normal (fun m -> m "Analyse result: Postcondition not unifiable.");
        VerificationResults.set_result global_results test.name test.id false;
        false

  let make_post_subst (test : t) (post_state : SPState.t) : SSubst.t =
    let subst_lst =
      List.map (fun e -> (e, e)) (Expr.Set.elements test.spec_vars)
    in
    let params_subst_lst = SStore.bindings (SPState.get_store post_state) in
    let params_subst_lst =
      List.map (fun (x, v) -> (Expr.PVar x, v)) params_subst_lst
    in
    let subst = SSubst.init (subst_lst @ params_subst_lst) in
    subst

  let analyse_proc_result test flag ?parent_id result =
    match (result : SAInterpreter.result_t) with
    | ExecRes.RFail { proc; proc_idx; error_state; errors } ->
        L.verbose (fun m ->
            m
              "VERIFICATION FAILURE: Procedure %s, Command %d\n\
               Spec %s %a\n\
               @[<v 2>State:@\n\
               %a@]@\n\
               @[<v 2>Errors:@\n\
               %a@]@\n"
              proc proc_idx test.name
              (Fmt.Dump.pair Fmt.int Fmt.int)
              test.id SPState.pp error_state
              Fmt.(list ~sep:(any "@\n") SAInterpreter.Logging.pp_err)
              errors);
        if not !Config.debug then Fmt.pr "f @?";
        false
    | ExecRes.RSucc { flag = fl; final_state; last_report; _ } ->
        if Some fl <> test.flag then (
          L.normal (fun m ->
              m
                "VERIFICATION FAILURE: Spec %s %a terminated with flag %s \
                 instead of %s\n"
                test.name
                (Fmt.Dump.pair Fmt.int Fmt.int)
                test.id (Flag.str fl) (Flag.str flag));
          if not !Config.debug then Fmt.pr "f @?";
          false)
        else
          let parent_id =
            match parent_id with
            | None -> last_report
            | id -> id
          in
          DL.log (fun m ->
              m "Unify: setting parent to %a" (Fmt.option L.ReportId.pp)
                parent_id);
          L.with_parent_id parent_id (fun () ->
              let store = SPState.get_store final_state in
              let () =
                SStore.filter store (fun x v ->
                    if x = Names.return_variable then Some v else None)
              in
              let subst = make_post_subst test final_state in
              if analyse_result subst test final_state then (
                L.normal (fun m ->
                    m
                      "VERIFICATION SUCCESS: Spec %s %a terminated successfully\n"
                      test.name
                      (Fmt.Dump.pair Fmt.int Fmt.int)
                      test.id);
                if not !Config.debug then Fmt.pr "s @?";
                true)
              else (
                L.normal (fun m ->
                    m
                      "VERIFICATION FAILURE: Spec %s %a - post condition not \
                       unifiable\n"
                      test.name
                      (Fmt.Dump.pair Fmt.int Fmt.int)
                      test.id);
                if not !Config.debug then Fmt.pr "f @?";
                false))

  let analyse_proc_results
      (test : t)
      (flag : Flag.t)
      (rets : SAInterpreter.result_t list) : bool =
    if rets = [] then (
      L.(
        normal (fun m ->
            m "ERROR: Function %s evaluates to 0 results." test.name));
      exit 1);
    let success = List.for_all (analyse_proc_result test flag) rets in
    print_success_or_failure success;
    success

  let analyse_lemma_results (test : t) (rets : SPState.t list) : bool =
    let success : bool =
      rets <> []
      && List.fold_left
           (fun ac final_state ->
             let empty_store = SStore.init [] in
             let final_state = SPState.set_store final_state empty_store in
             let subst = make_post_subst test final_state in
             if analyse_result subst test final_state then (
               L.normal (fun m ->
                   m
                     "VERIFICATION SUCCESS: Spec %s %a terminated successfully\n"
                     test.name
                     (Fmt.Dump.pair Fmt.int Fmt.int)
                     test.id);
               ac)
             else (
               L.normal (fun m ->
                   m
                     "VERIFICATION FAILURE: Spec %s %a - post condition not \
                      unifiable\n"
                     test.name
                     (Fmt.Dump.pair Fmt.int Fmt.int)
                     test.id);
               false))
           true rets
    in
    if rets = [] then (
      L.(
        normal (fun m ->
            m "ERROR: Function %s evaluates to 0 results." test.name));
      exit 1);
    print_success_or_failure success;
    success

  let verify_up_to_procs (prog : UP.prog) (test : t) : UP.prog =
    (* Printf.printf "Inside verify with a test for %s\n" test.name; *)
    match test.flag with
    | Some _ ->
        let msg = "Verifying one spec of procedure " ^ test.name ^ "... " in
        L.tmi (fun fmt -> fmt "%s" msg);
        Fmt.pr "%s@?" msg;
        (* Reset coverage for every procedure in verification *)
        { prog with coverage = Hashtbl.create 1 }
    | None -> raise (Failure "Debugging lemmas unsupported!")

  let verify (prog : UP.prog) (test : t) : bool =
    let state = test.pre_state in

    (* Printf.printf "Inside verify with a test for %s\n" test.name; *)
    match test.flag with
    | Some flag ->
        let prog = verify_up_to_procs prog test in
        let rets =
          SAInterpreter.evaluate_proc
            (fun x -> x)
            prog test.name test.params state
        in
        L.verbose (fun m ->
            m "Verification: Concluded evaluation: %d obtained results.%a@\n"
              (List.length rets) SAInterpreter.Logging.pp_result rets);
        analyse_proc_results test flag rets
    | None -> (
        let lemma = Prog.get_lemma_exn prog.prog test.name in
        match lemma.lemma_proof with
        | None ->
            if !Config.lemma_proof then
              raise
                (Failure (Printf.sprintf "Lemma %s WITHOUT proof" test.name))
            else true (* It's already correct *)
        | Some proof ->
            let msg = "Verifying lemma " ^ test.name ^ "... " in
            L.tmi (fun fmt -> fmt "%s" msg);
            Fmt.pr "%s@?" msg;
            let rets = SAInterpreter.evaluate_lcmds prog proof state in
            analyse_lemma_results test rets)

  let pred_extracting_visitor =
    object
      inherit [_] Visitors.reduce
      inherit Visitors.Utils.ss_monoid
      method! visit_Pred _ pred_name _ = SS.singleton pred_name
      method! visit_Fold _ pred_name _ _ = SS.singleton pred_name
      method! visit_Unfold _ pred_name _ _ _ = SS.singleton pred_name
      method! visit_GUnfold _ pred_name = SS.singleton pred_name
    end

  let filter_internal_preds (prog : UP.prog) (pred_names : SS.t) =
    SS.filter
      (fun pred_name ->
        let pred = Prog.get_pred_exn prog.prog pred_name in
        not pred.pred_internal)
      pred_names

  let lemma_extracting_visitor =
    object
      inherit [_] Visitors.reduce
      inherit Visitors.Utils.ss_monoid
      method! visit_ApplyLem _ lemma_name _ _ = SS.singleton lemma_name
    end

  let filter_internal_lemmas (prog : UP.prog) (lemma_names : SS.t) =
    SS.filter
      (fun lemma_name ->
        let lemma = Prog.get_lemma_exn prog.prog lemma_name in
        not lemma.lemma_internal)
      lemma_names

  let record_proc_dependencies proc_name (prog : UP.prog) =
    let proc = Prog.get_proc_exn prog.prog proc_name in
    let preds_used =
      filter_internal_preds prog (pred_extracting_visitor#visit_proc () proc)
    in
    let lemmas_used =
      filter_internal_lemmas prog (lemma_extracting_visitor#visit_proc () proc)
    in
    SS.iter
      (CallGraph.add_proc_pred_use SAInterpreter.call_graph proc_name)
      preds_used;
    SS.iter
      (CallGraph.add_proc_lemma_use SAInterpreter.call_graph proc_name)
      lemmas_used

  let record_lemma_dependencies lemma_name (prog : UP.prog) =
    let lemma = Prog.get_lemma_exn prog.prog lemma_name in
    let preds_used =
      filter_internal_preds prog (pred_extracting_visitor#visit_lemma () lemma)
    in
    let lemmas_used =
      filter_internal_lemmas prog
        (lemma_extracting_visitor#visit_lemma () lemma)
    in
    SS.iter
      (CallGraph.add_lemma_pred_use SAInterpreter.call_graph lemma_name)
      preds_used;
    SS.iter
      (CallGraph.add_lemma_call SAInterpreter.call_graph lemma_name)
      lemmas_used

  let record_preds_used_by_pred pred_name (prog : UP.prog) =
    let pred = Prog.get_pred_exn prog.prog pred_name in
    let preds_used =
      filter_internal_preds prog (pred_extracting_visitor#visit_pred () pred)
    in
    SS.iter
      (CallGraph.add_pred_call SAInterpreter.call_graph pred_name)
      preds_used

  let check_previously_verified prev_results cur_verified =
    Option.fold ~none:true
      ~some:(fun res ->
        VerificationResults.check_previously_verified
          ~printer:print_success_or_failure res cur_verified)
      prev_results

  let get_tests_to_verify
      (prog : prog_t)
      (pnames_to_verify : SS.t)
      (lnames_to_verify : SS.t) : UP.prog * t list * t list =
    let ipreds = UP.init_preds prog.preds in
    match ipreds with
    | Error e ->
        Fmt.pr
          "Creation of unification plans for predicates failed with:\n%a\n@?"
          UP.pp_up_err_t e;
        Fmt.failwith "Creation of unification plans for predicates failed."
    | Ok preds -> (
        let () = Hides_derivations.derive_predicates_hiding prog preds in

        let preds_with_hiding = Hashtbl.create 1 in
        let () =
          Hashtbl.iter
            (fun name (up_pred : UP.pred) ->
              Hashtbl.replace preds_with_hiding name up_pred.pred)
            preds
        in

        let ipreds = UP.init_preds preds_with_hiding in
        let preds = Result.get_ok ipreds in

        let pred_ins =
          Hashtbl.fold
            (fun name (pred : UP.pred) pred_ins ->
              Hashtbl.add pred_ins name pred.pred.pred_ins;
              pred_ins)
            preds
            (Hashtbl.create Config.medium_tbl_size)
        in

        (* STEP 1: Get the specs to verify *)
        Fmt.pr "Obtaining specs to verify...\n@?";
        let specs_to_verify =
          List.filter
            (fun (spec : Spec.t) -> SS.mem spec.spec_name pnames_to_verify)
            (Prog.get_specs prog)
        in

        (* STEP 2: Convert specs to symbolic tests *)
        (* Printf.printf "Converting symbolic tests from specs: %f\n" (cur_time -. start_time); *)
        let tests : t list =
          List.concat_map
            (fun (spec : Spec.t) ->
              let tests, new_spec =
                testify_spec spec.spec_name preds pred_ins spec
              in
              let proc = Prog.get_proc_exn prog spec.spec_name in
              Hashtbl.replace prog.procs proc.proc_name
                { proc with proc_spec = Some new_spec };
              tests)
            specs_to_verify
        in

        (* STEP 3: Get the lemmas to verify *)
        Fmt.pr "Obtaining lemmas to verify...\n@?";
        let lemmas_to_verify =
          List.filter
            (fun (lemma : Lemma.t) -> SS.mem lemma.lemma_name lnames_to_verify)
            (Prog.get_lemmas prog)
        in

        (* STEP 4: Convert lemmas to symbolic tests *)
        (* Printf.printf "Converting symbolic tests from lemmas: %f\n" (cur_time -. start_time); *)
        let lemmas_to_verify =
          List.sort
            (fun (l1 : Lemma.t) l2 ->
              Stdlib.compare l1.lemma_name l2.lemma_name)
            lemmas_to_verify
        in
        let tests' : t list =
          List.concat_map
            (fun lemma ->
              let tests, new_lemma = testify_lemma preds pred_ins lemma in
              Hashtbl.replace prog.lemmas lemma.lemma_name new_lemma;
              tests)
            lemmas_to_verify
        in

        Fmt.pr "Obtained %d symbolic tests in total\n@?"
          (List.length tests + List.length tests');

        L.verbose (fun m ->
            m
              ("@[-------------------------------------------------------------------------@\n"
             ^^ "UNFOLDED and SIMPLIFIED SPECS and LEMMAS@\n%a@\n%a"
             ^^ "@\n\
                 -------------------------------------------------------------------------@]"
              )
              Fmt.(list ~sep:(any "@\n") Spec.pp)
              (Prog.get_specs prog)
              Fmt.(list ~sep:(any "@\n") Lemma.pp)
              (Prog.get_lemmas prog));

        (* STEP 4: Create unification plans for specs and predicates *)
        (* Printf.printf "Creating unification plans: %f\n" (cur_time -. start_time); *)
        match UP.init_prog ~preds_tbl:preds prog with
        | Error _ -> failwith "Creation of unification plans failed."
        | Ok prog' ->
            (* STEP 5: Determine static dependencies and add to call graph *)
            List.iter
              (fun test -> record_proc_dependencies test.name prog')
              tests;
            List.iter
              (fun test -> record_lemma_dependencies test.name prog')
              tests';
            Hashtbl.iter
              (fun pred_name _ -> record_preds_used_by_pred pred_name prog')
              prog'.preds;
            (prog', tests', tests))

  let verify_procs
      ?(prev_results : VerificationResults.t option)
      (prog : prog_t)
      (pnames_to_verify : SS.t)
      (lnames_to_verify : SS.t) : unit =
    let prog', tests', tests =
      get_tests_to_verify prog pnames_to_verify lnames_to_verify
    in
    (* STEP 6: Run the symbolic tests *)
    let cur_time = Sys.time () in
    Printf.printf "Running symbolic tests: %f\n" (cur_time -. !start_time);
    let success : bool =
      List.fold_left
        (fun ac test -> if verify prog' test then ac else false)
        true (tests' @ tests)
    in
    let end_time = Sys.time () in
    let cur_verified = SS.union pnames_to_verify lnames_to_verify in
    let success =
      success && check_previously_verified prev_results cur_verified
    in
    let msg : string =
      if success then "All specs succeeded:" else "There were failures:"
    in
    let msg : string = Printf.sprintf "%s %f%!" msg (end_time -. !start_time) in
    Printf.printf "%s\n" msg;
    L.normal (fun m -> m "%s" msg)

  let verify_up_to_procs (prog : prog_t) :
      SAInterpreter.result_t SAInterpreter.cont_func =
    L.with_normal_phase ~title:"Program verification" (fun () ->
        (* Analyse all procedures and lemmas *)
        let procs_to_verify =
          SS.of_list (Prog.get_noninternal_proc_names prog)
        in
        let lemmas_to_verify =
          SS.of_list (Prog.get_noninternal_lemma_names prog)
        in
        let procs_to_verify, lemmas_to_verify =
          if !Config.Verification.verify_only_some_of_the_things then
            ( SS.inter procs_to_verify
                (SS.of_list !Config.Verification.procs_to_verify),
              SS.inter lemmas_to_verify
                (SS.of_list !Config.Verification.lemmas_to_verify) )
          else (procs_to_verify, lemmas_to_verify)
        in
        let prog, _, proc_tests =
          get_tests_to_verify prog procs_to_verify lemmas_to_verify
        in
        (* TODO: Verify All procedures. Currently we only verify the first
               procedure. Assume there is at least one procedure*)
        let test = List.hd proc_tests in
        SAInterpreter.init_evaluate_proc
          (fun x -> x)
          prog test.name test.params test.pre_state)

  let postprocess_files source_files =
    let cur_source_files =
      Option.value ~default:(SourceFiles.make ()) source_files
    in
    let call_graph = SAInterpreter.call_graph in
    ResultsDir.write_verif_results cur_source_files call_graph ~diff:""
      global_results

  let verify_prog
      (prog : prog_t)
      (incremental : bool)
      (source_files : SourceFiles.t option) : unit =
    let f prog incremental source_files =
      let open ResultsDir in
      let open ChangeTracker in
      if incremental && prev_results_exist () then (
        (* Only verify changed procedures and lemmas *)
        let cur_source_files =
          match source_files with
          | Some files -> files
          | None -> failwith "Cannot use -a in incremental mode"
        in
        let prev_source_files, prev_call_graph, results =
          read_verif_results ()
        in
        let proc_changes, lemma_changes =
          get_verif_changes prog ~prev_source_files ~prev_call_graph
            ~cur_source_files
        in
        let procs_to_prune =
          proc_changes.changed_procs @ proc_changes.deleted_procs
          @ proc_changes.dependent_procs
        in
        let lemmas_to_prune =
          lemma_changes.changed_lemmas @ lemma_changes.deleted_lemmas
          @ lemma_changes.dependent_lemmas
        in
        let () = CallGraph.prune_procs prev_call_graph procs_to_prune in
        let () = CallGraph.prune_lemmas prev_call_graph lemmas_to_prune in
        let () =
          VerificationResults.prune results (procs_to_prune @ lemmas_to_prune)
        in
        let procs_to_verify =
          SS.of_list
            (proc_changes.changed_procs @ proc_changes.new_procs
           @ proc_changes.dependent_procs)
        in
        let lemmas_to_verify =
          SS.of_list
            (lemma_changes.changed_lemmas @ lemma_changes.new_lemmas
           @ lemma_changes.dependent_lemmas)
        in
        if !Config.Verification.verify_only_some_of_the_things then
          failwith "Cannot use --incremental and --procs or --lemma together";
        let () =
          verify_procs ~prev_results:results prog procs_to_verify
            lemmas_to_verify
        in
        let cur_call_graph = SAInterpreter.call_graph in
        let cur_results = global_results in
        let call_graph = CallGraph.merge prev_call_graph cur_call_graph in
        let results = VerificationResults.merge results cur_results in
        let diff = Fmt.str "%a" ChangeTracker.pp_proc_changes proc_changes in
        write_verif_results cur_source_files call_graph ~diff results)
      else
        (* Analyse all procedures and lemmas *)
        let cur_source_files =
          Option.value ~default:(SourceFiles.make ()) source_files
        in
        let procs_to_verify =
          SS.of_list (Prog.get_noninternal_proc_names prog)
        in
        let lemmas_to_verify =
          SS.of_list (Prog.get_noninternal_lemma_names prog)
        in
        let procs_to_verify, lemmas_to_verify =
          if !Config.Verification.verify_only_some_of_the_things then
            ( SS.inter procs_to_verify
                (SS.of_list !Config.Verification.procs_to_verify),
              SS.inter lemmas_to_verify
                (SS.of_list !Config.Verification.lemmas_to_verify) )
          else (procs_to_verify, lemmas_to_verify)
        in
        let () = verify_procs prog procs_to_verify lemmas_to_verify in
        let call_graph = SAInterpreter.call_graph in
        write_verif_results cur_source_files call_graph ~diff:"" global_results
    in
    L.with_normal_phase ~title:"Program verification" (fun () ->
        f prog incremental source_files)

  module Debug = struct
    let get_tests_for_prog (prog : prog_t) =
      let open Syntaxes.Option in
      let ipreds = UP.init_preds prog.preds in
      let preds = Result.get_ok ipreds in
      let pred_ins =
        Hashtbl.fold
          (fun name (pred : UP.pred) pred_ins ->
            Hashtbl.add pred_ins name pred.pred.pred_ins;
            pred_ins)
          preds
          (Hashtbl.create Config.medium_tbl_size)
      in
      let specs = Prog.get_specs prog in
      let tests =
        specs
        |> List.filter_map (fun (spec : Spec.t) ->
               let tests, new_spec =
                 testify_spec spec.spec_name preds pred_ins spec
               in
               if List.length tests > 1 then
                 DL.log (fun m ->
                     let tests_json =
                       ("tests", `List (List.map to_yojson tests))
                     in
                     let spec_json = ("spec", Spec.to_yojson spec) in
                     m ~json:[ tests_json; spec_json ]
                       "Spec for %s gave multiple tests???" spec.spec_name);
               let+ test = List_utils.hd_opt tests in
               let proc = Prog.get_proc_exn prog spec.spec_name in
               Hashtbl.replace prog.procs proc.proc_name
                 { proc with proc_spec = Some new_spec };
               (spec.spec_name, test))
      in
      DL.log (fun m ->
          m
            ~json:[ ("tests", proc_tests_to_yojson tests) ]
            "Verifier.Debug.get_tests_for_prog: Got tests");
      tests

    let analyse_result test parent_id result =
      analyse_proc_result test Normal ~parent_id result
  end
end

module From_scratch (SMemory : SMemory.S) (External : External.S) = struct
  module INTERNAL__ = struct
    module SState = SState.Make (SMemory)
  end

  include
    Make
      (INTERNAL__.SState)
      (PState.Make (SVal.M) (SVal.SESubst) (SStore) (INTERNAL__.SState)
         (Preds.SPreds))
      (External)
end
