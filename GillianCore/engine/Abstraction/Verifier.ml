open Containers

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

  type t

  val start_time : float ref
  val reset : unit -> unit

  val verify_prog :
    (Annot.t, int) Prog.t -> bool -> SourceFiles.t option -> unit

  val verify_up_to_procs :
    (Annot.t, int) Prog.t -> SAInterpreter.result_t SAInterpreter.cont_func

  val postprocess_files : SourceFiles.t option -> unit
end

module Make
    (SState : State.S
                with type vt = SVal.M.t
                 and type st = SVal.SESubst.t
                 and type store_t = SStore.t)
    (SPState : PState.S
                 with type vt = SVal.M.t
                  and type st = SVal.SESubst.t
                  and type store_t = SStore.t
                  and type preds_t = Preds.SPreds.t)
    (External : External.S) =
struct
  module L = Logging
  module SSubst = SVal.SESubst

  module SAInterpreter =
    GInterpreter.Make (SVal.M) (SVal.SESubst) (SStore) (SPState) (External)

  module Normaliser = Normaliser.Make (SPState)
  module SPState = SPState

  type st = SPState.st
  type state = SPState.t
  type heap_t = SPState.heap_t
  type m_err = SPState.m_err_t

  let print_success_or_failure success =
    if success then Fmt.pr "%a" (Fmt.styled `Green Fmt.string) "Success\n"
    else Fmt.pr "%a" (Fmt.styled `Red Fmt.string) "Failure\n";
    Format.print_flush ()

  type t = {
    name : string;
    id : int * int;
    params : string list;
    pre_state : SPState.t;
    post_up : UP.t;
    flag : Flag.t option;
    spec_vars : Expr.Set.t;
  }

  let global_results = VerificationResults.make ()
  let start_time = ref 0.

  let reset () =
    VerificationResults.reset global_results;
    SAInterpreter.reset ()

  let testify
      (preds : (string, UP.pred) Hashtbl.t)
      (pred_ins : (string, int list) Hashtbl.t)
      (name : string)
      (params : string list)
      (id : int)
      (pre : Asrt.t)
      (posts : Asrt.t list)
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
        let simple_posts = List.map (fun post -> (post, (label, None))) posts in
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
            let pre' = Asrt.star (SPState.to_assertions ss_pre) in
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
          List.mapi test_of_normalised_state normalised_assertions
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
      (preds : UP.preds_tbl_t)
      (pred_ins : (string, int list) Hashtbl.t)
      (name : string)
      (params : string list)
      (id : int)
      (sspec : Spec.st) : (t option * Spec.st option) list =
    let ( let+ ) x f = List.map f x in
    let+ stest, sspec' =
      testify preds pred_ins name params id sspec.ss_pre sspec.ss_posts
        (Some sspec.ss_flag)
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
              testify_sspec preds pred_ins spec.spec_name spec.spec_params id
                sspec
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
        (fun Lemma.{ lemma_hyp; lemma_concs } ->
          testify preds pred_ins lemma.lemma_name lemma.lemma_params 0 lemma_hyp
            lemma_concs None None true)
        lemma.lemma_specs
    in
    let tests, specs =
      List.fold_left
        (fun (test_acc, spec_acc) (test_opt, spec_opt) ->
          let test_acc =
            match test_opt with
            | Some t -> t :: test_acc
            | None -> test_acc
          in
          let spec_acc =
            match spec_opt with
            | Some (lemma_hyp, lemma_concs) ->
                Lemma.{ lemma_hyp; lemma_concs } :: spec_acc
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

  (* let (tests, lemmas) = List.fold_left
     let tests = Option.fold ~some:(fun test -> [ test ]) ~none:[] test in
     match sspec with
     | Some (lemma_hyp, lemma_concs) ->
         (tests, { lemma with lemma_hyp; lemma_concs })
     | None ->
         raise
           (Failure
              (Printf.sprintf "Could not testify lemma %s" lemma.lemma_name)) *)

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

    (* TODO: Understand if this should be done: setup all program variables in the subst *)
    SStore.iter (SPState.get_store state) (fun v value ->
        if not (SSubst.mem subst (PVar v)) then SSubst.put subst (PVar v) value);

    (* Option.may (fun v_ret -> SSubst.put subst Names.return_variable v_ret)
       (SStore.get (SState.get_store state) Names.return_variable); *)
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

  let analyse_proc_results
      (test : t)
      (flag : Flag.t)
      (rets : SAInterpreter.result_t list) : bool =
    let success : bool =
      rets <> []
      && List.fold_left
           (fun ac result ->
             match (result : SAInterpreter.result_t) with
             | ExecRes.RFail (proc, i, state, errs) ->
                 L.verbose (fun m ->
                     m
                       "VERIFICATION FAILURE: Procedure %s, Command %d\n\
                        Spec %s %a\n\
                        @[<v 2>State:@\n\
                        %a@]@\n\
                        @[<v 2>Errors:@\n\
                        %a@]@\n"
                       proc i test.name
                       (Fmt.Dump.pair Fmt.int Fmt.int)
                       test.id SPState.pp state
                       Fmt.(list ~sep:(any "@\n") SAInterpreter.pp_err)
                       errs);
                 Fmt.pr "f @?";
                 false
             | ExecRes.RSucc (fl, _, state) ->
                 if Some fl <> test.flag then (
                   L.normal (fun m ->
                       m
                         "VERIFICATION FAILURE: Spec %s %a terminated with \
                          flag %s instead of %s\n"
                         test.name
                         (Fmt.Dump.pair Fmt.int Fmt.int)
                         test.id (Flag.str fl) (Flag.str flag));
                   Fmt.pr "f @?";
                   false)
                 else
                   let subst = make_post_subst test state in
                   if analyse_result subst test state then (
                     L.normal (fun m ->
                         m
                           "VERIFICATION SUCCESS: Spec %s %a terminated \
                            successfully\n"
                           test.name
                           (Fmt.Dump.pair Fmt.int Fmt.int)
                           test.id);
                     Fmt.pr "s @?";
                     ac)
                   else (
                     L.normal (fun m ->
                         m
                           "VERIFICATION FAILURE: Spec %s %a - post condition \
                            not unifiable\n"
                           test.name
                           (Fmt.Dump.pair Fmt.int Fmt.int)
                           test.id);
                     Fmt.pr "f @?";
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

  let analyse_lemma_results (test : t) (rets : SPState.t list) : bool =
    let success : bool =
      rets <> []
      && List.fold_left
           (fun ac final_state ->
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
              (List.length rets) SAInterpreter.pp_result rets);
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
      (prog : (Annot.t, int) Prog.t)
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
            (fun spec ->
              let tests, new_spec = testify_spec preds pred_ins spec in
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
      (prog : (Annot.t, int) Prog.t)
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

  let verify_up_to_procs (prog : (Annot.t, int) Prog.t) :
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
      (prog : (Annot.t, int) Prog.t)
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
