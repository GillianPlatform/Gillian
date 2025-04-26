open Containers
open Location
module DL = Debugger_log

module type S = sig
  type heap_t
  type state
  type m_err
  type annot

  module SPState :
    PState.S
      with type t = state
       and type heap_t = heap_t
       and type m_err_t = m_err

  module SAInterpreter :
    G_interpreter.S
      with type vt = Expr.t
       and type st = SVal.SESubst.t
       and type store_t = SStore.t
       and type state_t = state
       and type heap_t = heap_t
       and type state_err_t = SPState.err_t
       and type annot = annot

  module SMatcher : Matcher.S

  type t
  type prog_t = (annot, int) Prog.t
  type proc_tests = (string * t) list [@@deriving to_yojson]

  val start_time : float ref
  val reset : unit -> unit

  val verify_prog :
    init_data:SPState.init_data ->
    prog_t ->
    bool ->
    SourceFiles.t option ->
    unit Gillian_result.t

  val verify_up_to_procs :
    ?proc_name:string ->
    init_data:SPState.init_data ->
    prog_t ->
    SAInterpreter.result_t SAInterpreter.cont_func

  val postprocess_files : SourceFiles.t option -> unit

  module Debug : sig
    val get_tests_for_prog : init_data:SPState.init_data -> prog_t -> proc_tests

    val analyse_result :
      t -> Logging.Report_id.t -> SAInterpreter.result_t -> bool
  end
end

module Make
    (SState : SState.S
                with type vt = SVal.M.t
                 and type st = SVal.SESubst.t
                 and type store_t = SStore.t)
    (SPState : PState.S
                 with type state_t = SState.t
                  and type init_data = SState.init_data)
    (PC : ParserAndCompiler.S)
    (External : External.T(PC.Annot).S) =
struct
  module L = Logging
  module SSubst = SVal.SESubst
  module SPState = SPState

  module SAInterpreter =
    G_interpreter.Make (SVal.M) (SVal.SESubst) (SStore) (SPState) (PC)
      (External)

  module Normaliser = Normaliser.Make (SPState)

  type state = SPState.t
  type heap_t = SPState.heap_t
  type m_err = SPState.m_err_t
  type annot = PC.Annot.t

  module SMatcher = Matcher.Make (SState)

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
    post_mp : MP.t;
    post_loc : Location.t option;
    flag : Flag.t option;
    spec_vars : Expr.Set.t; [@to_yojson yojson_of_expr_set]
  }
  [@@deriving to_yojson]

  type prog_t = (annot, int) Prog.t
  type proc_tests = (string * t) list

  let proc_tests_to_yojson tests =
    `List
      (tests
      |> List.map (fun (name, test) -> `List [ `String name; to_yojson test ]))

  let global_results = VerificationResults.make ()
  let start_time = ref 0.

  let reset () =
    VerificationResults.reset global_results;
    SAInterpreter.reset_call_graph ()

  let testify
      ~(init_data : SPState.init_data)
      (func_or_lemma_name : string)
      (preds : (string, MP.pred) Hashtbl.t)
      (pred_ins : (string, int list) Hashtbl.t)
      (name : string)
      (params : string list)
      (id : int)
      (pre : Asrt.t located)
      (posts : Asrt.t located list)
      (variant : Expr.t option)
      (flag : Flag.t option)
      (label : (string * SS.t) option)
      (to_verify : bool) :
      (t option * (Asrt.t located * Asrt.t located list) option) list =
    let test_of_normalised_state id' (ss_pre, subst) =
      (* Step 2 - spec_vars = lvars(pre)\dom(subst) -U- alocs(range(subst)) *)
      let lvars =
        SS.fold
          (fun x acc ->
            if Names.is_spec_var_name x then Expr.Set.add (Expr.LVar x) acc
            else acc)
          (Asrt.lvars (fst pre))
          Expr.Set.empty
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
            spec_vars Asrt.pp (fst pre) SPState.pp ss_pre SSubst.pp subst
            (List.length posts)
            Fmt.(list ~sep:(any "@\n") Asrt.pp)
            (List.map fst posts));
      let subst =
        SSubst.filter subst (fun e _ ->
            match e with
            | PVar _ -> false
            | _ -> true)
      in
      let posts =
        List.filter_map
          (fun (p, loc) ->
            let substituted = SSubst.substitute_asrt subst ~partial:true p in
            let reduced = Reduction.reduce_assertion substituted in
            if Simplifications.admissible_assertion reduced then
              Some (reduced, loc)
            else None)
          posts
      in
      if not to_verify then
        let pre' = SPState.to_assertions ss_pre in
        (None, Some ((pre', snd pre), posts))
      else
        (* Step 4 - create a matching plan for the postconditions and s_test *)
        let () =
          L.verbose (fun fmt -> fmt "Creating MPs for posts of %s" name)
        in
        let pvar_params =
          List.fold_left
            (fun acc x -> Expr.Set.add (Expr.PVar x) acc)
            Expr.Set.empty params
        in
        let known_matchables =
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
        let known_matchables = Expr.Set.union known_matchables existentials in
        let simple_posts =
          List.map (fun (post, _) -> (post, (label, None))) posts
        in
        let post_mp =
          MP.init known_matchables Expr.Set.empty pred_ins simple_posts
        in
        L.verbose (fun m -> m "END of STEP 4@\n");
        match post_mp with
        | Error _ ->
            let msg =
              Printf.sprintf "Warning: testify failed for %s. Cause: post_mp \n"
                name
            in
            Printf.printf "%s" msg;
            L.verbose (fun m -> m "%s" msg);
            (None, None)
        | Ok post_mp ->
            let pre' = SPState.to_assertions ss_pre in
            let ss_pre =
              match flag with
              (* Lemmas should not have stores when being proven *)
              | None ->
                  let empty_store = SStore.init [] in
                  SPState.set_store ss_pre empty_store
              | Some _ -> ss_pre
            in
            let post_loc =
              List.fold_left
                (fun acc (_, loc) ->
                  match (acc, loc) with
                  | None, None -> None
                  | Some l, None | None, Some l -> Some l
                  | Some l1, Some l2 -> Some (Location.merge l1 l2))
                None posts
            in
            let test =
              {
                name;
                id = (id, id');
                params;
                pre_state = ss_pre;
                post_mp;
                flag;
                spec_vars;
                post_loc;
              }
            in
            (Some test, Some ((pre', snd pre), posts))
    in
    try
      (* Step 1 - normalise the precondition *)
      match
        Normaliser.normalise_assertion ~init_data ~pred_defs:preds
          ~pvars:(SS.of_list params) (fst pre)
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
      let msg =
        Fmt.str "Preprocessing %s failed during normalisation\n%s" name msg
      in
      let loc = snd pre in
      let error =
        Gillian_result.Error.(
          CompilationError { msg; loc; additional_data = None })
      in
      raise (Gillian_result.Exc.Gillian_error error)

  let testify_sspec
      ~init_data
      (spec_name : string)
      (preds : MP.preds_tbl_t)
      (pred_ins : (string, int list) Hashtbl.t)
      (name : string)
      (params : string list)
      (id : int)
      (sspec : Spec.st) : (t option * Spec.st option) list =
    let ( let+ ) x f = List.map f x in
    let+ stest, sspec' =
      testify ~init_data spec_name preds pred_ins name params id sspec.ss_pre
        sspec.ss_posts sspec.ss_variant (Some sspec.ss_flag)
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
      ~init_data
      (spec_name : string)
      (preds : MP.preds_tbl_t)
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
              testify_sspec ~init_data spec_name preds pred_ins spec.spec_name
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
      ~init_data
      (preds : MP.preds_tbl_t)
      (pred_ins : (string, int list) Hashtbl.t)
      (lemma : Lemma.t) : t list * Lemma.t =
    let tests_and_specs =
      List.concat_map
        (fun Lemma.{ lemma_hyp; lemma_concs; lemma_spec_variant } ->
          let to_verify = Option.is_some lemma.lemma_proof in
          testify ~init_data lemma.lemma_name preds pred_ins lemma.lemma_name
            lemma.lemma_params 0 lemma_hyp lemma_concs lemma_spec_variant None
            None to_verify)
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
                Lemma.
                  {
                    lemma_hyp;
                    lemma_concs;
                    lemma_spec_variant = lemma.lemma_variant;
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
        m "Analyse result: About to match one postcondition of %s. post: %a"
          test.name MP.pp test.post_mp);
    let matching_result =
      SPState.matches state subst test.post_mp (Postcondition test.name)
    in
    L.normal (fun m ->
        m "Analysis result: Postcondition %a"
          (fun ft b ->
            Fmt.string ft
            @@ if b then "matched successfully" else "not matchable")
          matching_result);
    VerificationResults.set_result global_results test.name test.id
      matching_result;
    matching_result

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

  let analyse_proc_result test flag ?parent_id result : unit Gillian_result.t =
    match (result : SAInterpreter.result_t) with
    | Exec_res.RFail { proc; proc_idx; error_state; errors; loc } ->
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
        Fmt.pr "f @?";
        let errors =
          errors
          |> List.map @@ fun e ->
             let msg = Fmt.str "%a" SAInterpreter.Logging.pp_err e in
             Gillian_result.Error.{ msg; loc }
        in
        Gillian_result.analysis_failures errors
    | Exec_res.RSucc { flag = fl; final_state; last_report; loc; _ } ->
        if Some fl <> test.flag then (
          let msg =
            Fmt.str "Terminated with flag %s instead of %s" (Flag.str fl)
              (Flag.str flag)
          in
          L.normal (fun m ->
              m "VERIFICATION FAILURE in spec %s %a: %s\n" test.name
                (Fmt.Dump.pair Fmt.int Fmt.int)
                test.id msg);
          Fmt.pr "f @?";
          Gillian_result.analysis_failures [ { msg; loc } ])
        else
          let parent_id =
            match parent_id with
            | None -> last_report
            | id -> id
          in
          DL.log (fun m ->
              m "Match: setting parent to %a"
                (Fmt.option L.Report_id.pp)
                parent_id);
          L.Parent.with_id parent_id @@ fun () ->
          let store = SPState.get_store final_state in
          let () =
            SStore.filter_map_inplace store (fun x v ->
                if x = Names.return_variable then Some v else None)
          in
          let subst = make_post_subst test final_state in
          if analyse_result subst test final_state then
            let () =
              L.normal (fun m ->
                  m "VERIFICATION SUCCESS: Spec %s %a terminated successfully\n"
                    test.name
                    (Fmt.Dump.pair Fmt.int Fmt.int)
                    test.id)
            in
            let () = Fmt.pr "s @?" in
            Ok ()
          else
            let msg = "Failed to match against postcondition" in
            let () =
              L.normal (fun m ->
                  m "VERIFICATION FAILURE in spec %s %a: %s\n" test.name
                    (Fmt.Dump.pair Fmt.int Fmt.int)
                    test.id msg)
            in
            let () = Fmt.pr "f @?" in
            Gillian_result.analysis_failures [ { msg; loc = test.post_loc } ]

  let analyse_proc_results
      (test : t)
      (flag : Flag.t)
      (rets : SAInterpreter.result_t list) : unit Gillian_result.t =
    if rets = [] then (
      L.(
        normal (fun m ->
            m "ERROR: Function %s evaluates to 0 results." test.name));
      exit 1);
    let result =
      List.fold_left
        (fun acc ret ->
          let res = analyse_proc_result test flag ret in
          Gillian_result.merge acc res)
        (Ok ()) rets
    in
    print_success_or_failure (Result.is_ok result);
    result

  let analyse_lemma_results (test : t) (rets : SPState.t list) :
      unit Gillian_result.t =
    let open Syntaxes.Result in
    let* () =
      if rets = [] then
        Gillian_result.internal_error "Lemma evaluates to 0 results."
      else Ok ()
    in
    let errors =
      rets
      |> List.filter_map @@ fun final_state ->
         let empty_store = SStore.init [] in
         let final_state = SPState.set_store final_state empty_store in
         let subst = make_post_subst test final_state in
         if analyse_result subst test final_state then
           let () =
             L.normal (fun m ->
                 m "VERIFICATION SUCCESS: Spec %s %a terminated successfully\n"
                   test.name
                   (Fmt.Dump.pair Fmt.int Fmt.int)
                   test.id)
           in
           None
         else
           let msg = "Failed to match against postcondition" in
           let () =
             L.normal (fun m ->
                 m "VERIFICATION FAILURE in spec %s %a: %s\n" test.name
                   (Fmt.Dump.pair Fmt.int Fmt.int)
                   test.id msg)
           in
           Some Gillian_result.Error.{ msg; loc = test.post_loc }
    in
    let result =
      match errors with
      | [] -> Ok ()
      | _ -> Gillian_result.analysis_failures errors
    in
    print_success_or_failure (Result.is_ok result);
    result

  (* FIXME: This function name is very bad! *)
  let verify_up_to_procs (prog : annot MP.prog) (test : t) : annot MP.prog =
    (* Printf.printf "Inside verify with a test for %s\n" test.name; *)
    match test.flag with
    | Some _ ->
        let msg = "Verifying one spec of procedure " ^ test.name ^ "... " in
        L.tmi (fun fmt -> fmt "%s" msg);
        Fmt.pr "%s@?" msg;
        (* Reset coverage for every procedure in verification *)
        { prog with coverage = Hashtbl.create 1 }
    | None -> raise (Failure "Debugging lemmas unsupported!")

  let verify (prog : annot MP.prog) (test : t) : unit Gillian_result.t =
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
              Gillian_result.operation_error
                (Fmt.str "Lemma %s WITHOUT proof" test.name)
            else Ok () (* It's already correct *)
        | Some proof -> (
            let msg = "Verifying lemma " ^ test.name ^ "... " in
            L.tmi (fun fmt -> fmt "%s" msg);
            Fmt.pr "%s@?" msg;
            let lemma_evaluation_results =
              SAInterpreter.evaluate_lcmds prog proof state
            in
            let successes, errors = Res_list.split lemma_evaluation_results in
            match errors with
            | [] -> analyse_lemma_results test successes
            | _ ->
                let errors =
                  errors
                  |> List.map @@ fun e ->
                     let msg = Fmt.str "%a" SPState.pp_err e in
                     Gillian_result.Error.{ msg; loc = None }
                in
                print_success_or_failure false;
                Gillian_result.analysis_failures errors))

  let pred_extracting_visitor =
    object
      inherit [_] Visitors.reduce
      inherit Visitors.Utils.ss_monoid
      method! visit_Pred _ pred_name _ = SS.singleton pred_name
      method! visit_Fold _ pred_name _ _ = SS.singleton pred_name
      method! visit_Unfold _ pred_name _ _ _ = SS.singleton pred_name
      method! visit_GUnfold _ pred_name = SS.singleton pred_name
    end

  let filter_internal_preds (prog : annot MP.prog) (pred_names : SS.t) =
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

  let filter_internal_lemmas (prog : annot MP.prog) (lemma_names : SS.t) =
    SS.filter
      (fun lemma_name ->
        let lemma = Prog.get_lemma_exn prog.prog lemma_name in
        not lemma.lemma_internal)
      lemma_names

  let record_proc_dependencies proc_name (prog : annot MP.prog) =
    let proc = Prog.get_proc_exn prog.prog proc_name in
    let preds_used =
      filter_internal_preds prog (pred_extracting_visitor#visit_proc () proc)
    in
    let lemmas_used =
      filter_internal_lemmas prog (lemma_extracting_visitor#visit_proc () proc)
    in
    SS.iter
      (Call_graph.add_proc_pred_use SAInterpreter.call_graph proc_name)
      preds_used;
    SS.iter
      (Call_graph.add_proc_lemma_use SAInterpreter.call_graph proc_name)
      lemmas_used

  let record_lemma_dependencies lemma_name (prog : annot MP.prog) =
    let lemma = Prog.get_lemma_exn prog.prog lemma_name in
    let preds_used =
      filter_internal_preds prog (pred_extracting_visitor#visit_lemma () lemma)
    in
    let lemmas_used =
      filter_internal_lemmas prog
        (lemma_extracting_visitor#visit_lemma () lemma)
    in
    SS.iter
      (Call_graph.add_lemma_pred_use SAInterpreter.call_graph lemma_name)
      preds_used;
    SS.iter
      (Call_graph.add_lemma_call SAInterpreter.call_graph lemma_name)
      lemmas_used

  let record_preds_used_by_pred pred_name (prog : annot MP.prog) =
    let pred = Prog.get_pred_exn prog.prog pred_name in
    let preds_used =
      filter_internal_preds prog (pred_extracting_visitor#visit_pred () pred)
    in
    SS.iter
      (Call_graph.add_pred_call SAInterpreter.call_graph pred_name)
      preds_used

  let check_previously_verified prev_results cur_verified =
    Option.fold ~none:true
      ~some:(fun res ->
        VerificationResults.check_previously_verified
          ~printer:print_success_or_failure res cur_verified)
      prev_results

  let get_tests_to_verify
      ~init_data
      (prog : prog_t)
      (pnames_to_verify : SS.t)
      (lnames_to_verify : SS.t) : annot MP.prog * t list * t list =
    let ipreds = MP.init_preds prog.preds in
    match ipreds with
    | Error e ->
        Fmt.pr "Creation of matching plans for predicates failed with:\n%a\n@?"
          MP.pp_err e;
        Fmt.failwith "Creation of matching plans for predicates failed."
    | Ok preds -> (
        let pred_ins =
          Hashtbl.fold
            (fun name (pred : MP.pred) pred_ins ->
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
                testify_spec ~init_data spec.spec_name preds pred_ins spec
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
              let tests, new_lemma =
                testify_lemma ~init_data preds pred_ins lemma
              in
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

        (* STEP 4: Create matching plans for specs and predicates *)
        (* Printf.printf "Creating matching plans: %f\n" (cur_time -. start_time); *)
        match MP.init_prog ~preds_tbl:preds prog with
        | Error e ->
            Fmt.failwith "Creation of matching plans failed:@\n %a@\n@?"
              MP.pp_err e
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
      ~(init_data : SPState.init_data)
      ?(prev_results : VerificationResults.t option)
      (prog : prog_t)
      (pnames_to_verify : SS.t)
      (lnames_to_verify : SS.t) : unit Gillian_result.t =
    let prog', tests', tests =
      get_tests_to_verify ~init_data prog pnames_to_verify lnames_to_verify
    in
    (* STEP 6: Run the symbolic tests *)
    let cur_time = Sys.time () in
    Printf.printf "Running symbolic tests: %f\n" (cur_time -. !start_time);
    let result =
      let rec aux = function
        | [], acc -> acc
        | _, acc when not (Gillian_result.should_continue acc) -> acc
        | test :: tests, acc ->
            let res = verify prog' test in
            aux (tests, Gillian_result.merge acc res)
      in
      aux (tests' @ tests, Ok ())
    in
    let end_time = Sys.time () in
    let cur_verified = SS.union pnames_to_verify lnames_to_verify in
    let success =
      Result.is_ok result && check_previously_verified prev_results cur_verified
    in
    let msg : string =
      if success then "All specs succeeded:" else "There were failures:"
    in
    let msg : string = Printf.sprintf "%s %f%!" msg (end_time -. !start_time) in
    Printf.printf "%s\n" msg;
    L.normal (fun m -> m "%s" msg);
    result

  let select_procs_and_lemmas ~procs_to_verify ~lemmas_to_verify =
    match !Config.Verification.things_to_verify with
    | All -> (procs_to_verify, lemmas_to_verify)
    | ProcsOnly -> (procs_to_verify, SS.empty)
    | LemmasOnly -> (SS.empty, lemmas_to_verify)
    | Specific ->
        ( SS.inter procs_to_verify
            (SS.of_list !Config.Verification.procs_to_verify),
          SS.inter lemmas_to_verify
            (SS.of_list !Config.Verification.lemmas_to_verify) )

  let verify_up_to_procs
      ?(proc_name : string option)
      ~(init_data : SPState.init_data)
      (prog : prog_t) : SAInterpreter.result_t SAInterpreter.cont_func =
    L.Phase.with_normal ~title:"Program verification" (fun () ->
        (* Analyse all procedures and lemmas *)
        let procs_to_verify =
          SS.of_list (Prog.get_noninternal_proc_names prog)
        in
        let lemmas_to_verify =
          SS.of_list (Prog.get_noninternal_lemma_names prog)
        in
        let procs_to_verify, lemmas_to_verify =
          select_procs_and_lemmas ~procs_to_verify ~lemmas_to_verify
        in
        let prog, _, proc_tests =
          get_tests_to_verify ~init_data prog procs_to_verify lemmas_to_verify
        in
        (* TODO: Verify All procedures. Currently we only verify the first
               procedure (unless specified).
               Assume there is at least one procedure*)
        let test =
          match proc_name with
          | Some proc_name -> (
              match
                proc_tests |> List.find_opt (fun test -> test.name = proc_name)
              with
              | Some test -> test
              | None ->
                  Fmt.failwith "Couldn't find test for proc '%s'!" proc_name)
          | None -> (
              match proc_tests with
              | test :: _ -> test
              | _ -> failwith "No tests found!")
        in
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
      ~(init_data : SPState.init_data)
      (prog : prog_t)
      (incremental : bool)
      (source_files : SourceFiles.t option) : unit Gillian_result.t =
    L.Phase.with_normal ~title:"Program verification" @@ fun () ->
    let open ResultsDir in
    let open ChangeTracker in
    if incremental && prev_results_exist () then (
      (* Only verify changed procedures and lemmas *)
      let cur_source_files =
        match source_files with
        | Some files -> files
        | None -> failwith "Cannot use -a in incremental mode"
      in
      let prev_source_files, prev_call_graph, results = read_verif_results () in
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
      let () = Call_graph.prune_procs prev_call_graph procs_to_prune in
      let () = Call_graph.prune_lemmas prev_call_graph lemmas_to_prune in
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
      if Config.Verification.(!things_to_verify <> All) then
        failwith
          "Cannot use --incremental while restricting procs and/or lemmas to \
           verify";
      let r =
        verify_procs ~init_data ~prev_results:results prog procs_to_verify
          lemmas_to_verify
      in
      let cur_call_graph = SAInterpreter.call_graph in
      let cur_results = global_results in
      let call_graph = Call_graph.merge prev_call_graph cur_call_graph in
      let results = VerificationResults.merge results cur_results in
      let diff = Fmt.str "%a" ChangeTracker.pp_proc_changes proc_changes in
      let () = write_verif_results cur_source_files call_graph ~diff results in
      r)
    else
      (* Analyse all procedures and lemmas *)
      let cur_source_files =
        Option.value ~default:(SourceFiles.make ()) source_files
      in
      let procs_to_verify = SS.of_list (Prog.get_noninternal_proc_names prog) in
      let lemmas_to_verify =
        SS.of_list (Prog.get_noninternal_lemma_names prog)
      in
      let procs_to_verify, lemmas_to_verify =
        select_procs_and_lemmas ~procs_to_verify ~lemmas_to_verify
      in
      let r = verify_procs ~init_data prog procs_to_verify lemmas_to_verify in
      let call_graph = SAInterpreter.call_graph in
      let () =
        write_verif_results cur_source_files call_graph ~diff:"" global_results
      in
      r

  module Debug = struct
    let get_tests_for_prog ~init_data (prog : prog_t) =
      let open Syntaxes.Option in
      let ipreds = MP.init_preds prog.preds in
      let preds = Result.get_ok ipreds in
      let pred_ins =
        Hashtbl.fold
          (fun name (pred : MP.pred) pred_ins ->
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
                 testify_spec ~init_data spec.spec_name preds pred_ins spec
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
      analyse_proc_result test Normal ~parent_id result |> Result.is_ok
  end
end

module From_scratch
    (SMemory : SMemory.S)
    (PC : ParserAndCompiler.S)
    (External : External.T(PC.Annot).S) =
struct
  module INTERNAL__ = struct
    module SState = SState.Make (SMemory)
  end

  include
    Make (INTERNAL__.SState) (PState.Make (INTERNAL__.SState)) (PC) (External)
end
