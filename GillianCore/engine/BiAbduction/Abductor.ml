open Containers

module type S = sig
  type init_data
  type annot

  val test_prog :
    init_data:init_data -> annot UP.prog -> bool -> SourceFiles.t option -> unit
end

module Make
    (SPState : PState.S
                 with type vt = SVal.M.t
                  and type st = SVal.SESubst.t
                  and type store_t = SStore.t
                  and type preds_t = Preds.SPreds.t)
    (PC : ParserAndCompiler.S with type init_data = SPState.init_data)
    (External : External.T(PC.Annot).S) :
  S with type annot = PC.Annot.t and type init_data = PC.init_data = struct
  module L = Logging
  module SSubst = SVal.SESubst
  module Normaliser = Normaliser.Make (SPState)
  module SBAState = BiState.Make (SVal.M) (SVal.SESubst) (SStore) (SPState)

  module SBAInterpreter =
    G_interpreter.Make (SVal.M) (SVal.SESubst) (SStore) (SBAState) (PC)
      (External)

  type bi_state_t = SBAState.t
  type result_t = SBAInterpreter.result_t
  type t = { name : string; params : string list; state : bi_state_t }
  type annot = PC.Annot.t
  type init_data = PC.init_data

  let make_id_subst (a : Asrt.t) : SSubst.t =
    let lvars = Asrt.lvars a in
    let alocs = Asrt.alocs a in
    let lvar_bindings =
      List.map (fun x -> (Expr.LVar x, Expr.LVar x)) (SS.elements lvars)
    in
    let aloc_bindings =
      List.map (fun x -> (Expr.LVar x, Expr.ALoc x)) (SS.elements alocs)
    in
    let bindings = lvar_bindings @ aloc_bindings in
    let bindings' =
      List.map
        (fun (x, e) ->
          match SVal.M.from_expr e with
          | Some v -> (x, v)
          | _ -> raise (Failure "DEATH. make_id_subst"))
        bindings
    in
    SSubst.init bindings'

  let make_spec
      (_ : annot UP.prog)
      (name : string)
      (params : string list)
      (bi_state_i : bi_state_t)
      (bi_state_f : bi_state_t)
      (fl : Flag.t) : Spec.t * Spec.t =
    (* let start_time = time() in *)

    (* HMMMMM *)
    (* let _              = SBAState.simplify ~kill_new_lvars:true bi_state_f in *)
    let state_i, _ = SBAState.get_components bi_state_i in
    let state_f, state_af = SBAState.get_components bi_state_f in
    let pvars = SS.of_list (Names.return_variable :: params) in

    L.verbose (fun m ->
        m
          "Going to create a spec for @[<h>%s(%a)@]\n\
           @[<v 2>AF:@\n\
           %a@]@\n\
           @[<v 2>Final STATE:@\n\
           %a@]"
          name
          Fmt.(list ~sep:comma string)
          params SPState.pp state_af SPState.pp state_f);

    (* Drop all pvars except ret/err from state *)
    let () =
      SStore.filter (SPState.get_store state_f) (fun x v ->
          if x = Names.return_variable then Some v else None)
    in
    let post, spost =
      (* FIXME: NOT WORKING DUE TO SIMPLIFICATION TYPE CHANGING *)
      let _ = SPState.simplify ~kill_new_lvars:true state_f in
      (* TODO: Come up with a generic cleaning mechanism *)
      (* if ((name <> "main") && !Config.js) then (
           let state_f'  = SPState.copy state_f in
           (* let state_f'  = JSCleanUp.exec prog state_f' name false in  *)
           let post  = Asrt.star (List.sort Asrt.compare (SPState.to_assertions ~to_keep:pvars state_f)) in
           let spost = Asrt.star (List.sort Asrt.compare (SPState.to_assertions ~to_keep:pvars state_f')) in
           post, spost
         ) else ( *)
      let post =
        Asrt.star
          (List.sort Asrt.compare
             (SPState.to_assertions ~to_keep:pvars state_f))
      in
      (post, post)
      (* ) *)
    in

    let pre, spre =
      let af_asrt = Asrt.star (SPState.to_assertions state_af) in
      let af_subst = make_id_subst af_asrt in
      match SPState.produce state_i af_subst af_asrt with
      | Ok [ state_i' ] ->
          (* FIXME: NOT WORKING DUE TO SIMPLIFICATION TYPE CHANGING *)
          let _ = SPState.simplify ~kill_new_lvars:true state_i' in
          let pre =
            Asrt.star
              (List.sort Asrt.compare
                 (SPState.to_assertions ~to_keep:pvars state_i'))
          in
          (* TODO: Come up with a generic cleaning mechanism *)
          (* let spre = (match (name <> "main") && !Config.js with
             | false -> pre
             | true ->
                 let state_i'' = SPState.copy state_i' in
                 (* let state_i'' = JSCleanUp.exec prog state_i'' name true in  *)
                 Asrt.star (List.sort Asrt.compare (SPState.to_assertions ~to_keep:pvars state_i''))) in *)
          (pre, pre)
      | Ok _ -> failwith "Bi-abduction: anti-frame branched"
      | Error _ ->
          raise
            (Failure "Bi-abduction: cannot produce anti-frame in initial state")
    in

    let make_spec_aux pre post =
      let post_clocs = Asrt.clocs post in
      let pre_clocs = Asrt.clocs pre in
      let new_clocs = SS.diff post_clocs pre_clocs in
      let subst = Hashtbl.create Config.medium_tbl_size in
      List.iter
        (fun cloc -> Hashtbl.replace subst cloc (Expr.ALoc (ALoc.alloc ())))
        (SS.elements new_clocs);
      let subst_fun cloc =
        match Hashtbl.find_opt subst cloc with
        | Some e -> e
        | None -> Lit (Loc cloc)
      in
      let new_post = Asrt.subst_clocs subst_fun post in

      let spec : Spec.t =
        {
          spec_name = name;
          spec_params = params;
          spec_sspecs =
            [
              {
                ss_pre = pre;
                ss_posts = [ new_post ];
                (* FIXME: Understand variant, but probably nothing can be done automatically *)
                ss_variant = None;
                ss_flag = fl;
                ss_to_verify = false;
                ss_label = None;
              };
            ];
          spec_normalised = true;
          spec_incomplete = true;
          spec_to_verify = false;
        }
      in
      spec
    in

    let spec = make_spec_aux pre post in
    let sspec = make_spec_aux spre spost in

    L.verbose (fun m ->
        m
          "@[<v 2>Created a spec for @[<h>%s(%a)@] successfully. Here is the \
           spec:@\n\
           %a@]"
          name
          Fmt.(list ~sep:comma string)
          params Spec.pp spec);

    (* update_statistics "make_spec_AbsBi" (time() -. start_time); *)
    (sspec, spec)

  let testify ~init_data ~(prog : annot UP.prog) (bi_spec : BiSpec.t) : t list =
    L.verbose (fun m -> m "Bi-testifying: %s" bi_spec.bispec_name);
    let proc_names = Prog.get_proc_names prog.prog in
    let params = SS.of_list bi_spec.bispec_params in
    let normalise =
      Normaliser.normalise_assertion ~init_data ~pred_defs:prog.preds
        ~pvars:params
    in
    let make_test asrt =
      match normalise asrt with
      | Error _ -> []
      | Ok l ->
          List.map
            (fun (ss_pre, _) ->
              {
                name = bi_spec.bispec_name;
                params = bi_spec.bispec_params;
                state =
                  SBAState.make ~procs:(SS.of_list proc_names) ~state:ss_pre
                    ~init_data;
              })
            l
    in
    List.concat_map make_test bi_spec.bispec_pres

  let run_test
      (ret_fun : result_t -> Spec.t * bool)
      (prog : annot UP.prog)
      (test : t) : (Spec.t * bool) list =
    let state = SBAState.copy test.state in
    let state = SBAState.add_spec_vars state (SBAState.get_lvars state) in
    try SBAInterpreter.evaluate_proc ret_fun prog test.name test.params state
    with Failure msg ->
      L.print_to_all
        (Printf.sprintf "ERROR in proc %s with message:\n%s\n" test.name msg);
      []

  let process_sym_exec_result
      (prog : annot UP.prog)
      (name : string)
      (params : string list)
      (state_i : bi_state_t)
      (result : result_t) : Spec.t * bool =
    let process_spec = make_spec prog in
    let state_i = SBAState.copy state_i in
    match result with
    | RFail { error_state; _ } ->
        let sspec, spec =
          process_spec name params state_i error_state Flag.Error
        in
        if !Config.bug_specs_propagation then UP.add_spec prog spec;
        (sspec, false)
    | RSucc { flag; final_state; _ } ->
        let sspec, spec = process_spec name params state_i final_state flag in
        let () =
          try UP.add_spec prog spec
          with _ ->
            L.fail
              (Format.asprintf "When trying to build an UP for %s, I died!" name)
        in
        (sspec, true)

  let run_tests (prog : annot UP.prog) (tests : t list) =
    let num_tests = List.length tests in
    Fmt.pr "Running tests on %d procs.\n@?" num_tests;
    let rec run_tests_aux tests succ_specs bug_specs i =
      match tests with
      | [] -> (succ_specs, bug_specs)
      | test :: rest ->
          L.verbose (fun m -> m "Running bi-abduction on %s\n" test.name);
          Fmt.pr "Testing %s... @?" test.name;
          let rets =
            run_test
              (process_sym_exec_result prog test.name test.params test.state)
              prog test
          in
          let cur_succ_specs, cur_bug_specs = List.partition snd rets in
          let new_succ_specs = succ_specs @ List.map fst cur_succ_specs in
          let new_bug_specs = bug_specs @ List.map fst cur_bug_specs in
          Fmt.pr "%dS %dB (%d/%d)\n@?"
            (List.length cur_succ_specs)
            (List.length cur_bug_specs)
            i num_tests;
          run_tests_aux rest new_succ_specs new_bug_specs (i + 1)
    in
    run_tests_aux tests [] [] 1

  let get_test_results
      (prog : annot UP.prog)
      (succ_specs : Spec.t list)
      (bug_specs : Spec.t list) =
    let succ_specs, error_specs =
      List.partition
        (fun (spec : Spec.t) ->
          match spec.spec_sspecs with
          | [ sspec ] -> sspec.ss_flag = Flag.Normal
          | _ -> false)
        succ_specs
    in

    let bug_specs_txt =
      Format.asprintf "@[<v 2>BUG SPECS:@\n%a@]@\n"
        Fmt.(list ~sep:(any "@\n") (UP.pp_spec ~preds:prog.preds))
        bug_specs
    in
    let normal_specs_txt =
      Format.asprintf "@[<v 2>SUCCESSFUL SPECS:@\n%a@]@\n"
        Fmt.(list ~sep:(any "@\n") (UP.pp_spec ~preds:prog.preds))
        succ_specs
    in

    if !Config.specs_to_stdout then (
      L.print_to_all bug_specs_txt;
      L.print_to_all normal_specs_txt)
    else (
      L.normal (fun m -> m "%s" bug_specs_txt);
      L.normal (fun m -> m "%s" normal_specs_txt));

    (* This is a hack to not count auxiliary functions that are bi-abduced *)
    let len_succ = List.length succ_specs in
    let auxiliaries =
      List.fold_left
        (fun ac (spec : Spec.t) ->
          ac || String.equal spec.spec_name "assumeType")
        false succ_specs
    in
    let offset = if auxiliaries then 12 else 0 in
    let len_succ = len_succ - offset in
    let () =
      L.print_to_all
        (Printf.sprintf "SUCCESS SPECS: %d\nERROR SPECS: %d\nBUG SPECS: %d"
           len_succ (List.length error_specs) (List.length bug_specs))
    in
    let results = BiAbductionResults.make () in
    let () =
      List.iter
        (fun (spec : Spec.t) ->
          BiAbductionResults.set_spec results spec.spec_name spec)
        (Prog.get_specs prog.prog)
    in
    results

  let str_concat = String.concat ", "

  let test_procs ~init_data (prog : annot UP.prog) =
    L.verbose (fun m -> m "Starting bi-abductive testing in normal mode");
    let proc_names = Prog.get_noninternal_proc_names prog.prog in
    L.verbose (fun m -> m "Proc names: %s" (str_concat proc_names));
    let bi_specs = List.map (Prog.get_bispec_exn prog.prog) proc_names in

    let tests = List.concat_map (testify ~init_data ~prog) bi_specs in
    let test_names tests = str_concat (List.map (fun t -> t.name) tests) in
    L.verbose (fun m -> m "I have tests for: %s" (test_names tests));
    let tests = List.sort (fun t1 t2 -> Stdlib.compare t1.name t2.name) tests in
    L.verbose (fun m -> m "I have tests for: %s" (test_names tests));
    let succ_specs, bug_specs = run_tests prog tests in
    get_test_results prog succ_specs bug_specs

  let specs_equal (spec_a : Spec.t) (spec_b : Spec.t) =
    (* FIXME: Perform a more robust comparsion based on unification *)
    String.equal (Spec.hash_of_t spec_a) (Spec.hash_of_t spec_b)

  let test_procs_incrementally
      (prog : annot UP.prog)
      ~(init_data : SPState.init_data)
      ~(prev_results : BiAbductionResults.t)
      ~(reverse_graph : CallGraph.t)
      ~(changed_procs : string list)
      ~(to_test : string list) =
    L.verbose (fun m -> m "Starting bi-abductive testing in incremental mode");
    let to_test_set = SS.of_list to_test in
    let filter spec_name = not (SS.mem spec_name to_test_set) in
    let prev_specs = BiAbductionResults.get_all_specs ~filter prev_results in
    let prev_spec_names =
      str_concat (List.map (fun (s : Spec.t) -> s.spec_name) prev_specs)
    in
    L.verbose (fun m -> m "I will use the stored specs of: %s" prev_spec_names);
    List.iter (fun spec -> UP.add_spec prog spec) prev_specs;

    let rec test_procs_aux to_test checked succ_specs bug_specs =
      (* FIXME: Keep tests in a heap/priority queue *)
      match to_test with
      | [] -> (succ_specs, bug_specs)
      | proc_name :: rest ->
          let () = UP.remove_spec prog proc_name in
          let tests =
            testify ~init_data ~prog (Prog.get_bispec_exn prog.prog proc_name)
          in
          let cur_succ_specs, cur_bug_specs = run_tests prog tests in
          let checked = SS.add proc_name checked in
          let new_to_test =
            if BiAbductionResults.contains_spec prev_results proc_name then
              let prev_spec =
                BiAbductionResults.get_spec_exn prev_results proc_name
              in
              let new_spec = (Hashtbl.find prog.specs proc_name).data in
              if not (specs_equal prev_spec new_spec) then (
                L.verbose (fun m -> m "The spec of %s has changed" proc_name);
                (* Must also check immediate callers *)
                let callers =
                  ChangeTracker.get_callers prog.prog ~reverse_graph
                    ~excluded_procs:changed_procs ~proc_name
                in
                let callers =
                  List.filter (fun name -> not (SS.mem name checked)) callers
                in
                L.verbose (fun m ->
                    m "I will check its callers: %s" (str_concat callers));
                List.sort Stdlib.compare (rest @ callers))
              else (
                L.verbose (fun m -> m "The spec of %s is unchanged" proc_name);
                rest)
            else rest
          in
          let new_succ_specs = succ_specs @ cur_succ_specs in
          let new_bug_specs = bug_specs @ cur_bug_specs in
          test_procs_aux new_to_test checked new_succ_specs new_bug_specs
    in
    (* Keep list sorted to ensure tests are executed in the required order *)
    let to_test = List.sort Stdlib.compare to_test in
    L.verbose (fun m -> m "I will begin by checking: %s" (str_concat to_test));
    let succ_specs, bug_specs = test_procs_aux to_test SS.empty [] [] in
    get_test_results prog succ_specs bug_specs

  let test_prog
      ~init_data
      (prog : annot UP.prog)
      (incremental : bool)
      (source_files : SourceFiles.t option) : unit =
    let open ResultsDir in
    let open ChangeTracker in
    if incremental && prev_results_exist () then
      (* Only test changed procedures *)
      let cur_source_files =
        match source_files with
        | Some files -> files
        | None -> failwith "Cannot use -a in incremental mode"
      in
      let prev_source_files, prev_call_graph, prev_results =
        read_biabduction_results ()
      in
      let proc_changes =
        get_changes prog.prog ~prev_source_files ~prev_call_graph
          ~cur_source_files
      in
      let to_test = proc_changes.changed_procs @ proc_changes.new_procs in
      let to_prune = proc_changes.changed_procs @ proc_changes.deleted_procs in
      let reverse_graph = CallGraph.to_reverse_graph prev_call_graph in
      let cur_results =
        test_procs_incrementally prog ~init_data ~prev_results ~reverse_graph
          ~changed_procs:proc_changes.changed_procs ~to_test
      in
      let cur_call_graph = SBAInterpreter.call_graph in
      let () = CallGraph.prune_procs prev_call_graph to_prune in
      let call_graph = CallGraph.merge prev_call_graph cur_call_graph in
      let results = BiAbductionResults.merge prev_results cur_results in
      let diff = Fmt.str "%a" ChangeTracker.pp_proc_changes proc_changes in
      write_biabduction_results cur_source_files call_graph ~diff results
    else
      (* Test all procedures *)
      let cur_source_files =
        Option.value ~default:(SourceFiles.make ()) source_files
      in
      let call_graph = SBAInterpreter.call_graph in
      let results = test_procs ~init_data prog in
      write_biabduction_results cur_source_files call_graph ~diff:"" results
end

module From_scratch
    (SMemory : SMemory.S)
    (PC : ParserAndCompiler.S with type init_data = SMemory.init_data)
    (External : External.T(PC.Annot).S) =
  Make
    (PState.Make (SVal.M) (SVal.SESubst) (SStore) (SState.Make (SMemory))
       (Preds.SPreds))
       (PC)
    (External)
