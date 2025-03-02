open Containers

module type S = sig
  type init_data
  type annot

  val test_prog :
    init_data:init_data ->
    ?call_graph:Call_graph.t ->
    annot MP.prog ->
    bool ->
    SourceFiles.t option ->
    unit
end

module Make
    (SPState : PState.S)
    (PC : ParserAndCompiler.S with type init_data = SPState.init_data)
    (External : External.T(PC.Annot).S) :
  S with type annot = PC.Annot.t and type init_data = PC.init_data = struct
  module L = Logging
  module SSubst = SVal.SESubst
  module Normaliser = Normaliser.Make (SPState)
  module SBAState = BiState.Make (SPState)

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
      (_ : annot MP.prog)
      (name : string)
      (params : string list)
      (bi_state_i : bi_state_t)
      (bi_state_f : bi_state_t)
      (fl : Flag.t) : Spec.t option =
    let open Syntaxes.List in
    let sspecs =
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
      (* Drop all pvars except ret/err from the state *)
      let () =
        SStore.filter_map_inplace (SPState.get_store state_f) (fun x v ->
            if x = Names.return_variable then Some v else None)
      in
      let* post =
        let _, finals_simplified =
          SPState.simplify ~kill_new_lvars:true state_f
        in
        let+ final_simplified = finals_simplified in
        List.sort Asrt.compare
          (SPState.to_assertions ~to_keep:pvars final_simplified)
      in

      let+ pre =
        let af_asrt = SPState.to_assertions state_af in
        let af_subst = make_id_subst af_asrt in
        let* af_produce_res = SPState.produce state_i af_subst af_asrt in
        match af_produce_res with
        | Ok state_i' ->
            let _, simplifieds =
              SPState.simplify ~kill_new_lvars:true state_i'
            in
            let+ simplified = simplifieds in
            List.sort Asrt.compare
              (SPState.to_assertions ~to_keep:pvars simplified)
        | Error _ ->
            L.verbose (fun m -> m "Failed to produce anti-frame");
            []
      in
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
      Spec.
        {
          ss_pre = (pre, None);
          ss_posts = [ (new_post, None) ];
          ss_variant = None;
          ss_flag = fl;
          ss_to_verify = false;
          ss_label = None;
        }
    in
    match sspecs with
    | [] -> None
    | sspecs ->
        let spec =
          Spec.
            {
              spec_name = name;
              spec_params = params;
              spec_sspecs = sspecs;
              spec_normalised = true;
              spec_incomplete = true;
              spec_to_verify = false;
            }
        in
        L.verbose (fun m ->
            m
              "@[<v 2>Created a spec for @[<h>%s(%a)@] successfully. Here is \
               the spec:@\n\
               %a@]"
              name
              Fmt.(list ~sep:comma string)
              params Spec.pp spec);
        Some spec

  let testify ~init_data ~(prog : annot MP.prog) (bi_spec : BiSpec.t) : t list =
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
    List.concat_map (fun (pre, _) -> make_test pre) bi_spec.bispec_pres

  let run_test
      (ret_fun : result_t -> (Spec.t * Flag.t) option)
      (prog : annot MP.prog)
      (test : t) : (Spec.t * Flag.t) list =
    let state = SBAState.copy test.state in
    let state = SBAState.add_spec_vars state (SBAState.get_lvars state) in
    try
      let opt_results =
        SBAInterpreter.evaluate_proc ret_fun prog test.name test.params state
      in
      let count = ref 0 in
      let result =
        List.filter_map
          (function
            | None ->
                incr count;
                None
            | x -> x)
          opt_results
      in
      if !count > 0 then
        L.verbose (fun m ->
            m "WARNING: %d vanished while creating the for %s!" !count test.name);
      result
    with Failure msg ->
      L.print_to_all
        (Printf.sprintf "ERROR in proc %s with message:\n%s\n" test.name msg);
      []

  let process_sym_exec_result
      (prog : annot MP.prog)
      (name : string)
      (params : string list)
      (state_i : bi_state_t)
      (result : result_t) : (Spec.t * Flag.t) option =
    let open Syntaxes.Option in
    let process_spec = make_spec prog in
    let state_i = SBAState.copy state_i in
    let add_spec spec =
      try MP.add_spec prog spec
      with _ ->
        L.fail
          (Format.asprintf "When trying to build an MP for %s, I died!" name)
    in
    match result with
    | RFail { error_state; errors; _ } ->
        (* Because of fixes, the state may have changed since between the start of execution
           and the failure: the anti-frame might have been modified before immediately erroring.
           BiState thus returns, with every error, the state immeditaly before the error happens
           with any applied fixes - this is the state that should be used to ensure fixes
           don't get lost. *)
        let rec find_error_state (aux : SBAInterpreter.err_t list) =
          match aux with
          | [] -> error_state
          | e :: errs -> (
              match e with
              | EState (EMem (s, _)) -> s
              | _ -> find_error_state errs)
        in
        let error_state = find_error_state errors in
        let+ spec = process_spec name params state_i error_state Flag.Bug in
        add_spec spec;
        (spec, Flag.Bug)
    | RSucc { flag; final_state; _ } ->
        let+ spec = process_spec name params state_i final_state flag in
        add_spec spec;
        (spec, flag)

  type proc_stats = {
    gil_size : int;
    mutable tests : int;
    mutable succs : int;
    mutable bugs : int;
    mutable time : float;
  }

  let pp_proc_stats fmt { gil_size; tests; succs; bugs; time } =
    Fmt.pf fmt "%d, %d, %d, %d, %f" gil_size tests succs bugs time

  let run_tests (prog : annot MP.prog) (tests : t list) =
    let num_tests = List.length tests in
    Fmt.pr "Running tests on %d procs.\n@?" num_tests;

    let stats : (string * proc_stats) list ref = ref [] in
    let get_stats name get_gil_size =
      match List.assoc_opt name !stats with
      | Some s -> s
      | None ->
          let gil_size = get_gil_size () in
          let s = { gil_size; tests = 0; succs = 0; bugs = 0; time = 0.0 } in
          stats := (name, s) :: !stats;
          s
    in

    let rec run_tests_aux tests succ_specs err_specs bug_specs i =
      match tests with
      | [] -> (succ_specs, err_specs, bug_specs)
      | test :: rest ->
          let rec part3 = function
            | [] -> ([], [], [])
            | (spec, flag) :: rest -> (
                let succ_specs, err_specs, bug_specs = part3 rest in
                match flag with
                | Flag.Normal -> (spec :: succ_specs, err_specs, bug_specs)
                | Flag.Error -> (succ_specs, spec :: err_specs, bug_specs)
                | Flag.Bug -> (succ_specs, err_specs, spec :: bug_specs))
          in
          L.verbose (fun m -> m "Running bi-abduction on %s\n" test.name);
          Fmt.pr "Testing %s... @?" test.name;
          let start_time = Sys.time () in
          let rets =
            run_test
              (process_sym_exec_result prog test.name test.params test.state)
              prog test
          in
          let end_time = Sys.time () in
          let stats =
            get_stats test.name (fun () ->
                match Hashtbl.find_opt prog.prog.procs test.name with
                | None -> -1
                | Some prog -> Array.length prog.proc_body)
          in
          let cur_succ_specs, cur_err_specs, cur_bug_specs = part3 rets in
          let new_succ_specs = succ_specs @ cur_succ_specs in
          let new_err_specs = err_specs @ cur_err_specs in
          let new_bug_specs = bug_specs @ cur_bug_specs in

          stats.tests <- stats.tests + 1;
          stats.succs <- stats.succs + List.length cur_succ_specs;
          stats.bugs <- stats.bugs + List.length cur_bug_specs;
          stats.time <- stats.time +. (end_time -. start_time);
          Fmt.pr "%dS %dB (%d/%d)\n@?"
            (List.length cur_succ_specs)
            (List.length cur_bug_specs)
            i num_tests;

          run_tests_aux rest new_succ_specs new_err_specs new_bug_specs (i + 1)
    in
    let result = run_tests_aux tests [] [] [] 1 in
    Fmt.pr "\nTest results:\nProc, GIL Commands, Tests, Succs, Bugs, Time\n";
    !stats
    |> List.sort (fun (name1, _) (name2, _) -> String.compare name1 name2)
    |> List.iter (fun (name, stats) ->
           Fmt.pr "%s, %a\n" name pp_proc_stats stats);
    Fmt.pr "@?";
    result

  let get_test_results
      (prog : annot MP.prog)
      (succ_specs : Spec.t list)
      (error_specs : Spec.t list)
      (bug_specs : Spec.t list) =
    let sort_specs =
      List.sort
        (fun Spec.{ spec_name = name1; _ } Spec.{ spec_name = name2; _ } ->
          String.compare name1 name2)
    in
    let bug_specs_txt =
      Format.asprintf "@[<v 2>BUG SPECS:@\n%a@]@\n"
        Fmt.(list ~sep:(any "@\n") (MP.pp_spec ~preds:prog.preds))
        (sort_specs bug_specs)
    in
    let error_specs_txt =
      Format.asprintf "@[<v 2>ERROR SPECS:@\n%a@]@\n"
        Fmt.(list ~sep:(any "@\n") (MP.pp_spec ~preds:prog.preds))
        (sort_specs error_specs)
    in
    let normal_specs_txt =
      Format.asprintf "@[<v 2>SUCCESSFUL SPECS:@\n%a@]@\n"
        Fmt.(list ~sep:(any "@\n") (MP.pp_spec ~preds:prog.preds))
        (sort_specs succ_specs)
    in

    if !Config.specs_to_stdout then (
      L.print_to_all bug_specs_txt;
      L.print_to_all error_specs_txt;
      L.print_to_all normal_specs_txt)
    else (
      L.normal (fun m -> m "%s" bug_specs_txt);
      L.normal (fun m -> m "%s" error_specs_txt);
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

  let sort_tests_by_callgraph tests callgraph =
    let rec aux acc rest_tests = function
      | [] -> (acc, rest_tests)
      | name :: rest ->
          let selected_tests, rest_tests =
            List.partition (fun t -> t.name = name) rest_tests
          in
          aux (acc @ selected_tests) rest_tests rest
    in
    let callgraph_order = Call_graph.get_sorted_names callgraph in
    let sorted_tests, rest_tests = aux [] tests callgraph_order in
    let rest_sorted_tests =
      rest_tests |> List.sort (fun t1 t2 -> Stdlib.compare t1.name t2.name)
    in
    sorted_tests @ rest_sorted_tests

  let test_procs ~init_data ~call_graph (prog : annot MP.prog) =
    L.verbose (fun m -> m "Starting bi-abductive testing in normal mode");
    let proc_names = Prog.get_noninternal_proc_names prog.prog in
    L.verbose (fun m -> m "Proc names: %s" (str_concat proc_names));
    let bi_specs = List.map (Prog.get_bispec_exn prog.prog) proc_names in

    let tests = List.concat_map (testify ~init_data ~prog) bi_specs in
    let test_names tests = str_concat (List.map (fun t -> t.name) tests) in
    L.verbose (fun m -> m "I have tests for: %s" (test_names tests));
    let tests = sort_tests_by_callgraph tests call_graph in
    L.verbose (fun m -> m "I have tests for: %s" (test_names tests));
    let succ_specs, err_specs, bug_specs = run_tests prog tests in
    get_test_results prog succ_specs err_specs bug_specs

  let specs_equal (spec_a : Spec.t) (spec_b : Spec.t) =
    (* FIXME: Perform a more robust comparsion based on matching *)
    String.equal (Spec.hash_of_t spec_a) (Spec.hash_of_t spec_b)

  let test_procs_incrementally
      (prog : annot MP.prog)
      ~(init_data : SPState.init_data)
      ~(prev_results : BiAbductionResults.t)
      ~(reverse_graph : Call_graph.t)
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
    List.iter (fun spec -> MP.add_spec prog spec) prev_specs;

    let rec test_procs_aux to_test checked succ_specs error_specs bug_specs =
      (* FIXME: Keep tests in a heap/priority queue *)
      match to_test with
      | [] -> (succ_specs, error_specs, bug_specs)
      | proc_name :: rest ->
          let () = MP.remove_spec prog proc_name in
          let tests =
            testify ~init_data ~prog (Prog.get_bispec_exn prog.prog proc_name)
          in
          let cur_succ_specs, cur_err_specs, cur_bug_specs =
            run_tests prog tests
          in
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
          let new_err_specs = error_specs @ cur_err_specs in
          let new_bug_specs = bug_specs @ cur_bug_specs in
          test_procs_aux new_to_test checked new_succ_specs new_err_specs
            new_bug_specs
    in
    (* Keep list sorted to ensure tests are executed in the required order *)
    let to_test = List.sort Stdlib.compare to_test in
    L.verbose (fun m -> m "I will begin by checking: %s" (str_concat to_test));
    let succ_specs, error_specs, bug_specs =
      test_procs_aux to_test SS.empty [] [] []
    in
    get_test_results prog succ_specs error_specs bug_specs

  let test_prog
      ~init_data
      ?(call_graph = Call_graph.make ())
      (prog : annot MP.prog)
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
      let reverse_graph = Call_graph.to_reverse_graph prev_call_graph in
      let cur_results =
        test_procs_incrementally prog ~init_data ~prev_results ~reverse_graph
          ~changed_procs:proc_changes.changed_procs ~to_test
      in
      let cur_call_graph = SBAInterpreter.call_graph in
      let () = Call_graph.prune_procs prev_call_graph to_prune in
      let call_graph = Call_graph.merge prev_call_graph cur_call_graph in
      let results = BiAbductionResults.merge prev_results cur_results in
      let diff = Fmt.str "%a" ChangeTracker.pp_proc_changes proc_changes in
      write_biabduction_results cur_source_files call_graph ~diff results
    else
      (* Test all procedures *)
      let cur_source_files =
        Option.value ~default:(SourceFiles.make ()) source_files
      in
      let dyn_call_graph = SBAInterpreter.call_graph in
      let results = test_procs ~init_data ~call_graph prog in
      write_biabduction_results cur_source_files dyn_call_graph ~diff:"" results
end

module From_scratch
    (SMemory : SMemory.S)
    (PC : ParserAndCompiler.S with type init_data = SMemory.init_data)
    (External : External.T(PC.Annot).S) =
  Make (PState.Make (SState.Make (SMemory))) (PC) (External)
