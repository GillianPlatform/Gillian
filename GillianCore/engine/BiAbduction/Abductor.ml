open Containers

module Make
    (SState : State.S
                with type vt = SVal.M.t
                 and type st = SVal.SSubst.t
                 and type store_t = SStore.t)
    (SPState : PState.S
                 with type vt = SVal.M.t
                  and type st = SVal.SSubst.t
                  and type store_t = SStore.t
                  and type preds_t = Preds.SPreds.t)
    (External : External.S) =
struct
  module L = Logging
  module SSubst = SVal.SSubst
  module Normaliser = Normaliser.Make (SPState)
  module SBAState = BiState.Make (SVal.M) (SVal.SSubst) (SStore) (SPState)
  module SBAInterpreter =
    GInterpreter.Make (SVal.M) (SVal.SSubst) (SStore) (SBAState) (External)

  type bi_state_t = SBAState.t

  type abs_state = SPState.t

  type result_t = SBAInterpreter.result_t

  type t = { name : string; params : string list; state : bi_state_t }

  let make_id_subst (a : Asrt.t) : SSubst.t =
    let lvars = Asrt.lvars a in
    let alocs = Asrt.alocs a in
    let lvar_bindings =
      List.map (fun x -> (x, Expr.LVar x)) (SS.elements lvars)
    in
    let aloc_bindings =
      List.map (fun x -> (x, Expr.ALoc x)) (SS.elements alocs)
    in
    let bindings = lvar_bindings @ aloc_bindings in
    let bindings' =
      List.map
        (fun (x, e) ->
          match SVal.M.from_expr e with
          | Some v -> (x, v)
          | _      -> raise (Failure "DEATH. make_id_subst"))
        bindings
    in
    SSubst.init bindings'

  let make_spec
      (prog : UP.prog)
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

    let post, spost =
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
      | Some state_i' ->
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
      | None          ->
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
        | None   -> Lit (Loc cloc)
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
                ss_flag = fl;
                ss_to_verify = false;
                ss_label = None;
              };
            ];
          spec_normalised = true;
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

  let testify (prog : UP.prog) (bi_spec : BiSpec.t) : t list =
    L.verbose (fun m -> m "Bi-testifying: %s" bi_spec.bispec_name);
    let proc_names = Prog.get_proc_names prog.prog in
    let params = SS.of_list bi_spec.bispec_params in
    let normalise =
      Normaliser.normalise_assertion ~pred_defs:prog.preds ~pvars:params
    in
    let make_test asrt =
      match normalise asrt with
      | None             -> None
      | Some (ss_pre, _) ->
          Some
            {
              name = bi_spec.bispec_name;
              params = bi_spec.bispec_params;
              state =
                SBAState.initialise (SS.of_list proc_names) ss_pre
                  (Some prog.preds);
            }
    in
    let rec filter_none = function
      | []          -> []
      | Some a :: b -> a :: filter_none b
      | None :: b   -> filter_none b
    in
    filter_none (List.map make_test bi_spec.bispec_pres)

  let run_test (ret_fun : result_t -> Spec.t * bool) (prog : UP.prog) (test : t)
      : (Spec.t * bool) list =
    let state = SBAState.copy test.state in
    try
      let specs : (Spec.t * bool) list =
        SBAInterpreter.evaluate_proc ret_fun prog test.name test.params state
      in
      specs
    with Failure msg ->
      L.print_to_all
        (Printf.sprintf "ERROR in proc %s with message:\n%s\n" test.name msg);
      []

  let test_procs
      ?(prev_results : BiAbductionResults.t option)
      (prog : UP.prog)
      (to_test : SS.t) : BiAbductionResults.t =
    L.verbose (fun m -> m "Starting bi-abductive testing");
    let proc_names = Prog.get_proc_names prog.prog in
    L.verbose (fun m -> m "Proc names: %s" (String.concat ", " proc_names));
    let bispecs = Prog.get_bispecs prog.prog in
    let () =
      List.iter
        (fun (bispec : BiSpec.t) ->
          L.verbose (fun m -> m "Found bi-spec for: %s" bispec.bispec_name))
        bispecs
    in
    let bispecs_to_test =
      List.filter
        (fun (bispec : BiSpec.t) -> SS.mem bispec.bispec_name to_test)
        bispecs
    in

    let tests = List.concat (List.map (testify prog) bispecs_to_test) in
    let test_names tests =
      String.concat ", " (List.map (fun t -> t.name) tests)
    in
    L.verbose (fun m -> m "I have tests for: %s" (test_names tests));
    let tests =
      List.sort (fun test1 test2 -> Stdlib.compare test1.name test2.name) tests
    in
    L.verbose (fun m -> m "I have tests for: %s" (test_names tests));

    let filter spec_name = not (SS.mem spec_name to_test) in
    let prev_specs =
      Option.fold ~none:[]
        ~some:(fun results -> BiAbductionResults.get_all_specs ~filter results)
        prev_results
    in
    let prev_spec_names =
      List.map (fun (spec : Spec.t) -> spec.spec_name) prev_specs
    in
    L.verbose (fun m ->
        m "I will use the previously-stored specs of: %s"
          (String.concat ", " prev_spec_names));
    List.iter (fun spec -> UP.add_spec prog spec) prev_specs;

    let process_spec name params state_pre state_post flag : Spec.t * Spec.t =
      make_spec prog name params state_pre state_post flag
    in

    let process_symb_exec_result
        (name : string)
        (params : string list)
        (state_i : bi_state_t)
        (result : result_t) : Spec.t * bool =
      let state_i = SBAState.copy state_i in
      match result with
      | RFail (_, _, state_f, errs) ->
          let sspec, spec =
            process_spec name params state_i state_f Flag.Error
          in
          if !Config.bug_specs_propagation then UP.add_spec prog spec;
          (sspec, false)
      | RSucc (fl, _, state_f) ->
          let sspec, spec = process_spec name params state_i state_f fl in
          let () =
            try UP.add_spec prog spec
            with _ ->
              Printf.printf "when trying to build an UP for %s, I died!\n" name
          in
          (sspec, true)
    in

    let bug_specs, succ_specs =
      List.split
        (List.map
           (fun test ->
             L.verbose (fun m ->
                 m "Running bi-abduction on function %s\n" test.name);
             let rets =
               run_test
                 (process_symb_exec_result test.name test.params test.state)
                 prog test
             in
             let succ_specs, bug_specs =
               List.partition (fun (_, b) -> b) rets
             in
             let succ_specs = List.map (fun (spec, _) -> spec) succ_specs in
             let bug_specs = List.map (fun (spec, _) -> spec) bug_specs in
             (bug_specs, succ_specs))
           tests)
    in

    let succ_specs : Spec.t list = List.concat succ_specs in
    let bug_specs : Spec.t list = List.concat bug_specs in

    let succ_specs, error_specs =
      List.partition
        (fun (spec : Spec.t) ->
          match spec.spec_sspecs with
          | [ sspec ] -> sspec.ss_flag = Flag.Normal
          | _         -> false)
        succ_specs
    in

    L.normal (fun m ->
        m "@[<v 2>BUG SPECS:@\n%a@]@\n"
          Fmt.(list ~sep:(any "@\n") (UP.pp_spec ~preds:prog.preds))
          bug_specs);

    L.normal (fun m ->
        m "@[<v 2>SUCCESSFUL SPECS:@\n%a@]@\n"
          Fmt.(list ~sep:(any "@\n") (UP.pp_spec ~preds:prog.preds))
          succ_specs);

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

  let test_prog
      (prog : UP.prog)
      (incremental : bool)
      (source_files : SourceFiles.t option) : unit =
    let open ResultsDir in
    let open ChangeTracker in
    if incremental && prev_results_exist () then
      (* Only test changed procedures *)
      let cur_source_files =
        match source_files with
        | Some files -> files
        | None       -> failwith "Cannot use -a in incremental mode"
      in
      let prev_source_files, prev_call_graph, results =
        read_biabduction_results ()
      in
      let proc_changes =
        get_changes prog.prog ~prev_source_files ~prev_call_graph
          ~cur_source_files
      in
      let procs_to_prune =
        proc_changes.changed_procs @ proc_changes.deleted_procs
        @ proc_changes.dependent_procs
      in
      let () = CallGraph.prune_procs prev_call_graph procs_to_prune in
      let () = BiAbductionResults.prune results procs_to_prune in
      let procs_to_test =
        SS.of_list
          ( proc_changes.changed_procs @ proc_changes.new_procs
          @ proc_changes.dependent_procs )
      in
      let cur_results = test_procs ~prev_results:results prog procs_to_test in
      let cur_call_graph = SBAInterpreter.call_graph in
      let call_graph = CallGraph.merge prev_call_graph cur_call_graph in
      let results = BiAbductionResults.merge results cur_results in
      let diff = Fmt.str "%a" ChangeTracker.pp_proc_changes proc_changes in
      write_biabduction_results cur_source_files call_graph ~diff results
    else
      (* Test all procedures *)
      let cur_source_files =
        Option.value ~default:(SourceFiles.make ()) source_files
      in
      let procs_to_test =
        SS.of_list (Prog.get_noninternal_proc_names prog.prog)
      in
      let results = test_procs prog procs_to_test in
      let call_graph = SBAInterpreter.call_graph in
      write_biabduction_results cur_source_files call_graph ~diff:"" results
end

module From_scratch (SMemory : SMemory.S) (External : External.S) = struct
  module INTERNAL__ = struct
    module SState = SState.Make (SMemory)
  end

  include Make
            (INTERNAL__.SState)
            (PState.Make (SVal.M) (SVal.SSubst) (SStore) (INTERNAL__.SState)
               (Preds.SPreds))
            (External)
end
