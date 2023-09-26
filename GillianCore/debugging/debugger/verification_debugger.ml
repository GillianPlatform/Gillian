open Syntaxes.Option
module L = Logging
module DL = Debugger_log

module Make
    (ID : Init_data.S)
    (PC : ParserAndCompiler.S with type init_data = ID.t)
    (Verification : Verifier.S
                      with type SPState.init_data = ID.t
                       and type annot = PC.Annot.t)
    (Lifter : Debugger_lifter.S
                with type memory = Verification.SAInterpreter.heap_t
                 and type memory_error = Verification.SPState.m_err_t
                 and type tl_ast = PC.tl_ast
                 and type cmd_report =
                  Verification.SAInterpreter.Logging.ConfigReport.t
                 and type annot = PC.Annot.t) =
struct
  open Base_debugger.Premake (ID) (PC) (Verification) (Lifter)

  module Impl : Debugger_impl = struct
    type proc_state_ext = unit

    type debug_state_ext = {
      mutable unify_maps : (L.Report_id.t * Unify_map.t) list;
      tests : (string * Verification.t) list;
    }

    let preprocess_prog ~no_unfold prog =
      LogicPreprocessing.preprocess prog (not no_unfold)

    let init (debug_state : unit base_debug_state) =
      let { init_data; prog; main_proc_name; _ } = debug_state in
      Config.Verification.set_procs_to_verify [ main_proc_name ];
      let tests = Verification.Debug.get_tests_for_prog ~init_data prog in
      { tests; unify_maps = [] }

    let init_proc _ _ = ()

    let launch_proc ~proc_name (debug_state : debug_state_ext base_debug_state)
        =
      Config.Verification.(
        let procs_to_verify = !procs_to_verify in
        if not (procs_to_verify |> List.mem proc_name) then
          set_procs_to_verify (procs_to_verify @ [ proc_name ]));
      Verification.verify_up_to_procs ~init_data:debug_state.init_data
        ~proc_name debug_state.prog

    module Unify = struct
      open Verification.SUnifier.Logging

      type unify_result = Unify_map.unify_result = Success | Failure

      module Build_map = Unify_map.Make_builder (Verification)

      let build_unify_map = Build_map.f

      let get_test debug_state proc_name =
        let tests = debug_state.ext.tests in
        match tests |> List.assoc_opt proc_name with
        | None ->
            DL.failwith
              (fun () ->
                let tests_json = Verification.proc_tests_to_yojson tests in
                [ ("tests", tests_json) ])
              (Fmt.str "No test found for proc `%s`!" proc_name)
        | Some test -> test

      let is_unify_successful id =
        L.Log_queryer.get_unify_results id
        |> List.exists (fun (_, content) ->
               let result =
                 content |> Yojson.Safe.from_string
                 |> UnifyResultReport.of_yojson |> Result.get_ok
               in
               match result with
               | Success _ -> true
               | Failure _ -> false)

      let do_unify prev_id test result =
        DL.log (fun m -> m "Unifying result for %a" L.Report_id.pp prev_id);
        let success = Verification.Debug.analyse_result test prev_id result in
        let+ id, content = L.Log_queryer.get_unify_for prev_id in
        (id, content, success)

      let f result proc_name prev_id debug_state =
        let test = get_test debug_state proc_name in
        let+ id, content, success =
          match L.Log_queryer.get_unify_for prev_id with
          | Some (id, content) -> Some (id, content, is_unify_successful id)
          | None -> do_unify prev_id test result
        in
        let unify_report = content |> of_yojson_string UnifyReport.of_yojson in
        let kind = unify_report.unify_kind in
        let result = if success then Success else Failure in
        (id, kind, result)

      let get_unifys
          cur_report_id
          (debug_state : debug_state_ext base_debug_state)
          _ =
        let { ext; _ } = debug_state in
        let unify =
          let+ unify_id, _ = L.Log_queryer.get_unify_for cur_report_id in
          let unify_map =
            match ext.unify_maps |> List.assoc_opt unify_id with
            | Some map -> map
            | None ->
                let map = build_unify_map unify_id in
                ext.unify_maps <- (unify_id, map) :: ext.unify_maps;
                map
          in
          let result = unify_map |> Unify_map.result in
          (unify_id, fst unify_map, result)
        in
        match unify with
        | None -> []
        | Some (id, kind, result) -> [ Exec_map.{ id; kind; result } ]

      let unify_final_cmd prev_id ~proc_name result debug_state =
        match f result proc_name prev_id debug_state with
        | None -> []
        | Some (id, kind, result) -> [ Exec_map.{ id; kind; result } ]

      let get_unify_map unify_id debug_state =
        let ext = debug_state.ext in
        match ext.unify_maps |> List.assoc_opt unify_id with
        | Some map -> map
        | None ->
            let map = build_unify_map unify_id in
            ext.unify_maps <- (unify_id, map) :: ext.unify_maps;
            map
    end
  end

  include Make (Impl)
end
