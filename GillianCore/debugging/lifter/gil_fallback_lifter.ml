include Gil_fallback_lifter_intf
open Lifter
open Effect.Deep

module Make : Make =
functor
  (SMemory : SMemory.S)
  (PC : ParserAndCompiler.S)
  (TLLifter : functor
     (Gil : Gil_lifter_with_state with type Lifter.memory = SMemory.t)
     (V : Verifier.S with type annot = PC.Annot.t) ->
     S
       with type memory = SMemory.t
        and type tl_ast = PC.tl_ast
        and type memory_error = SMemory.err_t
        and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
        and type annot = PC.Annot.t
        and type init_data = PC.init_data
        and type pc_err = PC.err)
  (Verifier : Verifier.S with type annot = PC.Annot.t)
  ->
  struct
    let gil_state = ref None

    module Gil_lifter = Gil_lifter.Make (SMemory) (PC) (Verifier)

    module Gil_lifter_with_state = struct
      module Lifter = Gil_lifter

      let get_state () = !gil_state |> Option.get
    end

    module TLLifter = TLLifter (Gil_lifter_with_state) (Verifier)

    type memory = SMemory.t
    type tl_ast = PC.tl_ast
    type memory_error = SMemory.err_t
    type cmd_report = Verifier.SAInterpreter.Logging.ConfigReport.t
    type annot = PC.Annot.t
    type init_data = PC.init_data
    type pc_err = PC.err

    type t = {
      gil : Gil_lifter.t; [@to_yojson Gil_lifter.dump]
      tl : TLLifter.t option; [@to_yojson opt_to_yojson TLLifter.dump]
      mutable finish_gil_init : (cmd_report executed_cmd_data -> unit) option;
          [@to_yojson fun _ -> `Null]
    }
    [@@deriving to_yojson]

    type _ Effect.t +=
      | Step :
          (Logging.Report_id.t option * Branch_case.t option * Branch_case.path)
          -> cmd_report executed_cmd_data Effect.t

    module Defer = struct
      let delegate_to_gil (id, case, exec_data, state) =
        match state.finish_gil_init with
        | Some finish_gil ->
            let () = state.finish_gil_init <- None in
            finish_gil exec_data
        | None -> Gil_lifter.handle_cmd (Option.get id) case exec_data state.gil

      let ignore_node_updated f x =
        try f x with effect Node_updated _, k -> continue k ()

      let intercept_effect f state () =
        try f () with
        | effect TLLifter.Step ((id, case, _) as s), k ->
            let exec_data = Effect.perform (Step s) in
            let () =
              ignore_node_updated delegate_to_gil (id, case, exec_data, state)
            in
            continue k exec_data
        | effect Gil_lifter.Step s, k ->
            let exec_data = Effect.perform (Step s) in
            continue k exec_data

      let defer tl_f gil_f state =
        let f =
          match state.tl with
          | Some tl -> fun () -> tl_f tl
          | None -> fun () -> gil_f state.gil
        in
        intercept_effect f state ()

      let defer_with_id tl_f gil_f state id =
        defer (fun s -> tl_f s id) (fun s -> gil_f s id) state
    end

    open Defer

    let dump = to_yojson

    let get_matches_at_id id { gil; tl; _ } =
      match tl with
      | Some tl -> tl |> TLLifter.get_matches_at_id id
      | None -> gil |> Gil_lifter.get_matches_at_id id

    let memory_error_to_exception_info = TLLifter.memory_error_to_exception_info

    let get_variables state astate id =
      defer
        (fun s -> TLLifter.get_variables s astate id)
        (fun s -> Gil_lifter.get_variables s astate id)
        state

    let step_over = defer_with_id TLLifter.step_over Gil_lifter.step_over
    let step_in = defer_with_id TLLifter.step_in Gil_lifter.step_in
    let step_out = defer_with_id TLLifter.step_out Gil_lifter.step_out
    let step_back = defer_with_id TLLifter.step_back Gil_lifter.step_back

    let step_branch state id case =
      defer
        (fun s -> TLLifter.step_branch s id case)
        (fun s -> Gil_lifter.step_branch s id case)
        state

    let continue = defer_with_id TLLifter.continue Gil_lifter.continue

    let continue_back =
      defer_with_id TLLifter.continue_back Gil_lifter.continue_back

    let init_exn ~proc_name ~all_procs tl_ast prog =
      let gil, finish_gil_init = Gil_lifter.init_manual proc_name all_procs in
      let () = gil_state := Some gil in
      let state, finish_init =
        match TLLifter.init ~proc_name ~all_procs tl_ast prog with
        | None ->
            let finish_gil_init () = finish_gil_init None () in
            ({ gil; tl = None; finish_gil_init = None }, finish_gil_init)
        | Some (tl, finish_tl_init) ->
            let finish_gil_init exec_data =
              finish_gil_init (Some exec_data) () |> ignore
            in
            ( { gil; tl = Some tl; finish_gil_init = Some finish_gil_init },
              finish_tl_init )
      in
      let finish_init = intercept_effect finish_init state in
      (state, finish_init)

    let init ~proc_name ~all_procs tl_ast prog =
      Some (init_exn ~proc_name ~all_procs tl_ast prog)

    let parse_and_compile_files = TLLifter.parse_and_compile_files
    let pp_expr = defer TLLifter.pp_expr Gil_lifter.pp_expr
    let pp_asrt = defer TLLifter.pp_asrt Gil_lifter.pp_asrt
  end
