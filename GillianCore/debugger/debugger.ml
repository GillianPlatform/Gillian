module L = Logging
module Displayable = Displayable

module type S = sig
  type stop_reason = Step | ReachedStart | ReachedEnd | Breakpoint

  type frame = {
    index : int;
    name : string;
    source_path : string;
    start_line : int;
    start_column : int;
    end_line : int;
    end_column : int;
  }

  type scope = { name : string; id : int }

  type variable = { name : string; value : string; type_ : string option }

  type debugger_state

  val launch : string -> (debugger_state, string) result

  val step : ?reverse:bool -> debugger_state -> stop_reason

  val run : ?reverse:bool -> ?launch:bool -> debugger_state -> stop_reason

  val terminate : debugger_state -> unit

  val get_frames : debugger_state -> frame list

  val get_scopes : debugger_state -> scope list

  val get_variables : int -> debugger_state -> variable list

  val set_breakpoints : string option -> int list -> debugger_state -> unit
end

module Make
    (PC : ParserAndCompiler.S)
    (Verification : Verifier.S)
    (SMemory : SMemory.S)
    (Displayable : Displayable.S with type t = SMemory.t) =
struct
  module Breakpoints = Set.Make (Int)

  type stop_reason = Step | ReachedStart | ReachedEnd | Breakpoint

  type frame = {
    index : int;
    name : string;
    source_path : string;
    start_line : int;
    start_column : int;
    end_line : int;
    end_column : int;
  }

  type scope = { name : string; id : int }

  type variable = { name : string; value : string; type_ : string option }

  type breakpoints = (string, Breakpoints.t) Hashtbl.t

  type debugger_state = {
    source_file : string;
    source_files : SourceFiles.t option;
    scopes : scope list;
    prog : (Annot.t, int) Prog.t;
    mutable cont_func :
      unit -> Verification.result_t Verification.SAInterpreter.cont_func;
    mutable cur_report_id : string;
    mutable frames : frame list;
    mutable state : Verification.SAInterpreter.state_t option;
    mutable breakpoints : breakpoints;
  }

  let scopes_tbl = Hashtbl.create 0

  let store_scope = ({ name = "Store"; id = 1 } : scope)

  let heap_scope = ({ name = "Heap"; id = 2 } : scope)

  (* TODO: Find a common place to put the below three functions which are
     duplicated in CommandLine.ml *)
  let convert_other_imports oi =
    List.map
      (fun (ext, f) ->
        let fun_with_exn s = Stdlib.Result.get_ok (f s) in
        (ext, fun_with_exn))
      oi

  let get_progs_or_fail = function
    | Ok progs  -> (
        match progs.ParserAndCompiler.gil_progs with
        | [] ->
            Fmt.pr "Error: expected at least one GIL program\n";
            exit 1
        | _  -> progs)
    | Error err ->
        Fmt.pr "Error during compilation to GIL:\n%a" PC.pp_err err;
        exit 1

  let burn_gil prog outfile_opt =
    match outfile_opt with
    | Some outfile ->
        let outc = open_out outfile in
        let fmt = Format.formatter_of_out_channel outc in
        let () = Prog.pp_labeled fmt prog in
        close_out outc
    | None         -> ()

  let preprocess_files files already_compiled outfile_opt no_unfold =
    let e_prog, source_files_opt =
      if not already_compiled then
        let progs = get_progs_or_fail (PC.parse_and_compile_files files) in
        let e_progs = progs.gil_progs in
        let () = Gil_parsing.cache_labelled_progs (List.tl e_progs) in
        let e_prog = snd (List.hd e_progs) in
        let source_files = progs.source_files in
        (e_prog, Some source_files)
      else
        let e_prog = Gil_parsing.parse_eprog_from_file (List.hd files) in
        (e_prog, None)
    in
    let () = burn_gil e_prog outfile_opt in
    (* Prog.perform_syntax_checks e_prog; *)
    let prog =
      Gil_parsing.eprog_to_prog
        ~other_imports:(convert_other_imports PC.other_imports)
        e_prog
    in
    let () =
      L.verbose (fun m ->
          m "@\nProgram as parsed:@\n%a@\n" Prog.pp_indexed prog)
    in
    let prog = LogicPreprocessing.preprocess prog (not no_unfold) in
    let () =
      L.verbose (fun m ->
          m "@\nProgram after logic preprocessing:@\n%a@\n" Prog.pp_indexed prog)
    in
    (prog, source_files_opt)

  let has_hit_breakpoint dbg =
    match dbg.frames with
    | []         -> false
    | frame :: _ ->
        if Hashtbl.mem dbg.breakpoints frame.source_path then
          let breakpoints = Hashtbl.find dbg.breakpoints frame.source_path in
          (* Currently only one breakpoint per line is supported *)
          Breakpoints.mem frame.start_line breakpoints
        else false

  let rec call_stack_to_frames call_stack next_proc_body_idx prog =
    match call_stack with
    | [] -> []
    | (se : Verification.SAInterpreter.CallStack.stack_element) :: rest ->
        let defaults = (0, 0, 0, 0, "") in
        let proc = Prog.get_proc prog se.pid in
        let start_line, start_column, end_line, end_column, source_path =
          match proc with
          | None      -> defaults
          | Some proc -> (
              let annot, _, _ = proc.proc_body.(next_proc_body_idx) in
              let loc_opt = Annot.get_origin_loc annot in
              match loc_opt with
              | None     -> defaults
              | Some loc ->
                  ( loc.loc_start.pos_line,
                    (* VSCode column numbers start from 1 *)
                    loc.loc_start.pos_column + 1,
                    loc.loc_end.pos_line,
                    loc.loc_end.pos_column + 1,
                    loc.loc_source ))
        in
        let frame =
          {
            (* TODO: make this a guaranteed unique index*)
            index = se.call_index;
            name = se.pid;
            source_path;
            start_line;
            start_column;
            end_line;
            end_column;
          }
        in
        frame :: call_stack_to_frames rest se.call_index prog

  let update_report_id_and_inspection_fields report_id dbg =
    dbg.cur_report_id <- report_id;
    let open Verification.SAInterpreter in
    match Logging.LogQueryer.get_report report_id with
    | None                  ->
        raise
          (Failure
             (Printf.sprintf
                "Unable to find report id '%s'. Check the logging level is set \
                 correctly"
                report_id))
    | Some (content, type_) -> (
        match type_ with
        | t when t = Logging.LoggingConstants.ContentType.cmd_step -> (
            let cmd_step =
              content |> Yojson.Safe.from_string |> cmd_step_of_yojson
            in
            match cmd_step with
            | Ok cmd_step ->
                let () =
                  dbg.frames <-
                    call_stack_to_frames cmd_step.call_stack
                      cmd_step.proc_body_index dbg.prog
                in
                dbg.state <- cmd_step.state
            | Error err   -> raise (Failure err))
        | _ as t ->
            raise
              (Failure
                 (Printf.sprintf
                    "Cannot deserialize: type '%s' does not match callstack" t))
        )

  let launch file_name =
    Hashtbl.replace scopes_tbl store_scope.id store_scope.name;
    Hashtbl.replace scopes_tbl heap_scope.id heap_scope.name;

    let () = Fmt_tty.setup_std_outputs () in
    let () = Config.current_exec_mode := Verification in
    let () = PC.initialize Verification in
    let () = Config.stats := false in
    let () = Config.lemma_proof := true in
    let () = Config.manual_proof := false in
    let already_compiled = true in
    let outfile_opt = None in
    let no_unfold = false in
    (* TODO: Support debugging incremental mode *)
    (* let incremental = false in *)
    let prog, source_files_opt =
      preprocess_files [ file_name ] already_compiled outfile_opt no_unfold
    in
    let cont_func = Verification.verify_up_to_procs prog in
    match cont_func with
    | Verification.SAInterpreter.Finished _ -> Error "Nothing to run"
    | Verification.SAInterpreter.Continue (cur_report_id, cont_func) -> (
        match cur_report_id with
        | None               ->
            raise
              (Failure
                 "Did not log report. Check the logging level is set correctly")
        | Some cur_report_id ->
            let dbg =
              ({
                 source_file = file_name;
                 source_files = source_files_opt;
                 scopes = [ store_scope; heap_scope ];
                 cont_func;
                 prog;
                 frames = [];
                 state = None;
                 cur_report_id;
                 breakpoints = Hashtbl.create 0;
               }
                : debugger_state)
            in
            let () = update_report_id_and_inspection_fields cur_report_id dbg in
            Ok dbg)

  let execute_step dbg =
    let open Verification.SAInterpreter in
    let cont_func = dbg.cont_func () in
    match cont_func with
    | Finished _ -> ReachedEnd
    | Continue (cur_report_id, cont_func) -> (
        match cur_report_id with
        | None               ->
            raise
              (Failure
                 "Did not log report. Check the logging level is set correctly")
        | Some cur_report_id ->
            let () = dbg.cont_func <- cont_func in
            let () = update_report_id_and_inspection_fields cur_report_id dbg in
            if has_hit_breakpoint dbg then Breakpoint else Step)

  let step ?(reverse = false) dbg =
    if reverse then
      let prev_report_id =
        Logging.LogQueryer.get_previous_report_id dbg.cur_report_id
      in
      match prev_report_id with
      | None                -> ReachedStart
      | Some prev_report_id ->
          let () = update_report_id_and_inspection_fields prev_report_id dbg in
          if has_hit_breakpoint dbg then Breakpoint else Step
    else
      let next_report_id =
        Logging.LogQueryer.get_next_report_id dbg.cur_report_id
      in
      match next_report_id with
      | None                -> execute_step dbg
      | Some next_report_id ->
          let () = update_report_id_and_inspection_fields next_report_id dbg in
          if has_hit_breakpoint dbg then Breakpoint else Step

  let rec run ?(reverse = false) ?(launch = false) dbg =
    (* We need to check if a breakpoint has been hit if run is called
       immediately after launching to prevent missing a breakpoint on the first
       line *)
    if launch && has_hit_breakpoint dbg then Breakpoint
    else
      let stop_reason = step ~reverse dbg in
      match stop_reason with
      | Step              -> run ~reverse dbg
      | other_stop_reason -> other_stop_reason

  let terminate dbg =
    let () = Verification.postprocess_files dbg.source_files in
    let () = if !Config.stats then Statistics.print_statistics () in
    Logging.wrap_up ()

  let get_frames dbg = dbg.frames

  let get_scopes dbg = dbg.scopes

  let get_variables var_ref dbg =
    (* TODO: Display store and heap *)
    match Hashtbl.find_opt scopes_tbl var_ref with
    | None    -> []
    | Some id ->
        if id = "Store" then
          match dbg.state with
          | None       -> []
          | Some state ->
              let open Verification.SAInterpreter in
              let store = State.get_store state in
              Store.bindings store
              |> List.map (fun (var, value) ->
                     let value =
                       Fmt.to_to_string Verification.SAInterpreter.Val.full_pp
                         value
                     in
                     ({ name = var; value; type_ = None } : variable))
        else
          [
            ({ name = id ^ "_i"; value = "21354"; type_ = Some "integer" }
              : variable);
            ({ name = id ^ "_f"; value = "4.52"; type_ = Some "float" }
              : variable);
            ({ name = id ^ "_s"; value = "hello world"; type_ = Some "string" }
              : variable);
          ]

  let set_breakpoints source bp_list dbg =
    match source with
    (* We can't set the breakpoints if we do not know the source file *)
    | None -> ()
    | Some source ->
        let bp_set = Breakpoints.of_list bp_list in
        Hashtbl.replace dbg.breakpoints source bp_set
end
