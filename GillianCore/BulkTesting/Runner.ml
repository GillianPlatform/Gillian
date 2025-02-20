open Syntaxes.Result

module type S = sig
  val cmd_name : string
  val exec_mode : Exec_mode.t
  val run_all : test_suite_path:string -> incremental:bool -> unit
end

module Make
    (Backend : functor
      (Outcome : Outcome.S)
      (Suite : Suite.S)
      -> Backend.S with type category = Suite.category)
    (Outcome : Outcome.S)
    (Suite : Suite.S)
    (Expectations : Expectations.S
                      with type info = Suite.info
                       and type category = Suite.category
                       and type matcher = Backend(Outcome)(Suite).matcher
                       and type outcome = Outcome.t) : S = struct
  module Backend = Backend (Outcome) (Suite)
  module PC = Outcome.ParserAndCompiler

  module Interpreter =
    G_interpreter.Make (Outcome.Val) (Outcome.ESubst) (Outcome.Store)
      (Outcome.State)
      (PC)
      (Outcome.External)

  module Gil_parsing = Gil_parsing.Make (PC.Annot)

  let cmd_name = Suite.cmd_name
  let exec_mode = Suite.exec_mode

  type prev_results = {
    source_files : (string, SourceFiles.t) Hashtbl.t;
    call_graphs : (string, Call_graph.t) Hashtbl.t;
  }

  let cur_source_files = Hashtbl.create Config.small_tbl_size
  let cur_call_graphs = Hashtbl.create Config.small_tbl_size
  let tests_ran = ref ([] : string list)

  (** The [test_table] maps categories to category tables. A category table maps
      file paths to tests. *)
  let test_table :
      ( Suite.category,
        (string, (Suite.info, Suite.category) Test.t) Hashtbl.t )
      Hashtbl.t =
    Hashtbl.create Config.small_tbl_size

  let add_test_to_table (test : (Suite.info, Suite.category) Test.t) =
    let cat_tbl_opt = Hashtbl.find_opt test_table test.category in
    let cat_tbl =
      match cat_tbl_opt with
      | Some c -> c
      | None ->
          let new_cat_tbl = Hashtbl.create Config.small_tbl_size in
          let () = Hashtbl.replace test_table test.category new_cat_tbl in
          new_cat_tbl
    in
    Hashtbl.replace cat_tbl test.path test

  let register_one_test file_path =
    let code = Io_utils.load_file file_path in
    let tests =
      List.map
        (fun (name, info, category) ->
          Test.make ~name ~info ~category ~path:file_path)
        (Suite.create_tests file_path code)
    in
    List.iter add_test_to_table tests

  let register_tests file_list = List.iter register_one_test file_list

  let before_compilation test =
    Generators.reset ();
    Suite.beforeTest test.Test.info test.path

  let before_execution () =
    Allocators.reset_all ();
    Interpreter.reset_call_graph ()

  let should_execute prog filename prev_results_opt =
    match prev_results_opt with
    | Some { source_files; call_graphs } -> (
        let open Containers in
        let cur_source_files = Hashtbl.find cur_source_files filename in
        let prev_sources_opt = Hashtbl.find_opt source_files filename in
        let prev_graph_opt = Hashtbl.find_opt call_graphs filename in
        match (prev_sources_opt, prev_graph_opt) with
        | Some prev_source_files, Some prev_call_graph ->
            let proc_changes =
              ChangeTracker.get_sym_changes prog ~prev_source_files
                ~prev_call_graph ~cur_source_files
            in
            let changed_procs =
              SS.of_list
                (proc_changes.changed_procs @ proc_changes.new_procs
               @ proc_changes.dependent_procs)
            in
            if SS.mem !Config.entry_point changed_procs then true
            else (
              (* Keep previous call graph *)
              Hashtbl.add cur_call_graphs filename prev_call_graph;
              false)
        | _ -> true)
    | None -> true

  let execute_test
      prev_results_opt
      expect
      (test : (Suite.info, Suite.category) Test.t) =
    let open Outcome in
    let result = ref (Gillian_result.operation_error "Execution failure") in
    let execute () =
      let () = before_compilation test in
      let filename = Filename.basename (Filename.chop_extension test.path) in
      let res =
        let* progs = PC.parse_and_compile_files [ test.path ] in
        let e_progs = progs.gil_progs in
        let () = Hashtbl.add cur_source_files filename progs.source_files in
        let () = Gil_parsing.cache_labelled_progs (List.tl e_progs) in
        let e_prog = snd (List.hd e_progs) in
        let* prog =
          Gil_parsing.eprog_to_prog ~other_imports:PC.other_imports e_prog
        in
        if should_execute prog filename prev_results_opt then
          match MP.init_prog prog with
          | Error _ -> failwith "Failed to create matching plans"
          | Ok prog ->
              let () = before_execution () in
              let ret =
                Interpreter.evaluate_proc
                  (fun x -> x)
                  prog !Config.entry_point []
                  (State.init progs.init_data)
              in
              let call_graph = Interpreter.call_graph in
              let copy = Call_graph.merge (Call_graph.make ()) call_graph in
              let () = Hashtbl.add cur_call_graphs filename copy in
              let () = tests_ran := filename :: !tests_ran in
              Ok ret
        else Ok []
        (* TODO (Alexis): Persist and fetch actual execution summary *)
      in
      result := res
    in
    Backend.check_not_throw expect execute;
    !result

  let register_expectations prev_results_opt =
    Hashtbl.iter
      (Backend.register_expectations_for_category
         ~expectation:Expectations.expectation
         ~test_runner:(execute_test prev_results_opt))
      test_table

  let run_all ~test_suite_path ~incremental =
    let () = Fmt.pr "Registering tests...\n@?" in
    let is_wpst = Exec_mode.is_symbolic_exec exec_mode in
    let list_files =
      List.filter Suite.filter_source (Utils.Io_utils.get_files test_suite_path)
    in
    let () = Suite.init_suite list_files in
    let () = register_tests list_files in
    let prev_results_opt =
      if is_wpst && incremental && ResultsDir.prev_results_exist () then
        let source_files, call_graphs =
          ResultsDir.read_bulk_symbolic_results ()
        in
        Some { source_files; call_graphs }
      else None
    in
    let () = register_expectations prev_results_opt in
    let () = Backend.run () in
    if is_wpst then
      ResultsDir.write_bulk_symbolic_results ~tests_ran:!tests_ran
        cur_source_files cur_call_graphs
end

type t = (module S)

let cmd_name (r : t) =
  let (module R) = r in
  R.cmd_name

let run_all (r : t) =
  let (module R) = r in
  R.run_all

let exec_mode (r : t) =
  let (module R) = r in
  R.exec_mode

module DummyRunners = struct
  let runners = []
end
