module type S = sig
  val cmd_name : string

  val exec_mode : ExecMode.t

  val run_all : string -> unit
end

module Make (Backend : functor (Outcome : Outcome.S) (Suite : Suite.S) ->
  Backend.S with type category = Suite.category)
(Outcome : Outcome.S)
(Suite : Suite.S)
(Expectations : Expectations.S
                  with type info = Suite.info
                   and type category = Suite.category
                   and type matcher = Backend(Outcome)(Suite).matcher
                   and type outcome = Outcome.t) : S = struct
  module Backend = Backend (Outcome) (Suite)

  let cmd_name = Suite.cmd_name

  let exec_mode = Suite.exec_mode

  module CInterpreter =
    GInterpreter.Make (Outcome.Val) (Outcome.Subst) (Outcome.Store)
      (Outcome.State)
      (Outcome.External)

  (** The test_table maps category to category tables.
      A category table maps file names to tests *)
  let test_table :
      ( Suite.category,
        (string, (Suite.info, Suite.category) Test.t) Hashtbl.t )
      Hashtbl.t =
    Hashtbl.create 1

  let add_test_to_table (test : (Suite.info, Suite.category) Test.t) =
    let cat_tbl_opt = Hashtbl.find_opt test_table test.category in
    let cat_tbl =
      match cat_tbl_opt with
      | Some c -> c
      | None   ->
          let new_cat_tbl = Hashtbl.create 1 in
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

  let convert_other_imports oi =
    List.map
      (fun (ext, f) ->
        let fun_with_exn s = Stdlib.Result.get_ok (f s) in
        (ext, fun_with_exn))
      oi

  let execute_test expect test =
    let open Outcome in
    let open ParserAndCompiler in
    let result = ref (Outcome.FailedExec "Execution failure") in
    let () =
      Backend.check_not_throw expect (fun () ->
          let () = Utils.Generators.reset () in
          let () = Utils.Allocators.reset_all () in
          let () = Suite.beforeTest test.Test.info test.path in
          let res =
            match parse_and_compile_files [ test.Test.path ] with
            | Error err -> ParseAndCompileError err
            | Ok progs  -> (
                let e_progs = progs.gil_progs in
                let () = Gil_parsing.cache_labelled_progs e_progs in
                let e_prog = List.hd (List.map snd e_progs) in
                let other_imports = convert_other_imports other_imports in
                let prog = Gil_parsing.eprog_to_prog ~other_imports e_prog in
                match UP.init_prog prog with
                | Error _ -> failwith "Failed to create unification plan"
                | Ok prog ->
                    let ret = CInterpreter.evaluate_prog prog in
                    FinishedExec ret )
          in
          result := res)
    in
    !result

  let register_expectations () =
    Hashtbl.iter
      (Backend.register_expectations_for_category
         ~expectation:Expectations.expectation ~test_runner:execute_test)
      test_table

  let run_all path =
    let list_files =
      List.filter Suite.filter_source (Utils.Io_utils.get_files path)
    in
    Fmt.pr "Registering tests...\n@?";
    let () = Suite.init_suite list_files in
    let () = register_tests list_files in
    let () = register_expectations () in
    Backend.run ()
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
