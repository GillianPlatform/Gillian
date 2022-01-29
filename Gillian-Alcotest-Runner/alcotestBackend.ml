module Make (Outcome : Outcome.S) (Suite : Suite.S) = struct
  type category = Suite.category
  type matcher = AlcotestCheckers.Make(Outcome).matcher

  let sanitize_string s =
    let slash = Str.regexp_string "/" in
    let ws = Str.regexp {| +|} in
    let dot = Str.regexp_string "." in
    let no_slash = Str.global_replace slash "_" s in
    let no_ws = Str.global_replace ws "_" no_slash in
    Str.global_replace dot "_" no_ws

  module TestFramework = AlcotestFramework.Make (Outcome)

  (* Alcotest assumes that the code isn't supposed to raise errors *)
  let check_not_throw _ f = f ()
  let test_table = Hashtbl.create 1

  let expectation_for_one_test expectation test_executor name test =
    let open Alcotest in
    test_case name `Quick (fun () ->
        Suite.beforeEach ();
        let result = test_executor TestFramework.custom_checkers test in
        expectation TestFramework.custom_checkers test result)

  let register_expectations_for_category ~expectation ~test_runner cat cat_tbl =
    if Suite.skip_category cat then ()
      (* I haven't thought about how to show properly what has been skipped *)
    else
      let lst_test =
        Hashtbl.fold
          (fun name test lst ->
            expectation_for_one_test expectation test_runner
              (sanitize_string name) test
            :: lst)
          cat_tbl []
      in
      Hashtbl.replace test_table cat lst_test

  module AlcotestCore = Alcotest.Core.Make (Alcotest.Monad.Identity)

  let run () =
    let lst =
      Hashtbl.fold
        (fun cat lstcat lstall ->
          (sanitize_string ((Fmt.to_to_string Suite.pp_category) cat), lstcat)
          :: lstall)
        test_table []
    in
    Fmt_tty.setup_std_outputs ();
    let n = List.length lst in
    AlcotestCore.run ~and_exit:true
      (Printf.sprintf "Running %i test suites" n)
      lst
end
