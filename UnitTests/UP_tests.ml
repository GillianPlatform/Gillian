open OUnit
open Gillian.Gil_syntax
module UP = Gillian.Abstraction.UP
module KB = UP.KB

let test_pvar_known _ =
  let open Expr in
  let kb = KB.of_list [ PVar "x"; LVar "y"; LVar "z" ] in
  let e : Expr.t = PVar "x" in
  let expected_result = [ KB.empty ] in
  let obtained_result = UP.missing kb e in
  assert_equal expected_result obtained_result

let up_missing_suite =
  "Testing UP.missing" >::: [ "Program variable known" >:: test_pvar_known ]

let decorator test test_ctx = test test_ctx

let _ = run_test_tt_main (test_decorate decorator up_missing_suite)
