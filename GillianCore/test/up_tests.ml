open Gillian.Gil_syntax
module UP = Gillian.Abstraction.UP
module KB = UP.KB

let kb =
  Alcotest.testable
    Fmt.(braces (iter ~sep:comma KB.iter Expr.pp))
    Expr.Set.equal

let test_pvar_known () =
  let test_kb = KB.of_list [ PVar "x"; LVar "y"; LVar "z" ] in
  let e : Expr.t = PVar "x" in
  let expected_result = [ KB.empty ] in
  let obtained_result = UP.missing test_kb e in
  Alcotest.(check (list kb))
    "Program variable known" expected_result obtained_result

let tests = [ ("pvar_known", `Quick, test_pvar_known) ]
