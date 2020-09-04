open Gillian.Gil_syntax
module UP = Gillian.Abstraction.UP
module KB = UP.KB

let kb_pp = Fmt.(braces (iter ~sep:comma KB.iter Expr.pp))

let expr_pair_pp = Fmt.(parens (pair ~sep:comma Expr.pp Expr.pp))

let expr = Gillian.Gil_parsing.parse_expr_from_string

let learn_expr_testable = Alcotest.testable expr_pair_pp ( = )

let ins_outs_expr_testable =
  Alcotest.testable
    Fmt.(pair ~sep:comma kb_pp (list expr_pair_pp))
    (fun (kb1, el1) (kb2, el2) -> KB.equal kb1 kb2 && el1 = el2)

let up_learn_expr_simple_plus () =
  let e = expr "x + y" in
  let kb = KB.of_list [ e; expr "x" ] in
  let expected_result = [ (expr "y", expr "(x + y) - x") ] in
  let obtained_result = UP.learn_expr kb e in
  Alcotest.(check (list learn_expr_testable))
    "Simple plus" expected_result obtained_result

let up_learn_expr_complex_plus () =
  let e = Gillian.Gil_parsing.parse_expr_from_string "x + (l-len y) + z + t" in
  let kb = KB.of_list [ e; expr "x"; expr "z"; expr "t" ] in
  let expected_result =
    [ (expr "l-len y", expr "(x + (l-len y) + z + t) - t - z - x") ]
  in
  let obtained_result = UP.learn_expr kb e in
  Alcotest.(check (list learn_expr_testable))
    "More complex plus" expected_result obtained_result

let up_ins_outs_expr () =
  let e = Gillian.Gil_parsing.parse_expr_from_string "x + y + z + t" in
  let kb = KB.of_list [ e; expr "x"; expr "z"; expr "t" ] in
  let expected_result = [] in
  let obtained_result = UP.ins_outs_expr kb e in
  Alcotest.(check (list ins_outs_expr_testable))
    "More complex plus" expected_result obtained_result

let tests =
  [
    ("UP.learn_expr: simple plus", `Quick, up_learn_expr_simple_plus);
    ("UP.learn_expr: more complex plus", `Quick, up_learn_expr_complex_plus);
    ("UP.ins_outs_expr: simple plus", `Quick, up_ins_outs_expr);
  ]
