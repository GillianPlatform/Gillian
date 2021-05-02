open Alcotest
open JS_Parser.Syntax
open JS_Parser

let on_first_line n =
  let pos = Loc.{ line = 0; column = n } in
  Loc.{ start = pos; _end = pos; source = None }

let mk_exp s o = JS_Parser.Syntax.mk_exp s o []

let mk_exp_with_annot = JS_Parser.Syntax.mk_exp

(* Equality testing function for expressions, ignoring the character offsets *)
let rec exp_stx_eq e1 e2 =
  match (e1.exp_stx, e2.exp_stx) with
  | Label (s, e), Label (s', e') | Access (e, s), Access (e', s') ->
      s = s' && exp_stx_eq e e'
  | Unary_op (op, e), Unary_op (op', e') -> op = op' && exp_stx_eq e e'
  | Delete e, Delete e' | Throw e, Throw e' -> exp_stx_eq e e'
  | While (e1, e2), While (e1', e2')
  | Comma (e1, e2), Comma (e1', e2')
  | Assign (e1, e2), Assign (e1', e2')
  | DoWhile (e1, e2), DoWhile (e1', e2')
  | CAccess (e1, e2), CAccess (e1', e2')
  | With (e1, e2), With (e1', e2') -> exp_stx_eq e1 e1' && exp_stx_eq e2 e2'
  | BinOp (e1, op, e2), BinOp (e1', op', e2') ->
      op = op' && exp_stx_eq e1 e1' && exp_stx_eq e2 e2'
  | AssignOp (e1, op, e2), AssignOp (e1', op', e2') ->
      op = op' && exp_stx_eq e1 e1' && exp_stx_eq e2 e2'
  | ForIn (e1, e2, e3), ForIn (e1', e2', e3')
  | ConditionalOp (e1, e2, e3), ConditionalOp (e1', e2', e3') ->
      exp_stx_eq e1 e1' && exp_stx_eq e3 e3' && exp_stx_eq e3 e3'
  | If (e1, e2, o), If (e1', e2', o') ->
      exp_stx_eq e1 e1' && exp_stx_eq e2 e2' && opt_exp_eq o o'
  | VarDec l, VarDec l' ->
      List.for_all2 (fun (s, o) (s', o') -> s = s' && opt_exp_eq o o') l l'
  | Call (e, l), Call (e', l') | New (e, l), New (e', l') ->
      exp_stx_eq e e' && List.for_all2 exp_stx_eq l l'
  | FunctionExp (b, o, l, e), FunctionExp (b', o', l', e')
  | Function (b, o, l, e), Function (b', o', l', e') ->
      b = b' && o = o' && l = l' && exp_stx_eq e e'
  | Obj l, Obj l' ->
      List.for_all2
        (fun (pn, pt, e) (pn', pt', e') ->
          pn = pn' && pt = pt' && exp_stx_eq e e')
        l l'
  | Block l, Block l' -> list_exp_eq l l'
  | Array l, Array l' -> List.for_all2 opt_exp_eq l l'
  | Return o, Return o' -> opt_exp_eq o o'
  | Script (b, l), Script (b', l') -> b = b' && list_exp_eq l l'
  | For (o1, o2, o3, e), For (o1', o2', o3', e') ->
      opt_exp_eq o1 o1' && opt_exp_eq o2 o2' && opt_exp_eq o3 o3'
      && exp_stx_eq e e'
  | Try (e, o, oe), Try (e', o', oe') ->
      exp_stx_eq e e'
      && (match (o, o') with
         | None, None                 -> true
         | Some (s, e), Some (s', e') -> s = s' && exp_stx_eq e e'
         | _, _                       -> false)
      && opt_exp_eq oe oe'
  | Switch (e, l), Switch (e', l') ->
      exp_stx_eq e e'
      && List.for_all2
           (fun (c, e) (c', e') -> switch_case_eq c c' && exp_stx_eq e e')
           l l'
  | s1, s2 -> s1 = s2

and switch_case_eq c c' =
  match (c, c') with
  | Case e, Case e' -> exp_stx_eq e e'
  | c, c'           -> c = c'

and opt_exp_eq o o' =
  match (o, o') with
  | None, None      -> true
  | Some o, Some o' -> exp_stx_eq o o'
  | _, _            -> false

and list_exp_eq l l' = List.for_all2 exp_stx_eq l l'


let assert_exp_eq =
  let t = testable (Fmt.of_to_string (PrettyPrint.string_of_exp true)) exp_stx_eq in
  Alcotest.(check t) "same expression"

let add_script e = mk_exp (Script (false, [ e ])) Loc.none

let rm_node e =
  match e.exp_stx with
  | Script (_, [ e ]) | Label (_, e) -> e
  | _ -> Alcotest.fail "Could not find a matching Script or Label tag."

(** * Tests begin here * **)

let test_var test_ctx =
  let exp = parse_string_exn "var x" in
  assert_exp_eq (add_script (mk_exp (VarDec [ ("x", None) ]) Loc.none)) exp

let test_var_value test_ctx =
  let exp = parse_string_exn "var x = 5" in
  let num_5 = mk_exp (Num 5.0) (on_first_line 8) in
  assert_exp_eq
    (add_script (mk_exp (VarDec [ ("x", Some num_5) ]) Loc.none))
    exp

let test_var_list test_ctx =
  let exp = parse_string_exn "var x = 5, y = null" in
  let num_5 = mk_exp (Num 5.0) (on_first_line 8) in
  let nul = mk_exp Null (on_first_line 15) in
  let vardec =
    mk_exp (VarDec [ ("x", Some num_5); ("y", Some nul) ]) Loc.none
  in
  assert_exp_eq (add_script vardec) exp

let test_regexp test_ctx =
  let exp = parse_string_exn "/^\\s+/" in
  assert_exp_eq (add_script (mk_exp (RegExp ("^\\s+", "")) Loc.none)) exp

let test_regexp_with_flags test_ctx =
  let exp = parse_string_exn "/^\\s+/g" in
  assert_exp_eq (add_script (mk_exp (RegExp ("^\\s+", "g")) Loc.none)) exp

let test_not test_ctx =
  let exp = parse_string_exn "!selector" in
  let selector = mk_exp (Var "selector") (on_first_line 1) in
  assert_exp_eq (add_script (mk_exp (Unary_op (Not, selector)) Loc.none)) exp

let test_caccess test_ctx =
  let exp = parse_string_exn "this[0]" in
  let this = mk_exp This Loc.none in
  let zero = mk_exp (Num 0.0) (on_first_line 5) in
  assert_exp_eq (add_script (mk_exp (CAccess (this, zero)) Loc.none)) exp

let test_and test_ctx =
  let exp = parse_string_exn "a && b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (a, Boolean And, b)) (on_first_line 0)))
    exp

let test_array_literal test_ctx =
  let exp = parse_string_exn "[,x,,y]" in
  let x = mk_exp (Var "x") (on_first_line 2) in
  let y = mk_exp (Var "y") (on_first_line 5) in
  assert_exp_eq
    (add_script
       (mk_exp (Array [ None; Some x; None; Some y ]) (on_first_line 0)))
    exp

let test_ge test_ctx =
  let exp = parse_string_exn "1 >= 2" in
  let one = mk_exp (Num 1.0) (on_first_line 0) in
  let two = mk_exp (Num 2.0) (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (one, Comparison Ge, two)) (on_first_line 0)))
    exp

let test_or test_ctx =
  let exp = parse_string_exn "a || b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (a, Boolean Or, b)) (on_first_line 0)))
    exp

let test_not_triple_eq test_ctx =
  let exp = parse_string_exn "a !== b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 6) in
  assert_exp_eq
    (add_script
       (mk_exp (BinOp (a, Comparison NotTripleEqual, b)) (on_first_line 0)))
    exp

let test_hook test_ctx =
  let exp = parse_string_exn "a >= b ? a : b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  let ab = mk_exp (BinOp (a, Comparison Ge, b)) (on_first_line 0) in
  let a9 = mk_exp (Var "a") (on_first_line 9) in
  let b13 = mk_exp (Var "b") (on_first_line 13) in
  assert_exp_eq
    (add_script (mk_exp (ConditionalOp (ab, a9, b13)) (on_first_line 0)))
    exp

let test_instanceof test_ctx =
  let exp = parse_string_exn "a instanceof b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 13) in
  assert_exp_eq
    (add_script
       (mk_exp (BinOp (a, Comparison InstanceOf, b)) (on_first_line 0)))
    exp

let test_typeof test_ctx =
  let exp = parse_string_exn "typeof selector" in
  let selector = mk_exp (Var "selector") (on_first_line 7) in
  assert_exp_eq
    (add_script (mk_exp (Unary_op (TypeOf, selector)) (on_first_line 0)))
    exp

let test_pos test_ctx =
  let exp = parse_string_exn "+(a + 1)" in
  let a = mk_exp (Var "a") (on_first_line 2) in
  let one = mk_exp (Num 1.0) (on_first_line 6) in
  let a1 = mk_exp (BinOp (a, Arith Plus, one)) (on_first_line 2) in
  assert_exp_eq
    (add_script (mk_exp (Unary_op (Positive, a1)) (on_first_line 0)))
    exp

let test_dec_pre test_ctx =
  let exp = parse_string_exn "--a" in
  let a = mk_exp (Var "a") (on_first_line 2) in
  assert_exp_eq
    (add_script (mk_exp (Unary_op (Pre_Decr, a)) (on_first_line 0)))
    exp

let test_dec_post test_ctx =
  let exp = parse_string_exn "a--" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  assert_exp_eq
    (add_script (mk_exp (Unary_op (Post_Decr, a)) (on_first_line 0)))
    exp

let test_inc_pre test_ctx =
  let exp = parse_string_exn "++a" in
  let a = mk_exp (Var "a") (on_first_line 2) in
  assert_exp_eq
    (add_script (mk_exp (Unary_op (Pre_Incr, a)) (on_first_line 0)))
    exp

let test_inc_post test_ctx =
  let exp = parse_string_exn "a++" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  assert_exp_eq
    (add_script (mk_exp (Unary_op (Post_Incr, a)) (on_first_line 0)))
    exp

let test_for test_ctx =
  let exp = parse_string_exn "for (; a < 5; a++ ) { x = 1 }" in
  let empty = None in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let five = mk_exp (Num 5.0) (on_first_line 0) in
  let condition =
    Some (mk_exp (BinOp (a, Comparison Lt, five)) (on_first_line 0))
  in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let inc = Some (mk_exp (Unary_op (Post_Incr, a)) (on_first_line 0)) in
  let one = mk_exp (Num 1.0) (on_first_line 0) in
  let x = mk_exp (Var "x") (on_first_line 0) in
  let assignment = mk_exp (Assign (x, one)) (on_first_line 0) in
  let block = mk_exp (Block [ assignment ]) (on_first_line 0) in
  let loop = mk_exp (For (empty, condition, inc, block)) (on_first_line 0) in
  assert_exp_eq (add_script loop) exp

let test_forin test_ctx =
  let exp =
    parse_string_exn "for (var prop in oldObj) { obj[prop] = oldObj[prop] }"
  in
  let varprop = mk_exp (VarDec [ ("prop", None) ]) (on_first_line 5) in
  let oldObj1 = mk_exp (Var "oldObj") (on_first_line 17) in
  let obj = mk_exp (Var "obj") (on_first_line 27) in
  let prop1 = mk_exp (Var "prop") (on_first_line 31) in
  let ca1 = mk_exp (CAccess (obj, prop1)) (on_first_line 27) in
  let oldObj2 = mk_exp (Var "oldObj") (on_first_line 39) in
  let prop2 = mk_exp (Var "prop") (on_first_line 46) in
  let ca2 = mk_exp (CAccess (oldObj2, prop2)) (on_first_line 39) in
  let assignment = mk_exp (Assign (ca1, ca2)) (on_first_line 27) in
  let block = mk_exp (Block [ assignment ]) (on_first_line 25) in
  assert_exp_eq
    (add_script (mk_exp (ForIn (varprop, oldObj1, block)) (on_first_line 0)))
    exp

let test_assign_add test_ctx =
  let exp = parse_string_exn "a += b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (AssignOp (a, Plus, b)) (on_first_line 0)))
    exp

let test_assign_sub test_ctx =
  let exp = parse_string_exn "a -= b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (AssignOp (a, Minus, b)) (on_first_line 0)))
    exp

let test_assign_mul test_ctx =
  let exp = parse_string_exn "a *= b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (AssignOp (a, Times, b)) (on_first_line 0)))
    exp

let test_assign_div test_ctx =
  let exp = parse_string_exn "a /= b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (AssignOp (a, Div, b)) (on_first_line 0)))
    exp

let test_assign_mod test_ctx =
  let exp = parse_string_exn "a %= b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (AssignOp (a, Mod, b)) (on_first_line 0)))
    exp

let test_assign_ursh test_ctx =
  let exp = parse_string_exn "a >>>= b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 7) in
  assert_exp_eq
    (add_script (mk_exp (AssignOp (a, Ursh, b)) (on_first_line 0)))
    exp

let test_assign_lsh test_ctx =
  let exp = parse_string_exn "a <<= b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 6) in
  assert_exp_eq
    (add_script (mk_exp (AssignOp (a, Lsh, b)) (on_first_line 0)))
    exp

let test_assign_rsh test_ctx =
  let exp = parse_string_exn "a >>= b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 6) in
  assert_exp_eq
    (add_script (mk_exp (AssignOp (a, Rsh, b)) (on_first_line 0)))
    exp

let test_assign_bitand test_ctx =
  let exp = parse_string_exn "a &= b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (AssignOp (a, Bitand, b)) (on_first_line 0)))
    exp

let test_assign_bitor test_ctx =
  let exp = parse_string_exn "a |= b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (AssignOp (a, Bitor, b)) (on_first_line 0)))
    exp

let test_assign_bitxor test_ctx =
  let exp = parse_string_exn "a ^= b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (AssignOp (a, Bitxor, b)) (on_first_line 0)))
    exp

let test_notequal test_ctx =
  let exp = parse_string_exn "a != b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (a, Comparison NotEqual, b)) (on_first_line 0)))
    exp

let test_gt test_ctx =
  let exp = parse_string_exn "a > b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 4) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (a, Comparison Gt, b)) (on_first_line 0)))
    exp

let test_in test_ctx =
  let exp = parse_string_exn "a in b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (a, Comparison In, b)) (on_first_line 0)))
    exp

let test_comma1 test_ctx =
  let exp = parse_string_exn "a , b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 4) in
  assert_exp_eq (add_script (mk_exp (Comma (a, b)) (on_first_line 0))) exp

let test_comma2 test_ctx =
  let exp = parse_string_exn "a, b, c" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 3) in
  let c = mk_exp (Var "c") (on_first_line 6) in
  let ab = mk_exp (Comma (a, b)) (on_first_line 0) in
  assert_exp_eq (add_script (mk_exp (Comma (ab, c)) (on_first_line 0))) exp

let test_negative test_ctx =
  let exp = parse_string_exn "-a" in
  let a = mk_exp (Var "a") (on_first_line 1) in
  assert_exp_eq
    (add_script (mk_exp (Unary_op (Negative, a)) (on_first_line 0)))
    exp

let test_bitnot test_ctx =
  let exp = parse_string_exn "~a" in
  let a = mk_exp (Var "a") (on_first_line 1) in
  assert_exp_eq
    (add_script (mk_exp (Unary_op (Bitnot, a)) (on_first_line 0)))
    exp

let test_void test_ctx =
  let exp = parse_string_exn "void a" in
  let a = mk_exp (Var "a") (on_first_line 5) in
  assert_exp_eq (add_script (mk_exp (Unary_op (Void, a)) (on_first_line 0))) exp

let test_mod test_ctx =
  let exp = parse_string_exn "a % b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 4) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (a, Arith Mod, b)) (on_first_line 0)))
    exp

let test_ursh test_ctx =
  let exp = parse_string_exn "a >>> b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 6) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (a, Arith Ursh, b)) (on_first_line 0)))
    exp

let test_lsh test_ctx =
  let exp = parse_string_exn "a << b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (a, Arith Lsh, b)) (on_first_line 0)))
    exp

let test_rsh test_ctx =
  let exp = parse_string_exn "a >> b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 5) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (a, Arith Rsh, b)) (on_first_line 0)))
    exp

let test_bitand test_ctx =
  let exp = parse_string_exn "a & b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 4) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (a, Arith Bitand, b)) (on_first_line 0)))
    exp

let test_bitor test_ctx =
  let exp = parse_string_exn "a | b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 4) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (a, Arith Bitor, b)) (on_first_line 0)))
    exp

let test_bitxor test_ctx =
  let exp = parse_string_exn "a ^ b" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let b = mk_exp (Var "b") (on_first_line 4) in
  assert_exp_eq
    (add_script (mk_exp (BinOp (a, Arith Bitxor, b)) (on_first_line 0)))
    exp

let test_return test_ctx =
  let exp = parse_string_exn "function f() {return}" in
  let r = mk_exp (Return None) (on_first_line 14) in
  let block = mk_exp (Block [ r ]) (on_first_line 13) in
  assert_exp_eq
    (add_script
       (mk_exp (Function (false, Some "f", [], block)) (on_first_line 0)))
    exp

let test_return_exp test_ctx =
  let exp = parse_string_exn "function f() {return g()}" in
  let g = mk_exp (Var "g") (on_first_line 21) in
  let gcall = mk_exp (Call (g, [])) (on_first_line 21) in
  let r = mk_exp (Return (Some gcall)) (on_first_line 14) in
  let block = mk_exp (Block [ r ]) (on_first_line 13) in
  assert_exp_eq
    (add_script
       (mk_exp (Function (false, Some "f", [], block)) (on_first_line 0)))
    exp

let test_do_while test_ctx =
  let exp = parse_string_exn "do { a = 1 } while (a < 5)" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let five = mk_exp (Num 5.0) (on_first_line 0) in
  let condition = mk_exp (BinOp (a, Comparison Lt, five)) (on_first_line 0) in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let one = mk_exp (Num 1.0) (on_first_line 0) in
  let assignment = mk_exp (Assign (a, one)) (on_first_line 0) in
  let body = mk_exp (Block [ assignment ]) (on_first_line 0) in
  let loop = mk_exp (DoWhile (body, condition)) (on_first_line 0) in
  let () =
    Printf.printf "%s"
      (JS_Parser.PrettyPrint.string_of_exp true exp)
  in
  assert_exp_eq (add_script loop) exp

let test_delete test_ctx =
  let exp = parse_string_exn "delete a" in
  let a = mk_exp (Var "a") (on_first_line 7) in
  assert_exp_eq (add_script (mk_exp (Delete a) (on_first_line 0))) exp

let test_continue test_ctx =
  let exp = parse_string_exn "while (a > 5) {a++; continue}" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let five = mk_exp (Num 5.0) (on_first_line 0) in
  let condition = mk_exp (BinOp (a, Comparison Gt, five)) (on_first_line 0) in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let app = mk_exp (Unary_op (Post_Incr, a)) (on_first_line 0) in
  let cont = mk_exp (Continue None) (on_first_line 0) in
  let body = mk_exp (Block [ app; cont ]) (on_first_line 0) in
  assert_exp_eq
    (add_script (mk_exp (While (condition, body)) (on_first_line 0)))
    exp

let test_continue_label test_ctx =
  let exp = parse_string_exn "test: while (a > 5) {a++; continue test}" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let five = mk_exp (Num 5.0) (on_first_line 0) in
  let condition = mk_exp (BinOp (a, Comparison Gt, five)) (on_first_line 0) in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let app = mk_exp (Unary_op (Post_Incr, a)) (on_first_line 0) in
  let cont = mk_exp (Continue (Some "test")) (on_first_line 0) in
  let body = mk_exp (Block [ app; cont ]) (on_first_line 0) in
  let loop = mk_exp (While (condition, body)) (on_first_line 0) in
  assert_exp_eq
    (add_script (mk_exp (Label ("test", loop)) (on_first_line 0)))
    exp

let test_break test_ctx =
  let exp = parse_string_exn "while (a > 5) {a++; break}" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let five = mk_exp (Num 5.0) (on_first_line 0) in
  let condition = mk_exp (BinOp (a, Comparison Gt, five)) (on_first_line 0) in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let app = mk_exp (Unary_op (Post_Incr, a)) (on_first_line 0) in
  let cont = mk_exp (Break None) (on_first_line 0) in
  let body = mk_exp (Block [ app; cont ]) (on_first_line 0) in
  assert_exp_eq
    (add_script (mk_exp (While (condition, body)) (on_first_line 0)))
    exp

let test_break_label test_ctx =
  let exp = parse_string_exn "test: while (a > 5) {a++; break test}" in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let five = mk_exp (Num 5.0) (on_first_line 0) in
  let condition = mk_exp (BinOp (a, Comparison Gt, five)) (on_first_line 0) in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let app = mk_exp (Unary_op (Post_Incr, a)) (on_first_line 0) in
  let cont = mk_exp (Break (Some "test")) (on_first_line 0) in
  let body = mk_exp (Block [ app; cont ]) (on_first_line 0) in
  let loop = mk_exp (While (condition, body)) (on_first_line 0) in
  assert_exp_eq
    (add_script (mk_exp (Label ("test", loop)) (on_first_line 0)))
    exp

let test_try_catch test_ctx =
  let exp = parse_string_exn "try {a} catch (b) {c}" in
  let a = mk_exp (Var "a") (on_first_line 5) in
  let ablock = mk_exp (Block [ a ]) (on_first_line 4) in
  let c = mk_exp (Var "c") (on_first_line 19) in
  let cblock = mk_exp (Block [ c ]) (on_first_line 18) in
  assert_exp_eq
    (add_script
       (mk_exp (Try (ablock, Some ("b", cblock), None)) (on_first_line 0)))
    exp

let test_try_catch_finally test_ctx =
  let exp = parse_string_exn "try {a} catch (b) {c} finally {d}" in
  let a = mk_exp (Var "a") (on_first_line 5) in
  let ablock = mk_exp (Block [ a ]) (on_first_line 4) in
  let c = mk_exp (Var "c") (on_first_line 19) in
  let cblock = mk_exp (Block [ c ]) (on_first_line 18) in
  let d = mk_exp (Var "d") (on_first_line 31) in
  let dblock = mk_exp (Block [ d ]) (on_first_line 30) in
  assert_exp_eq
    (add_script
       (mk_exp
          (Try (ablock, Some ("b", cblock), Some dblock))
          (on_first_line 0)))
    exp

let test_try_finally test_ctx =
  let exp = parse_string_exn "try {a} finally {d}" in
  let a = mk_exp (Var "a") (on_first_line 5) in
  let ablock = mk_exp (Block [ a ]) (on_first_line 4) in
  let d = mk_exp (Var "d") (on_first_line 17) in
  let dblock = mk_exp (Block [ d ]) (on_first_line 16) in
  assert_exp_eq
    (add_script (mk_exp (Try (ablock, None, Some dblock)) (on_first_line 0)))
    exp

let test_switch test_ctx =
  let exp =
    parse_string_exn "switch (a) { case 1 : b; break; default : d; case 2 : c }"
  in
  let a = mk_exp (Var "a") (on_first_line 8) in
  let one = mk_exp (Num 1.0) (on_first_line 18) in
  let b = mk_exp (Var "b") (on_first_line 22) in
  let break = mk_exp (Break None) (on_first_line 25) in
  let block1 = mk_exp (Block [ b; break ]) (on_first_line 13) in
  let d = mk_exp (Var "d") (on_first_line 42) in
  let block2 = mk_exp (Block [ d ]) (on_first_line 32) in
  let two = mk_exp (Num 2.0) (on_first_line 50) in
  let c = mk_exp (Var "c") (on_first_line 54) in
  let block3 = mk_exp (Block [ c ]) (on_first_line 45) in
  assert_exp_eq
    (add_script
       (mk_exp
          (Switch
             ( a,
               [ (Case one, block1); (DefaultCase, block2); (Case two, block3) ]
             ))
          (on_first_line 0)))
    exp

let test_debugger test_ctx =
  let exp = parse_string_exn "debugger" in
  assert_exp_eq (add_script (mk_exp Debugger (on_first_line 0))) exp

let test_script_strict test_ctx =
  let exp = parse_string_exn "'use strict'; function f() {return}" in
  let string_exp = mk_exp (String "use strict") (on_first_line 0) in
  let r = mk_exp (Return None) (on_first_line 28) in
  let block = mk_exp (Block [ r ]) (on_first_line 14) in
  let script =
    mk_exp
      (Script
         ( true,
           [
             string_exp;
             mk_exp (Function (true, Some "f", [], block)) (on_first_line 14);
           ] ))
      (on_first_line 0)
  in
  assert_exp_eq script exp

let test_script_strict_break test_ctx =
  let exp = parse_string_exn {|'use\
   strict'; function f() {return}|} in
  let string_exp = mk_exp (String "use   strict") (on_first_line 0) in
  let r = mk_exp (Return None) (on_first_line 28) in
  let block = mk_exp (Block [ r ]) (on_first_line 14) in
  let script =
    mk_exp
      (Script
         ( false,
           [
             string_exp;
             mk_exp (Function (false, Some "f", [], block)) (on_first_line 14);
           ] ))
      (on_first_line 0)
  in
  assert_exp_eq script exp

let test_script_not_strict test_ctx =
  let exp = parse_string_exn "{'use strict'}; function f() {return}" in
  let string_exp = mk_exp (String "use strict") (on_first_line 1) in
  let block_strict = mk_exp (Block [ string_exp ]) (on_first_line 0) in
  let r = mk_exp (Return None) (on_first_line 30) in
  let block = mk_exp (Block [ r ]) (on_first_line 29) in
  let empty = mk_exp Skip (on_first_line 14) in
  let script =
    mk_exp
      (Script
         ( false,
           [
             block_strict;
             empty;
             mk_exp (Function (false, Some "f", [], block)) (on_first_line 16);
           ] ))
      (on_first_line 0)
  in
  assert_exp_eq script exp

let test_fun_strict test_ctx =
  let exp = parse_string_exn "function f() {'use strict'; return}" in
  let string_exp = mk_exp (String "use strict") (on_first_line 14) in
  let r = mk_exp (Return None) (on_first_line 28) in
  let block = mk_exp (Block [ string_exp; r ]) (on_first_line 13) in
  let script =
    mk_exp
      (Script
         ( false,
           [ mk_exp (Function (true, Some "f", [], block)) (on_first_line 0) ]
         ))
      (on_first_line 0)
  in
  assert_exp_eq script exp

let test_fun_strict_nested test_ctx =
  let exp =
    parse_string_exn
      {|
  function f1() {
  "use strict";
  return (function () {return})
  }|}
  in
  let use_strict_exp = mk_exp (String "use strict") (on_first_line 14) in
  let rn = mk_exp (Return None) (on_first_line 0) in
  let block_nes = mk_exp (Block [ rn ]) (on_first_line 0) in
  let f_exp =
    mk_exp (FunctionExp (true, None, [], block_nes)) (on_first_line 0)
  in
  let r = mk_exp (Return (Some f_exp)) (on_first_line 28) in
  let block = mk_exp (Block [ use_strict_exp; r ]) (on_first_line 13) in
  let script =
    mk_exp
      (Script
         ( false,
           [ mk_exp (Function (true, Some "f1", [], block)) (on_first_line 0) ]
         ))
      (on_first_line 0)
  in
  assert_exp_eq script exp

let test_fun_strict_break test_ctx =
  let exp = parse_string_exn {|function f() {'use\
   strict'; return}|} in
  let string_exp = mk_exp (String "use   strict") (on_first_line 14) in
  let r = mk_exp (Return None) (on_first_line 32) in
  let block = mk_exp (Block [ string_exp; r ]) (on_first_line 0) in
  let script =
    mk_exp
      (Script
         ( false,
           [ mk_exp (Function (false, Some "f", [], block)) (on_first_line 0) ]
         ))
      (on_first_line 0)
  in
  assert_exp_eq script exp

let test_getter test_ctx =
  let exp = parse_string_exn "a = {get y() { return 0;}};" in
  let zero = mk_exp (Num 0.0) (on_first_line 22) in
  let r = mk_exp (Return (Some zero)) (on_first_line 15) in
  let block = mk_exp (Block [ r ]) (on_first_line 13) in
  let getter =
    mk_exp (FunctionExp (false, None, [], block)) (on_first_line 9)
  in
  let obj =
    mk_exp (Obj [ (PropnameId "y", PropbodyGet, getter) ]) (on_first_line 4)
  in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let assign = mk_exp (Assign (a, obj)) (on_first_line 0) in
  let script = mk_exp (Script (false, [ assign ])) (on_first_line 0) in
  assert_exp_eq script exp

let test_setter test_ctx =
  let exp = parse_string_exn "a = {set y(val) {}};" in
  let block = mk_exp (Block []) (on_first_line 16) in
  let setter =
    mk_exp (FunctionExp (false, None, [ "val" ], block)) (on_first_line 9)
  in
  let obj =
    mk_exp (Obj [ (PropnameId "y", PropbodySet, setter) ]) (on_first_line 4)
  in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let assign = mk_exp (Assign (a, obj)) (on_first_line 0) in
  let script = mk_exp (Script (false, [ assign ])) (on_first_line 0) in
  assert_exp_eq script exp

let test_obj_init test_ctx =
  let exp = parse_string_exn "a = {1 : b, \"abc\" : c, name : d};" in
  let b = mk_exp (Var "b") (on_first_line 9) in
  let c = mk_exp (Var "c") (on_first_line 20) in
  let d = mk_exp (Var "d") (on_first_line 30) in
  let obj =
    mk_exp
      (Obj
         [
           (PropnameNum 1.0, PropbodyVal, b);
           (PropnameString "abc", PropbodyVal, c);
           (PropnameId "name", PropbodyVal, d);
         ])
      (on_first_line 4)
  in
  let a = mk_exp (Var "a") (on_first_line 0) in
  let assign = mk_exp (Assign (a, obj)) (on_first_line 0) in
  let script = mk_exp (Script (false, [ assign ])) (on_first_line 0) in
  assert_exp_eq script exp

(* TODO: Find why this test is not used anymore
 
  let test_fun_annot test_ctx =
  let exp = exp_from_file "test.js" in
  let string_exp = mk_exp (String "use strict") 52 in
  let r = mk_exp (Return None) 66 in
  let block = mk_exp (Block [string_exp; r]) 51 in
  let f = mk_exp_with_annot (Function (true, Some "f", [], block)) 38
    [{annot_type = EnsuresErr; annot_formula = "B"}] in
  let script = mk_exp_with_annot (Script (false, [f])) 0
    [{annot_type = TopEnsuresErr; annot_formula = "A"}] in
  assert_equal' script exp  *)

(* TODO: tests for object initializer, unnamed function expression *)

let suite =
  (* hack around oUnit *)
  let (>::) a b = (a, b) in
  [
         "test var" >:: test_var;
         "test var with assignment" >:: test_var_value;
         "test var list" >:: test_var_list;
         "test regexp" >:: test_regexp;
         "test regexp with flags" >:: test_regexp_with_flags;
         "test not" >:: test_not;
         "test_caccess" >:: test_caccess;
         "test_and" >:: test_and;
         "test_array_literal" >:: test_array_literal;
         "test_ge" >:: test_ge;
         "test_or" >:: test_or;
         "test_not_triple_eq" >:: test_not_triple_eq;
         "test_hook" >:: test_hook;
         "test_instanceof" >:: test_instanceof;
         "test_typeof" >:: test_typeof;
         "test_pos" >:: test_pos;
         "test_dec_pre" >:: test_dec_pre;
         "test_dec_post" >:: test_dec_post;
         "test_inc_pre" >:: test_inc_pre;
         "test_inc_post" >:: test_inc_post;
         "test_for" >:: test_for;
         "test_forin" >:: test_forin;
         "test_mod" >:: test_mod;
         "test_ursh" >:: test_ursh;
         "test_lsh" >:: test_lsh;
         "test_rsh" >:: test_rsh;
         "test_bitand" >:: test_bitand;
         "test_bitor" >:: test_bitor;
         "test_bitxor" >:: test_bitxor;
         "test_notequal" >:: test_notequal;
         "test_gt" >:: test_gt;
         "test_in" >:: test_in;
         "test_comma1" >:: test_comma1;
         "test_comma2" >:: test_comma2;
         "test_negative" >:: test_negative;
         "test_bitnot" >:: test_bitnot;
         "test_void" >:: test_void;
         "test_assign_add" >:: test_assign_add;
         "test_assign_sub" >:: test_assign_sub;
         "test_assign_mul" >:: test_assign_mul;
         "test_assign_div" >:: test_assign_div;
         "test_assign_mod" >:: test_assign_mod;
         "test_assign_ursh" >:: test_assign_ursh;
         "test_assign_lsh" >:: test_assign_lsh;
         "test_assign_rsh" >:: test_assign_rsh;
         "test_assign_bitand" >:: test_assign_bitand;
         "test_assign_bitor" >:: test_assign_bitor;
         "test_assign_bitxor" >:: test_assign_bitxor;
         "test_return" >:: test_return;
         "test_return_exp" >:: test_return_exp;
         "test_do_while" >:: test_do_while;
         "test_delete" >:: test_delete;
         "test_continue" >:: test_continue;
         "test_continue_label" >:: test_continue_label;
         "test_break" >:: test_break;
         "test_break_label" >:: test_break_label;
         "test_try_catch" >:: test_try_catch;
         "test_try_catch_finally" >:: test_try_catch_finally;
         "test_try_finally" >:: test_try_finally;
         "test_switch" >:: test_switch;
         "test_debugger" >:: test_debugger;
         "test_script_strict" >:: test_script_strict;
         "test_script_strict_break" >:: test_script_strict_break;
         "test_script_not_strict" >:: test_script_not_strict;
         "test_fun_strict" >:: test_fun_strict;
         "test_fun_strict_nested" >:: test_fun_strict_nested;
         "test_fun_strict_break" >:: test_fun_strict_break;
         "test_getter" >:: test_getter;
         "test_setter" >:: test_setter;
         "test_obj_init" >:: test_obj_init;
         (* "test_fun_annot" >:: test_fun_annot; *)
       ]

let alco_suite =
  let open Alcotest in
  List.map (fun (test_name, f) -> test_case test_name `Quick f) suite
  
  
let () =
  let open Alcotest in
  run "JS_Parser" [
    "Testing", alco_suite
  ]
