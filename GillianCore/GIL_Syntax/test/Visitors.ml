open Gil_syntax
open Ast_check

let test_lit_base_elements () =
  let open Literal in
  let unordered_list_literal = Alcotest.slist literal compare in
  let simple_lit = Undefined in
  Alcotest.check unordered_list_literal "Undefined's base element is itself"
    (base_elements simple_lit) [ Undefined ];
  let expected = [ Undefined; Null; Num 32.; Bool false ] in
  let lit_list = LList expected in
  Alcotest.check unordered_list_literal
    "The base elements of a list of literals is that list"
    (base_elements lit_list) expected;
  let rec_lit_list =
    LList [ Undefined; LList [ Null; LList [ Bool false ] ] ]
  in
  let expected = [ Undefined; Null; Bool false ] in
  Alcotest.check unordered_list_literal "Get base elements in recursive list"
    (base_elements rec_lit_list)
    expected;
  ()

let test_expr_base_elements () =
  let open Literal in
  let open Expr in
  let list_expr = Alcotest.list expr in
  let simple_lit = Lit Undefined in
  Alcotest.check list_expr "Undefined's base element is itself"
    (base_elements simple_lit) [ Lit Undefined ];
  let inner = [ Undefined; Null; Num 32.; Bool false ] in
  let lit_list = Lit (LList inner) in
  Alcotest.check list_expr
    "The base elements of a list of undefined is that list"
    (base_elements lit_list)
    (List.map (fun x -> Lit x) inner);
  let rec_expr_list =
    EList
      [
        Lit Undefined;
        EList
          [
            Lit (LList [ Bool false ]);
            BinOp (UnOp (UNot, Lit (Num 32.)), FPlus, PVar "b");
          ];
        LVar "a";
        ALoc "e";
      ]
  in
  let expected =
    [ Lit Undefined; Lit (Bool false); Lit (Num 32.); LVar "a"; ALoc "e" ]
  in
  Alcotest.check list_expr "Get base elements in recursive list"
    (base_elements rec_expr_list)
    expected;
  ()

let tests =
  [
    ("literal_base_elements", `Quick, test_lit_base_elements);
    ("expr_base_elements", `Quick, test_expr_base_elements);
  ]
