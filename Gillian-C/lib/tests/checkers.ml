open Alcotest
open Monadic
open Cgil_lib.SHeapTree
open Cgil_lib
open Gil_syntax

let perm = testable Perm.pp Perm.Infix.( =% )

let sval = testable SVal.pp SVal.equal

let err = testable pp_err err_equal

let expr = testable Expr.pp Expr.equal

let range = pair expr expr

let node = testable Node.pp Node.equal

let tree = testable Tree.pp (Tree.equal ~pc:Pc.empty)

let heaptree = testable pp equal

let pc = testable Pc.pp Pc.equal

let results (cont : 'a testable) (term : 'b testable) :
    ('a, 'b) Results.t testable =
  testable
    (Results.pp ~pp_cont:(Alcotest.pp cont) ~pp_term:(Alcotest.pp term))
    (Results.equal ~eq_cont:(Alcotest.equal cont)
       ~eq_term:(Alcotest.equal term))

let sat_results (cont : 'a testable) (term : 'b testable) :
    ('a, 'b) SatResults.t testable =
  results (pair cont pc) (pair term pc)
