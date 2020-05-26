open Alcotest
open Cgil_lib.SHeapTree
open Cgil_lib
open Gil_syntax

let perm = testable Perm.pp Perm.Infix.( =% )

let sval = testable SVal.pp SVal.equal

let err = testable pp_err err_equal

let expr = testable Expr.pp Expr.equal

let range = pair expr expr

let node = testable Node.pp Node.equal

let tree = testable Tree.pp Tree.equal

let heaptree = testable pp equal

let get_result = result (pair sval heaptree) err