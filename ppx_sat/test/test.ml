open Gillian
open Gil_syntax
open Monadic.Delayed

let add = ( + )

open Expr.Infix
open Monadic.Delayed.Syntax

let zero = Expr.int 0
let one = Expr.int 1
let two = Expr.int 2
let int_pat n x = x == Expr.int n
let zero_pat = int_pat 0
let one_pat = int_pat 1
let two_pat = int_pat 2
let three_pat = int_pat 3

let pp_branches =
  let open Fmt in
  brackets (list ~sep:comma (Monadic.Branch.pp int))

module type S = sig
  val run : unit -> unit
end

module Test_if_sat = struct
  let computation t = if%sat t >= one then return 10 else return 0

  let process x =
    let* z = computation x in
    let* y =
      if%sat x <= zero then return (-1)
      else
        if%sat x <= one then return 0
        else if%sat x >= two then return 2 else return 1
    in
    return (add z y)

  let starting_pc x =
    Monadic.Pc.make
      ~pfs:(Engine.PFS.of_list [ not (x == one) ])
      ~gamma:(Engine.Type_env.init ()) ()

  let results =
    let x = Expr.LVar "x" in
    let curr_pc = starting_pc x in
    resolve ~curr_pc (process x)

  let run () = Fmt.pr "%a@.@." pp_branches results
end

module Test_match_ent = struct
  let process x =
    match%ent x with
    | zero_pat -> return 0
    | one_pat -> return 1
    | two_pat -> return 2
    | three_pat -> return 3
    | _ -> return (-1)

  let pc_with_no_info = Monadic.Pc.init ()

  let pc_with_two x =
    Monadic.Pc.make
      ~pfs:(Engine.PFS.of_list [ x == two ])
      ~gamma:(Engine.Type_env.init ()) ()

  let results_no_info =
    let x = Expr.LVar "x" in
    let curr_pc = pc_with_no_info in
    resolve ~curr_pc (process x)

  let results_two =
    let x = Expr.LVar "x" in
    let curr_pc = pc_with_two x in
    resolve ~curr_pc (process x)

  let run () =
    Fmt.pr "%a@.@.%a@.@." pp_branches results_no_info pp_branches results_two
end

module Test_match_sat = struct
  let computation t = if%sat t >= one then return 10 else return 0

  let process x =
    let* z = computation x in
    let lt_zero x = x <= zero in
    let lt_one x = x <= one in
    let gt_two x = x >= two in
    let* y =
      match%sat x with
      | lt_zero -> return (-1)
      | lt_one -> return 0
      | gt_two -> return 2
      | _ -> return 1
    in
    return (Stdlib.( + ) z y)

  let starting_pc x =
    Monadic.Pc.make
      ~pfs:(Engine.PFS.of_list [ not (x == one) ])
      ~gamma:(Engine.Type_env.init ()) ()

  let results =
    let x = Expr.LVar "x" in
    let curr_pc = starting_pc x in
    resolve ~curr_pc (process x)

  let run () = Fmt.pr "%a@.@." pp_branches results
end

let () =
  List.iter
    (fun (module M : S) -> M.run ())
    [ (module Test_if_sat); (module Test_match_ent); (module Test_match_sat) ]
