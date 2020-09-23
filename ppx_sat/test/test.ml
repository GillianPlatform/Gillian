open Gil_syntax
open Monadic.Delayed
open Formula.Infix
open Monadic.Delayed.Syntax

let zero = Expr.num 0.

let one = Expr.num 1.

let two = Expr.num 2.

let three = Expr.num 3.

let computation t = if%sat t #>= one then return 10 else return 0

let process x =
  let* z = computation x in
  let* y =
    if%sat x #<= zero then return (-1)
    else
      if%sat x #<= one then return 0
      else if%sat x #>= two then return 2 else return 1
  in
  return (z + y)

let starting_pc x =
  Monadic.Pc.make
    ~pfs:(Engine.PFS.of_list [ Formula.Not x #== one ])
    ~gamma:(Engine.TypEnv.init ()) ()

let results =
  let x = Expr.LVar "x" in
  let curr_pc = starting_pc x in
  resolve ~curr_pc (process x)

let () =
  let open Fmt in
  pr "%a" (brackets (list ~sep:comma (Monadic.Branch.pp int))) results
