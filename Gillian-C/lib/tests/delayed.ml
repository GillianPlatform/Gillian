open Monadic
open Gil_syntax

let n x = Expr.num x

let var x = Expr.LVar x

let simple_test =
  let run () =
    let process =
      let open Delayed in
      let open Formula.Infix in
      let x = var "x" in
      let two = n 2. in
      branch_on x#>two
        ~then_branch:(fun () -> return 1)
        ~else_branch:(fun () ->
          let three = n 3. in
          branch_on x#==three
            ~then_branch:(fun () ->
              (* This can't happen, because we know that [not (x #> 2)] *)
              return 2)
            ~else_branch:(fun () -> return 3))
    in
    let curr_pc = Pc.init () in
    let branches = Delayed.resolve ~curr_pc process in
    Alcotest.(check (list int))
      "The false branch should not be retrieved" [ 1; 3 ]
      (List.map Branch.value branches)
  in
  ("Basic delayed check", `Quick, run)

let tests = [ simple_test ]
