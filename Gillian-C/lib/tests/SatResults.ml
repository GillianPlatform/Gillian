open Monadic
open SatResults

let pp_term : [< `Error ] Fmt.t =
 fun fmt -> function
  | `Error -> Fmt.pf fmt "Error"

let eq_term : [< `Error ] -> [< `Error ] -> bool =
 fun a b ->
  match (a, b) with
  | `Error, `Error -> true

let pp_cont : [< `Continue of int ] Fmt.t =
 fun fmt -> function
  | `Continue i -> Fmt.pf fmt "Continue %i" i

let eq_cont : [< `Continue of int ] -> [< `Continue of int ] -> bool =
 fun a b ->
  match (a, b) with
  | `Continue i, `Continue j -> Int.equal i j

let sat_res :
    ([< `Continue of int ], [< `Error ]) SatResults.t Alcotest.testable =
  Alcotest.testable
    (SatResults.pp ~pp_cont ~pp_term)
    (SatResults.equal ~eq_cont ~eq_term)

let branch pc = branch_on_sat ~pc

let basic =
  let run () =
    let open Gil_syntax in
    let open Formula.Infix in
    let open Syntax in
    let pfs = Gillian.Symbolic.PureContext.init () in
    let gamma = Gillian.Symbolic.TypEnv.init () in
    let pc = Pc.make ~pfs ~gamma () in
    let x = Expr.LVar (LVar.alloc ()) in
    let n = Expr.num in
    let zero = n 0. in
    let ten = n 10. in
    let res =
      branch_on_sat ~pc x #< zero
        ~then_branch:(fun pc -> SatResults.return ~pc (`Continue (-1)))
        ~else_branch:(fun pc ->
          let** `Continue n, pc =
            branch_on_sat ~pc x #> ten
              ~then_branch:(fun pc -> SatResults.return ~pc (`Continue 1))
              ~else_branch:(fun pc -> SatResults.terminate ~pc `Error)
          in
          SatResults.return ~pc (`Continue (n + 1)))
    in
    let tb = (`Continue (-1), Pc.extend pc [ x #< zero ]) in
    let etb = (`Continue 2, Pc.extend pc [ Formula.Not x #< zero; x #> ten ]) in
    let eeb =
      (`Error, Pc.extend pc [ Formula.Not x #< zero; Formula.Not x #> ten ])
    in
    Alcotest.(check sat_res)
      "Should branch properly"
      (Results.make ~cont:[ tb; etb ] ~term:[ eeb ])
      res
  in
  ("Nested sat checks", `Quick, run)

let tests = [ basic ]
