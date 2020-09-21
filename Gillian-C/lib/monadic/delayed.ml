module Formula = Gil_syntax.Formula

type 'a guarded_thunk = { guard : Formula.t; thunk : unit -> 'a }

and 'a t = Final of 'a | Branching of 'a t guarded_thunk list

let rec resolve ~curr_pc (process : 'a t) : 'a Branch.t list =
  match process with
  | Final z            -> [ { pc = curr_pc; value = z } ]
  | Branching branches ->
      let different_branches =
        List.filter_map
          (fun { guard; thunk } ->
            if FOSolver.sat ~pc:curr_pc guard then
              let extended_pc = Pc.extend (Pc.copy curr_pc) [ guard ] in
              let new_delayed = thunk () in
              Some (resolve ~curr_pc:extended_pc new_delayed)
            else None)
          branches
      in
      List.concat different_branches

let return x = Final x

let branch_on guard ~(then_branch : unit -> 'a t) ~(else_branch : unit -> 'a t)
    =
  Branching
    [
      { guard; thunk = then_branch };
      { guard = Formula.Not guard; thunk = else_branch };
    ]
