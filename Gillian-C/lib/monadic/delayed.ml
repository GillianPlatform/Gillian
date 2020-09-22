module Formula = Gil_syntax.Formula

type 'a guarded_thunk = { guard : Formula.t; thunk : unit -> 'a }

and _ t =
  | Final     : 'a -> 'a t
  | Branching : 'a t guarded_thunk list -> 'a t
  | Bound     : ('a t * ('a -> 'b t)) -> 'b t

let rec resolve : type a. curr_pc:Pc.t -> a t -> a Branch.t list =
 fun ~curr_pc process ->
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
  | Bound (x, f)       ->
      let branches_of_first_comp = resolve ~curr_pc x in
      let continue =
        List.map
          (fun Branch.{ pc; value } -> resolve ~curr_pc:pc (f value))
          branches_of_first_comp
      in
      List.concat continue

let return x = Final x

let bind x f = Bound (x, f)

let branch_on guard ~(then_branch : unit -> 'a t) ~(else_branch : unit -> 'a t)
    =
  Branching
    [
      { guard; thunk = then_branch };
      { guard = Formula.Not guard; thunk = else_branch };
    ]

let map x f = Bound (x, fun x -> Final (f x))

module Syntax = struct
  let ( let* ) = bind

  let ( let+ ) = map
end
