module Formula = Gil_syntax.Formula
module Expr = Gil_syntax.Expr

exception NonExhaustiveEntailment of Formula.t list

type 'a t = curr_pc:Pc.t -> 'a Branch.t list

let resolve ~curr_pc p = p ~curr_pc

(** When using Branching, it should be certain that the paths are complete *)

let return ?(learned = []) ?(learned_types = []) final_value ~curr_pc =
  let new_pc = Pc.extend (Pc.extend_types curr_pc learned_types) learned in
  [ Branch.make ~pc:new_pc ~value:final_value ]

let bind (x : 'a t) (f : 'a -> 'b t) ~curr_pc =
  List.concat_map
    (fun b -> f ~curr_pc:(Branch.pc b) (Branch.value b))
    (x ~curr_pc)

let branch_on
    (guard : Formula.t) ~(then_ : unit -> 'a t) ~(else_ : unit -> 'a t) ~curr_pc
    =
  match guard with
  | True  -> then_ () ~curr_pc
  | False -> else_ () ~curr_pc
  | guard ->
      let guard_sat = FOSolver.sat ~pc:curr_pc guard in
      if not guard_sat then (* [Not guard)] has to be sat *)
        else_ () ~curr_pc
      else
        let then_branches = then_ () ~curr_pc:(Pc.extend curr_pc [ guard ]) in
        let not_guard = Formula.Infix.fnot guard in
        if FOSolver.sat ~pc:curr_pc not_guard then
          let else_branches =
            else_ () ~curr_pc:(Pc.extend curr_pc [ not_guard ])
          in
          then_branches @ else_branches
        else then_branches

let if_sure
    (guard : Formula.t) ~(then_ : unit -> 'a t) ~(else_ : unit -> 'a t) ~curr_pc
    =
  match guard with
  | True  -> then_ () ~curr_pc
  | False -> else_ () ~curr_pc
  | guard ->
      if FOSolver.check_entailment ~pc:curr_pc guard then
        let extended_pc = Pc.extend curr_pc [ guard ] in
        then_ () ~curr_pc:extended_pc
      else
        let not_guard = Formula.Infix.fnot guard in
        if FOSolver.check_entailment ~pc:curr_pc not_guard then
          let extended_pc = Pc.extend curr_pc [ not_guard ] in
          else_ () ~curr_pc:extended_pc
        else raise (NonExhaustiveEntailment [ guard; not_guard ])

let branch_entailment (branches : (Formula.t * (unit -> 'a t)) list) ~curr_pc =
  let rec loop l =
    match l with
    | []                  -> raise
                               (NonExhaustiveEntailment (List.map fst branches))
    | (guard, thunk) :: r -> (
        match guard with
        | Formula.True -> thunk () ~curr_pc
        | False        -> loop r
        | _            ->
            if FOSolver.check_entailment ~pc:curr_pc guard then
              thunk () ~curr_pc
            else loop r)
  in
  loop branches

let map x f ~curr_pc =
  List.map
    (fun b ->
      let open Branch in
      { b with value = f b.value })
    (x ~curr_pc)

let reduce e ~curr_pc =
  [ Branch.make ~pc:curr_pc ~value:(FOSolver.reduce_expr ~pc:curr_pc e) ]

let resolve_loc l ~curr_pc =
  [ Branch.make ~pc:curr_pc ~value:(FOSolver.resolve_loc_name ~pc:curr_pc l) ]

module Syntax = struct
  let ( let* ) = bind

  let ( let+ ) = map
end
