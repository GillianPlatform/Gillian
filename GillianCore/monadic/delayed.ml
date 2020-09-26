module Formula = Gil_syntax.Formula
module Expr = Gil_syntax.Expr

exception NonExhaustiveEntailment of Formula.t list

type 'a guarded_thunk = { guard : Formula.t; thunk : unit -> 'a }

(** When using Branching, it should be certain that the paths are complete *)
and _ t =
  | Final            : { final_value : 'a; additional_knowledge : Formula.t list } -> 'a
                                                                                t
  | BranchEntailment : 'a t guarded_thunk list -> 'a t
  | BranchSat        : 'a t guarded_thunk list -> 'a t
  | ResolveLoc       : Expr.t -> string option t
  | Bound            : ('a t * ('a -> 'b t)) -> 'b t

let guarded_thunk_of_pair (guard, thunk) = { guard; thunk }

let rec resolve : type a. curr_pc:Pc.t -> a t -> a Branch.t list =
 fun ~curr_pc process ->
  match process with
  | Final { final_value; additional_knowledge } ->
      [ { pc = Pc.extend curr_pc additional_knowledge; value = final_value } ]
  | BranchSat branches ->
      let get_branches l =
        let rec loop acc no_sat_path l =
          match l with
          | []                    -> acc
          | [ { guard; thunk } ]  ->
              let should_go_in =
                if no_sat_path then
                  (* No previoues path was SAT, so this one has to be *)
                  true
                else FOSolver.sat ~pc:curr_pc guard
              in
              if should_go_in then
                let extended_pc = Pc.extend curr_pc [ guard ] in
                let new_delayed = thunk () in
                let follow_up = resolve ~curr_pc:extended_pc new_delayed in
                loop (follow_up @ acc) false []
              else loop acc false []
          | { guard; thunk } :: r ->
              if FOSolver.sat ~pc:curr_pc guard then
                (* I don't think that I need to copy the PC, because extending doesn't modify the original
                   mutable PFS and Gamma, but just in case let's put a comment here. *)
                let extended_pc = Pc.extend curr_pc [ guard ] in
                let new_delayed = thunk () in
                let follow_up = resolve ~curr_pc:extended_pc new_delayed in
                loop (follow_up @ acc) false r
              else loop acc no_sat_path r
        in
        loop [] true l
      in
      get_branches branches
  | Bound (x, f) ->
      let branches_of_first_comp = resolve ~curr_pc x in
      let continue =
        List.map
          (fun Branch.{ pc; value } -> resolve ~curr_pc:pc (f value))
          branches_of_first_comp
      in
      List.concat continue
  | BranchEntailment branches ->
      let rec resolve_list l =
        match l with
        | []                    ->
            raise
              (NonExhaustiveEntailment (List.map (fun b -> b.guard) branches))
        | { guard; thunk } :: r -> (
            match guard with
            | True ->
                (* Quick speedup *)
                let extended_pc = Pc.extend curr_pc [ guard ] in
                resolve ~curr_pc:extended_pc (thunk ())
            | _    ->
                if FOSolver.check_entailment ~pc:curr_pc guard then
                  let extended_pc = Pc.extend curr_pc [ guard ] in
                  resolve ~curr_pc:extended_pc (thunk ())
                else resolve_list r )
      in

      resolve_list branches
  | ResolveLoc loc_expr ->
      [
        { pc = curr_pc; value = FOSolver.resolve_loc_name ~pc:curr_pc loc_expr };
      ]

let return ?(learned = []) final_value =
  Final { final_value; additional_knowledge = learned }

let bind x f = Bound (x, f)

let resolve_loc loc = ResolveLoc loc

let if_sure
    (ent_guard : Formula.t) ~(then_ : unit -> 'a t) ~(else_ : unit -> 'a t) =
  BranchEntailment
    [ { guard = ent_guard; thunk = then_ }; { guard = True; thunk = else_ } ]

let branch_on guard ~(then_ : unit -> 'a t) ~(else_ : unit -> 'a t) =
  BranchSat
    [ { guard; thunk = then_ }; { guard = Formula.Not guard; thunk = else_ } ]

let branch_entailment branches =
  BranchEntailment (List.map guarded_thunk_of_pair branches)

let map x f = Bound (x, fun x -> return (f x))

module Syntax = struct
  let ( let* ) = bind

  let ( let+ ) = map
end
