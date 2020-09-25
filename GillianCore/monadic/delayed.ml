module Formula = Gil_syntax.Formula

type 'a guarded_thunk = { guard : Formula.t; thunk : unit -> 'a }

(** When using Branching, it should be certain that the paths are complete *)
and _ t =
  | Final        : 'a -> 'a t
  | IfEntailment : {
      ent_guard : Formula.t;
      then_ : unit -> 'a t;
      else_ : unit -> 'a t;
    }
      -> 'a t
  | Branching    : 'a t guarded_thunk list -> 'a t
  | Bound        : ('a t * ('a -> 'b t)) -> 'b t

let rec resolve : type a. curr_pc:Pc.t -> a t -> a Branch.t list =
 fun ~curr_pc process ->
  match process with
  | Final z -> [ { pc = curr_pc; value = z } ]
  | Branching branches ->
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
                let extended_pc = Pc.extend (Pc.copy curr_pc) [ guard ] in
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
  | IfEntailment { ent_guard; then_; else_ } ->
      if FOSolver.check_entailment ~pc:curr_pc ent_guard then
        let curr_pc = Pc.extend curr_pc [ ent_guard ] in
        resolve ~curr_pc (then_ ())
      else resolve ~curr_pc (else_ ())

let return x = Final x

let bind x f = Bound (x, f)

let if_sure
    (ent_guard : Formula.t) ~(then_ : unit -> 'a t) ~(else_ : unit -> 'a t) =
  IfEntailment { ent_guard; then_; else_ }

let branch_on guard ~(then_ : unit -> 'a t) ~(else_ : unit -> 'a t) =
  Branching
    [ { guard; thunk = then_ }; { guard = Formula.Not guard; thunk = else_ } ]

let map x f = Bound (x, fun x -> Final (f x))

module Syntax = struct
  let ( let* ) = bind

  let ( let+ ) = map
end
