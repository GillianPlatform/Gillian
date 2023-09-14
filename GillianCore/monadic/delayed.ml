module Formula = Gil_syntax.Formula
module Expr = Gil_syntax.Expr
module Type = Gil_syntax.Type

exception NonExhaustiveEntailment of Formula.t list

let () =
  Printexc.register_printer (function
    | NonExhaustiveEntailment fs ->
        let s =
          Fmt.str "NonExhaustiveEntailment(%a)" (Fmt.Dump.list Formula.pp) fs
        in
        Some s
    | _ -> None)

type 'a t = curr_pc:Pc.t -> 'a Branch.t list

let resolve ~curr_pc p = p ~curr_pc

(** When using Branching, it should be certain that the paths are complete *)

let return ?(learned = []) ?(learned_types = []) final_value ~curr_pc =
  let new_pc = Pc.extend (Pc.extend_types curr_pc learned_types) learned in
  [ Branch.make ~pc:new_pc ~value:final_value ]

let vanish () ~curr_pc = []

let bind (x : 'a t) (f : 'a -> 'b t) ~curr_pc =
  List.concat_map
    (fun b -> f ~curr_pc:(Branch.pc b) (Branch.value b))
    (x ~curr_pc)

let branch_on
    (guard : Formula.t)
    ~(then_ : unit -> 'a t)
    ~(else_ : unit -> 'a t)
    ~curr_pc =
  match guard with
  | True -> then_ () ~curr_pc
  | False -> else_ () ~curr_pc
  | guard -> (
      try
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
      with Z3_encoding.Z3Unknown ->
        Fmt.pr "TIMED OUT ON: %a" Formula.pp guard;
        vanish () ~curr_pc)

let if_sure
    (guard : Formula.t)
    ~(then_ : unit -> 'a t)
    ~(else_ : unit -> 'a t)
    ~curr_pc =
  match guard with
  | True -> then_ () ~curr_pc
  | False -> else_ () ~curr_pc
  | guard ->
      if FOSolver.check_entailment ~pc:curr_pc guard then
        let extended_pc = Pc.extend curr_pc [ guard ] in
        then_ () ~curr_pc:extended_pc
      else else_ () ~curr_pc

let branch_entailment (branches : (Formula.t * (unit -> 'a t)) list) ~curr_pc =
  let rec loop l =
    match l with
    | [] -> raise (NonExhaustiveEntailment (List.map fst branches))
    | (guard, thunk) :: r -> (
        match guard with
        | Formula.True -> thunk () ~curr_pc
        | False -> loop r
        | _ ->
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

let delayed_eval f x ~curr_pc =
  [ Branch.make ~pc:curr_pc ~value:(f ~pc:curr_pc x) ]

let delayed_eval2 f x y ~curr_pc =
  [ Branch.make ~pc:curr_pc ~value:(f ~pc:curr_pc x y) ]

let reduce = delayed_eval FOSolver.reduce_expr
let resolve_loc = delayed_eval FOSolver.resolve_loc_name

let entails =
  let entails ~pc lhs rhs =
    let temp_pc = Pc.extend pc lhs in
    FOSolver.check_entailment ~pc:temp_pc rhs
  in
  delayed_eval2 entails

let check_sat = delayed_eval FOSolver.sat

let is_always_true (f : Formula.t) ~curr_pc =
  FOSolver.check_entailment ~pc:curr_pc f

let assert_ (f : Formula.t) exn ~curr_pc =
  if FOSolver.check_entailment ~pc:curr_pc f then
    [ Branch.make ~pc:curr_pc ~value:() ]
  else raise exn

let has_type (e : Expr.t) (t : Type.t) ~curr_pc =
  let ret value = [ Branch.make ~pc:curr_pc ~value ] in
  match FOSolver.resolve_type ~pc:curr_pc e with
  | Some t' when Type.equal t t' -> ret true
  | _ -> ret false

let assert_type (e : Expr.t) (t : Type.t) exn ~curr_pc =
  match FOSolver.resolve_type ~pc:curr_pc e with
  | Some t' when Type.equal t t' -> [ Branch.make ~pc:curr_pc ~value:() ]
  | _ -> raise exn

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) = map
end
