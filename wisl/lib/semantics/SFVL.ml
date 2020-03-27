(** GIL symbolic field-value list *)
open Gillian.Utils.Containers

module Expr = Gillian.Gil_syntax.Expr
module SSubst = Gillian.Symbolic.Subst
open Gillian.Gil_syntax
module L = Gillian.Logging

type field_name = Expr.t

type field_value = Expr.t

(* Definition *)
type t = field_value Expr.Map.t

let gsbsts = Expr.substitutables

(* Printing *)
let pp ft sfvl =
  let open Fmt in
  (iter_bindings ~sep:comma Expr.Map.iter
     (hbox (parens (pair ~sep:(any " :") Expr.pp Expr.pp))))
    ft sfvl

(*************************************)
(** Field Value List Functions      **)

(*************************************)

(* Map functions to be reused *)

let add fn fv = Expr.Map.add fn fv

let empty = Expr.Map.empty

let field_names sfvl =
  let result, _ = List.split (Expr.Map.bindings sfvl) in
  result

let fold f sfvl ac = Expr.Map.fold f sfvl ac

let get fn sfvl = Option.map (fun fv -> fv) (Expr.Map.find_opt fn sfvl)

let is_empty sfvl = sfvl = empty

let iter f sfvl = Expr.Map.iter f sfvl

let partition f sfvl = Expr.Map.partition f sfvl

let remove = Expr.Map.remove

let union =
  Expr.Map.union (fun k fvl fvr ->
      L.(
        verbose (fun m ->
            m
              "WARNING: SFVL.union: merging with field in both lists (%s: %s \
               and %s), choosing left."
              ((Fmt.to_to_string Expr.pp) k)
              ((Fmt.to_to_string Expr.pp) fvl)
              ((Fmt.to_to_string Expr.pp) fvr)));
      Some fvl)

let to_list fv_list = fold (fun f v ac -> (f, v) :: ac) fv_list []

let of_list l =
  let add_several l fvl = List.fold_left (fun ac (a, b) -> add a b ac) fvl l in
  add_several l empty

(** Gets a first key-value pair that satisfies a predicate *)
let get_first (f : field_name -> bool) (sfvl : t) :
    (field_name * field_value) option =
  Expr.Map.find_first_opt f sfvl

(** Adds by testing something equal is not already there *)
let add_with_test
    ~(equality_test : field_name -> field_name -> bool)
    (ofs : field_name)
    (new_val : field_value)
    (sfvl : t) =
  let actual_ofs =
    Option.value ~default:ofs
      (Option.map fst (get_first (equality_test ofs) sfvl))
  in
  add actual_ofs new_val sfvl

(** Returns the logical variables occuring in --sfvl-- *)
let lvars (sfvl : t) : SS.t =
  let gllv = Expr.lvars in
  Expr.Map.fold
    (fun e_field e_val ac -> SS.union ac (SS.union (gllv e_field) (gllv e_val)))
    sfvl SS.empty

(** Returns the abstract locations occuring in --sfvl-- *)
let alocs (sfvl : t) : SS.t =
  Expr.Map.fold
    (fun e_field e_val ac ->
      SS.union ac (SS.union (Expr.alocs e_field) (Expr.alocs e_val)))
    sfvl SS.empty

(* Substitution *)
let substitution (subst : SSubst.t) (partial : bool) (fv_list : t) : t =
  let f_subst = SSubst.subst_in_expr subst ~partial in
  Expr.Map.fold
    (fun le_field le_val ac ->
      let sf = f_subst le_field in
      let sv = f_subst le_val in
      Expr.Map.add sf sv ac)
    fv_list Expr.Map.empty

let assertions_with_constructor ~constr loc sfvl =
  List.rev
    (Expr.Map.fold (fun field value ac -> constr loc field value :: ac) sfvl [])
