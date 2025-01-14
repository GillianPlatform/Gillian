(** JSIL symbolic field-value list *)

open Containers
module Expr = Gillian.Gil_syntax.Expr
module SSubst = Gillian.Symbolic.Subst
open Gillian.Gil_syntax
open Javert_utils
module L = Logging

type field_name = Expr.t
type field_value = Expr.t [@@deriving yojson]

(* Definition *)
type t = field_value Expr.Map.t [@@deriving yojson]

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

(* WHAT IS THIS? *)
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

(** Gets a first key-value pair that satisfies a predicate *)
let get_first (f : field_name -> bool) (sfvl : t) :
    (field_name * field_value) option =
  Expr.Map.find_first_opt f sfvl

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

let assertions (loc : Expr.t) (sfvl : t) : Asrt.t =
  List.rev
    (Expr.Map.fold
       (fun field value (ac : Asrt.t) ->
         Asrt_utils.points_to ~loc ~field ~value :: ac)
       sfvl [])

(* Substitution *)
let substitution (subst : SSubst.t) (partial : bool) (fv_list : t) : t =
  let f_subst = SSubst.subst_in_expr subst ~partial in
  Expr.Map.fold
    (fun le_field le_val ac ->
      let sf = f_subst le_field in
      let sv = f_subst le_val in
      Expr.Map.add sf sv ac)
    fv_list Expr.Map.empty

(* Selective substitution *)
let selective_substitution (subst : SSubst.t) (partial : bool) (fv_list : t) : t
    =
  let f_subst = SSubst.subst_in_expr subst ~partial in
  Expr.Map.fold
    (fun le_field le_val ac ->
      let sv = f_subst le_val in
      Expr.Map.add le_field sv ac)
    fv_list Expr.Map.empty

(* Correctness of field-value lists *)
let is_well_formed (_ : t) : bool = true

let wf_assertions (sfvl : t) : Expr.t list =
  let props = field_names sfvl in
  let props' = List_utils.cross_product props props (fun x y -> (x, y)) in
  let props' = List.filter (fun (x, y) -> x <> y) props' in
  List.map (fun (x, y) : Expr.t -> UnOp (Not, BinOp (x, Equal, y))) props'
