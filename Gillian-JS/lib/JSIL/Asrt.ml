module SSubst = Gillian.Symbolic.Subst
open Gillian.Gil_syntax

(** {b JSIL logic assertions}. *)
type t =
  | Emp  (** Empty heap             *)
  | Star of t * t  (** Separating conjunction *)
  | PointsTo of Expr.t * Expr.t * Expr.t  (** Heap cell assertion    *)
  | MetaData of Expr.t * Expr.t  (** MetaData               *)
  | Pred of string * Expr.t list  (** Predicates             *)
  | EmptyFields of Expr.t * Expr.t  (** emptyFields assertion  *)
  | Pure of Expr.t  (** Pure formula           *)
  | Types of (Expr.t * Type.t) list  (** Typing assertion       *)

let compare x y =
  let cmp = Stdlib.compare in
  match (x, y) with
  | Pure (BinOp (PVar x, Equal, _)), Pure (BinOp (PVar y, Equal, _)) -> cmp x y
  | Pure (BinOp (PVar _, Equal, _)), _ -> -1
  | _, Pure (BinOp (PVar _, Equal, _)) -> 1
  | PointsTo _, PointsTo _ -> cmp x y
  | PointsTo _, _ -> -1
  | _, PointsTo _ -> 1
  | MetaData _, MetaData _ -> cmp x y
  | MetaData _, _ -> -1
  | _, MetaData _ -> 1
  | EmptyFields _, EmptyFields _ -> cmp x y
  | EmptyFields _, _ -> -1
  | _, EmptyFields _ -> 1
  | Pure _, Pure _ -> cmp x y
  | Pure _, _ -> -1
  | _, Pure _ -> 1
  | Types _, Types _ -> cmp x y
  | Types _, _ -> -1
  | _, Types _ -> 1
  | Pred _, Pred _ -> cmp x y
  | Pred _, _ -> -1
  | _, Pred _ -> 1
  | _, _ -> cmp x y

module MyAssertion = struct
  type nonrec t = t

  let compare = Stdlib.compare
end

module Set = Set.Make (MyAssertion)

let rec pp fmt (a : t) : unit =
  match a with
  | Star (a1, a2) -> Fmt.pf fmt "%a *@ %a" pp a1 pp a2
  | PointsTo (e1, e2, e3) ->
      Fmt.pf fmt "((%a, %a) -> %a)" Expr.pp e1 Expr.pp e2 Expr.pp e3
  | Emp -> Fmt.string fmt "emp"
  (* x(y1, ..., yn) *)
  | Pred (name, params) ->
      Fmt.pf fmt "%s(%a)" name (Fmt.list ~sep:(Fmt.any ", ") Expr.pp) params
  (* types(e1:t1, ..., en:tn) *)
  | Types type_list ->
      let pp_pair =
        Fmt.pair ~sep:(Fmt.any " : ") Expr.pp (Fmt.of_to_string Type.str)
      in
      Fmt.pf fmt "types(@[%a@])" (Fmt.list ~sep:Fmt.comma pp_pair) type_list
  | EmptyFields (obj, domain) ->
      Fmt.pf fmt "empty_fields(%a : %a)" Expr.pp obj Expr.pp domain
  (* MetaData (e1, e2) *)
  | MetaData (e1, e2) -> Fmt.pf fmt "MetaData (%a, %a)" Expr.pp e1 Expr.pp e2
  (* Pure *)
  | Pure f -> Expr.pp fmt f

let full_pp = pp
let pp_list = Fmt.list ~sep:(Fmt.any "  ") pp

let star (asses : t list) : t =
  List.fold_left
    (fun ac a ->
      if not (a = Emp) then if ac = Emp then a else Star (ac, a) else ac)
    Emp asses
