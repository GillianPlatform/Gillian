(** {b GIL logic assertions}. *)
type t = TypeDef__.assertion =
  | Emp  (** Empty heap             *)
  | Star of t * t  (** Separating conjunction *)
  | Pred of string * Expr.t list  (** Predicates             *)
  | Pure of Formula.t  (** Pure formula           *)
  | Types of (Expr.t * Type.t) list  (** Typing assertion       *)
  | GA of string * Expr.t list * Expr.t list  (** Core assertion         *)
[@@deriving eq]

let to_yojson = TypeDef__.assertion_to_yojson
let of_yojson = TypeDef__.assertion_of_yojson

let compare x y =
  let cmp = Stdlib.compare in
  match (x, y) with
  | Pure (Eq (PVar x, _)), Pure (Eq (PVar y, _)) -> cmp x y
  | Pure (Eq (PVar _, _)), _ -> -1
  | _, Pure (Eq (PVar _, _)) -> 1
  | Pure _, Pure _ -> cmp x y
  | Pure _, _ -> -1
  | _, Pure _ -> 1
  | Types _, Types _ -> cmp x y
  | Types _, _ -> -1
  | _, Types _ -> 1
  | _, _ -> cmp x y

let prioritise (a1 : t) (a2 : t) =
  let lloc_aloc_pvar_lvar e1 e2 =
    match ((e1 : Expr.t), (e2 : Expr.t)) with
    | Lit (Loc _), Lit (Loc _) -> 0
    | Lit (Loc _), _ -> -1
    | _, Lit (Loc _) -> 1
    | ALoc _, ALoc _ -> 0
    | ALoc _, _ -> -1
    | _, ALoc _ -> 0
    | PVar _, PVar _ -> 0
    | PVar _, _ -> -1
    | _, PVar _ -> 1
    | LVar v, LVar v' -> (
        match (Names.is_spec_var_name v, Names.is_spec_var_name v') with
        | true, true -> 0
        | true, false -> -1
        | false, true -> 1
        | false, false -> Stdlib.compare e1 e2)
    | _, _ -> Stdlib.compare e1 e2
  in

  match (a1, a2) with
  | Types [ (e, _) ], Types [ (e', _) ] -> lloc_aloc_pvar_lvar e e'
  | Types _, _ -> -1
  | _, Types _ -> 1
  | Pred _, _ -> 1
  | _, Pred _ -> -1
  | _, _ -> Stdlib.compare a1 a2

module MyAssertion = struct
  type nonrec t = t

  let compare = Stdlib.compare
end

module Set = Set.Make (MyAssertion)

(** Deprecated, use {!Visitors.endo} instead. *)
let rec map
    (f_a_before : (t -> t * bool) option)
    (f_a_after : (t -> t) option)
    (f_e : (Expr.t -> Expr.t) option)
    (f_p : (Formula.t -> Formula.t) option)
    (a : t) : t =
  (* Map recursively to assertions and expressions *)
  let map_a = map f_a_before f_a_after f_e f_p in
  let map_e = Option.value ~default:(fun x -> x) f_e in
  let map_p = Option.value ~default:(Formula.map None None (Some map_e)) f_p in
  let f_a_before = Option.value ~default:(fun x -> (x, true)) f_a_before in
  let f_a_after = Option.value ~default:(fun x -> x) f_a_after in
  let a', recurse = f_a_before a in

  if not recurse then a'
  else
    let a'' =
      match a' with
      | Star (a1, a2) -> Star (map_a a1, map_a a2)
      | Emp -> Emp
      | Pred (s, le) -> Pred (s, List.map map_e le)
      | Pure form -> Pure (map_p form)
      | Types lt -> Types (List.map (fun (exp, typ) -> (map_e exp, typ)) lt)
      | GA (x, es1, es2) -> GA (x, List.map map_e es1, List.map map_e es2)
    in
    f_a_after a''

(* Get all the logical expressions of --a-- that denote a list
   and are not logical variables *)
let list_lexprs (a : t) : Expr.Set.t =
  Formula.list_lexprs_collector#visit_assertion () a

(* Get all the logical variables in --a-- *)
let lvars (a : t) : SS.t =
  Visitors.Collectors.lvar_collector#visit_assertion SS.empty a

(* Get all the program variables in --a-- *)
let pvars (a : t) : SS.t =
  Visitors.Collectors.pvar_collector#visit_assertion () a

(* Get all the abstract locations in --a-- *)
let alocs (a : t) : SS.t =
  Visitors.Collectors.aloc_collector#visit_assertion () a

(* Get all the concrete locations in [a] *)
let clocs (a : t) : SS.t =
  Visitors.Collectors.cloc_collector#visit_assertion () a

(* Get all the concrete locations in [a] *)
let locs (a : t) : SS.t =
  Visitors.Collectors.loc_collector#visit_assertion SS.empty a

(* Get all the variables in [a] *)
let vars (a : t) : SS.t =
  Visitors.Collectors.loc_collector#visit_assertion SS.empty a

(* Returns a list with the names of the predicates that occur in --a-- *)
let pred_names (a : t) : string list =
  let collector =
    object
      inherit [_] Visitors.reduce
      inherit Visitors.Utils.non_ordered_list_monoid
      method! visit_Pred () name _ = [ name ]
    end
  in
  collector#visit_assertion () a

(* Returns a list with the pure assertions that occur in --a-- *)
let pure_asrts (a : t) : Formula.t list =
  let collector =
    object
      inherit [_] Visitors.reduce
      inherit Visitors.Utils.non_ordered_list_monoid
      method! visit_Pure () f = [ f ]
    end
  in
  collector#visit_assertion () a

(* Returns a list with the simple assertions that occur in --a-- *)
let rec simple_asrts (a : t) : t list =
  match a with
  | Emp -> []
  | Star (a1, a2) -> simple_asrts a1 @ simple_asrts a2
  | _ -> [ a ]

(* Check if --a-- is a pure assertion *)
let rec is_pure_asrt (a : t) : bool =
  match a with
  | Pred _ | GA _ -> false
  | Star (a1, a2) -> is_pure_asrt a1 && is_pure_asrt a2
  | _ -> true

(* Check if --a-- is a pure assertion & non-recursive assertion.
   It assumes that only pure assertions are universally quantified *)
let is_pure_non_rec_asrt (a : t) : bool =
  match a with
  | Pure _ | Types _ | Emp -> true
  | _ -> false

(* Eliminate LStar and LTypes assertions.
   LTypes disappears. LStar is replaced by LAnd.
   This function expects its argument to be a PURE assertion. *)
let make_pure (a : t) : Formula.t =
  let s_asrts = simple_asrts a in
  let all_pure = List.for_all is_pure_non_rec_asrt s_asrts in
  if all_pure then
    let fs =
      List.map
        (fun a ->
          match a with
          | Pure f -> f
          | _ -> raise (Failure "DEATH. make_pure"))
        s_asrts
    in
    Formula.conjunct fs
  else raise (Failure "DEATH. make_pure")

let rec full_pp fmt a =
  match a with
  | Star (a1, a2) -> Fmt.pf fmt "%a *@ %a" full_pp a1 full_pp a2
  | Emp -> Fmt.string fmt "emp"
  | Pred (name, params) ->
      Fmt.pf fmt "@[<h>%s(%a)@]" name
        (Fmt.list ~sep:Fmt.comma Expr.full_pp)
        params
  | Types tls ->
      let pp_tl f (e, t) = Fmt.pf f "%a : %s" Expr.full_pp e (Type.str t) in
      Fmt.pf fmt "types(@[%a@])" (Fmt.list ~sep:Fmt.comma pp_tl) tls
  | Pure f -> Formula.full_pp fmt f
  | GA (a, ins, outs) ->
      let pp_e_l = Fmt.list ~sep:Fmt.comma Expr.full_pp in
      Fmt.pf fmt "@[<h><%s>(%a; %a)@]" a pp_e_l ins pp_e_l outs

(** GIL logic assertions *)
let rec pp fmt a =
  match a with
  | Star (a1, a2) -> Fmt.pf fmt "%a *@ %a" pp a1 pp a2
  | Emp -> Fmt.string fmt "emp"
  | Pred (name, params) ->
      Fmt.pf fmt "@[<h>%s(%a)@]" name (Fmt.list ~sep:Fmt.comma Expr.pp) params
  | Types tls ->
      let pp_tl f (e, t) = Fmt.pf f "%a : %s" Expr.pp e (Type.str t) in
      Fmt.pf fmt "types(@[%a@])" (Fmt.list ~sep:Fmt.comma pp_tl) tls
  | Pure f -> Formula.pp fmt f
  | GA (a, ins, outs) ->
      let pp_e_l = Fmt.list ~sep:Fmt.comma Expr.pp in
      Fmt.pf fmt "@[<h><%s>(%a; %a)@]" a pp_e_l ins pp_e_l outs

let star (asses : t list) : t =
  List.fold_left
    (fun ac a ->
      if not (a = Emp) then if ac = Emp then a else Star (ac, a) else ac)
    Emp asses

let subst_clocs (subst : string -> Expr.t) (a : t) : t =
  map None None
    (Some (Expr.subst_clocs subst))
    (Some (Formula.subst_clocs subst))
    a

let subst_expr_for_expr ~(to_subst : Expr.t) ~(subst_with : Expr.t) (a : t) : t
    =
  map None None
    (Some (Expr.subst_expr_for_expr ~to_subst ~subst_with))
    (Some (Formula.subst_expr_for_expr ~to_subst ~subst_with))
    a

module Infix = struct
  let ( ** ) a b =
    match (a, b) with
    | Pure True, x | x, Pure True | Emp, x | x, Emp -> x
    | _ -> Star (a, b)
end

let pvars_to_lvars (a : t) : t =
  let ff = Formula.pvars_to_lvars in
  let fe = Expr.pvars_to_lvars in
  map None None (Some fe) (Some ff) a
