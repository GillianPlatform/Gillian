open Containers

(** {b GIL logic assertions}. *)
type t = TypeDef__.assertion =
  | Emp  (** Empty heap             *)
  | Star  of t * t  (** Separating conjunction *)
  | Pred  of string * Expr.t list  (** Predicates             *)
  | Pure  of Formula.t  (** Pure formula           *)
  | Types of (Expr.t * Type.t) list  (** Typing assertion       *)
  | GA    of string * Expr.t list * Expr.t list  (** Core assertion         *)

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
    | Lit (Loc _), _           -> -1
    | _, Lit (Loc _)           -> 1
    | ALoc _, ALoc _           -> 0
    | ALoc _, _                -> -1
    | _, ALoc _                -> 0
    | PVar _, PVar _           -> 0
    | PVar _, _                -> -1
    | _, PVar _                -> 1
    | LVar v, LVar v'          -> (
        match (Names.is_spec_var_name v, Names.is_spec_var_name v') with
        | true, true   -> 0
        | true, false  -> -1
        | false, true  -> 1
        | false, false -> Stdlib.compare e1 e2 )
    | _, _                     -> Stdlib.compare e1 e2
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

(** Deprecated, use {!Visitors.map} instead. *)
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
      | Star (a1, a2)    -> Star (map_a a1, map_a a2)
      | Emp              -> Emp
      | Pred (s, le)     -> Pred (s, List.map map_e le)
      | Pure form        -> Pure (map_p form)
      | Types lt         -> Types
                              (List.map (fun (exp, typ) -> (map_e exp, typ)) lt)
      | GA (x, es1, es2) -> GA (x, List.map map_e es1, List.map map_e es2)
    in
    f_a_after a''

let rec fold
    (feo : (Expr.t -> 'a) option)
    (fpo : (Formula.t -> 'a) option)
    (f_ac : t -> 'b -> 'b -> 'a list -> 'a)
    (f_state : (t -> 'b -> 'b) option)
    (state : 'b)
    (asrt : t) : 'a =
  let new_state = (Option.value ~default:(fun _ x -> x) f_state) asrt state in
  let fold_a = fold feo fpo f_ac f_state new_state in
  let f_ac = f_ac asrt new_state state in
  let fes les = Option.fold ~some:(fun fe -> List.map fe les) ~none:[] feo in
  let fp form = Option.fold ~some:(fun fp -> [ fp form ]) ~none:[] fpo in

  (* Not convinced these are correct *)
  match asrt with
  | Emp              -> f_ac []
  | Pred (_, les)    -> f_ac (fes les)
  | Star (a1, a2)    -> f_ac [ fold_a a1; fold_a a2 ]
  | Pure form        -> f_ac (fp form)
  | Types vts        ->
      let les, _ = List.split vts in
      f_ac (fes les)
  | GA (_, es1, es2) -> f_ac (fes (es1 @ es2))

(* Get all the logical expressions of --a-- of the form (Lit (LList lst)) and (EList lst)  *)
let lists (a : t) : Expr.t list =
  let f_ac _ _ _ ac = List.concat ac in
  let fe = Expr.lists in
  let fp = Formula.fold (Some fe) f_ac None None in
  fold (Some fe) (Some fp) f_ac None None a

(* Get all the logical expressions of --a-- that denote a list
   and are not logical variables *)
let list_lexprs (a : t) : Expr.t list =
  let fe_ac le _ _ ac =
    match le with
    | Expr.Lit (LList _)
    | Expr.EList _
    | Expr.NOp (LstCat, _)
    | Expr.UnOp (Car, _)
    | Expr.UnOp (Cdr, _)
    | Expr.UnOp (LstLen, _) -> le :: List.concat ac
    | _ -> List.concat ac
  in

  let fe = Expr.fold fe_ac None None in
  let f_ac _ _ _ ac = List.concat ac in
  let fp = Formula.fold (Some fe) f_ac None None in
  fold (Some fe) (Some fp) f_ac None None a

(* Get all the logical variables in --a-- *)
let lvars (a : t) : SS.t =
  let fe_ac (le : Expr.t) _ _ (ac : string list list) : string list =
    match le with
    | Expr.LVar x -> [ x ]
    | _           -> List.concat ac
  in
  let fe = Expr.fold fe_ac None None in
  let fp f = SS.elements (Formula.lvars f) in
  let f_ac _ _ _ ac = List.concat ac in
  SS.of_list (fold (Some fe) (Some fp) f_ac None None a)

(* Get all the program variables in --a-- *)
let pvars (a : t) : SS.t =
  let fe_ac le _ _ ac =
    match le with
    | Expr.PVar x -> [ x ]
    | _           -> List.concat ac
  in
  let fe = Expr.fold fe_ac None None in
  let f_ac _ _ _ ac = List.concat ac in
  let fp = Formula.fold (Some fe) f_ac None None in
  SS.of_list (fold (Some fe) (Some fp) f_ac None None a)

(* Get all the abstract locations in --a-- *)
let alocs (a : t) : SS.t =
  let fe_ac le _ _ ac =
    match le with
    | Expr.ALoc l -> l :: List.concat ac
    | _           -> List.concat ac
  in
  let fe = Expr.fold fe_ac None None in
  let f_ac _ _ _ ac = List.concat ac in
  let fp = Formula.fold (Some fe) f_ac None None in
  SS.of_list (fold (Some fe) (Some fp) f_ac None None a)

(* Get all the concrete locations in [a] *)
let clocs (a : t) : SS.t =
  let fe_ac le _ _ ac =
    match le with
    | Expr.Lit (Loc l) -> l :: List.concat ac
    | _                -> List.concat ac
  in
  let fe = Expr.fold fe_ac None None in
  let f_ac _ _ _ ac = List.concat ac in
  let fp = Formula.fold (Some fe) f_ac None None in
  SS.of_list (fold (Some fe) (Some fp) f_ac None None a)

(* Get all the variables in [a] *)
let vars (a : t) : SS.t =
  let vars = [ alocs a; clocs a; lvars a; pvars a ] in
  List.fold_left SS.union SS.empty vars

(* Returns a list with the names of the predicates that occur in --a-- *)
let pred_names (a : t) : string list =
  let f_ac a _ _ ac =
    match a with
    | Pred (s, _) -> s :: List.concat ac
    | _           -> List.concat ac
  in
  fold None None f_ac None None a

(* Returns a list with the pure assertions that occur in --a-- *)
let pure_asrts (a : t) : Formula.t list =
  let f_ac a _ _ ac =
    match a with
    | Pure form -> form :: List.concat ac
    | _         -> List.concat ac
  in
  fold None None f_ac None None a

(* Returns a list with the pure assertions that occur in --a-- *)
let simple_asrts (a : t) : t list =
  let f_ac a _ _ ac =
    match a with
    | Star _ -> List.concat ac
    | Emp    -> []
    | a      -> [ a ]
  in
  fold None None f_ac None None a

(* Check if --a-- is a pure assertion *)
let is_pure_asrt (a : t) : bool =
  let f_ac a _ _ (ac : bool list) : bool =
    match a with
    | Pred _ | GA _ -> false
    | _             -> List.for_all (fun b -> b) ac
  in
  let aux = fold None None f_ac None None in
  let ret = aux a in
  ret

(* Check if --a-- is a pure assertion & non-recursive assertion.
   It assumes that only pure assertions are universally quantified *)
let is_pure_non_rec_asrt (a : t) : bool =
  match a with
  | Pure _ | Types _ | Emp -> true
  | _                      -> false

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
          | _      -> raise (Failure "DEATH. make_pure"))
        s_asrts
    in
    Formula.conjunct fs
  else raise (Failure "DEATH. make_pure")

(** GIL logic assertions *)
let rec pp fmt a =
  match a with
  | Star (a1, a2)       -> Fmt.pf fmt "%a *@ %a" pp a1 pp a2
  | Emp                 -> Fmt.string fmt "emp"
  | Pred (name, params) ->
      Fmt.pf fmt "@[<h>%s(%a)@]" name (Fmt.list ~sep:Fmt.comma Expr.pp) params
  | Types tls           ->
      let pp_tl f (e, t) = Fmt.pf f "%a : %s" Expr.pp e (Type.str t) in
      Fmt.pf fmt "types(@[%a@])" (Fmt.list ~sep:Fmt.comma pp_tl) tls
  | Pure f              -> Formula.pp fmt f
  | GA (a, ins, outs)   ->
      let pp_e_l = Fmt.list ~sep:Fmt.comma Expr.pp in
      Fmt.pf fmt "@[<h><%s>(%a; %a)@]" a pp_e_l ins pp_e_l outs

let pp_list = Fmt.list ~sep:(Fmt.any "    ") pp

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
