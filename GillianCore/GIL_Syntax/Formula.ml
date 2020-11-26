open Containers

type t = TypeDef__.formula =
  | True  (** Logical true *)
  | False  (** Logical false *)
  | Not     of t  (** Logical negation *)
  | And     of t * t  (** Logical conjunction *)
  | Or      of t * t  (** Logical disjunction *)
  | Eq      of Expr.t * Expr.t  (** Expression equality *)
  | Less    of Expr.t * Expr.t  (** Expression less-than for numbers *)
  | LessEq  of Expr.t * Expr.t  (** Expression less-than-or-equal for numbers *)
  | StrLess of Expr.t * Expr.t  (** Expression less-than for strings *)
  | SetMem  of Expr.t * Expr.t  (** Set membership *)
  | SetSub  of Expr.t * Expr.t  (** Set subsetness *)
  | ForAll  of (string * Type.t option) list * t  (** Forall *)

let compare = Stdlib.compare

let of_bool b = if b then True else False

module MyFormula = struct
  type nonrec t = t

  let compare = Stdlib.compare
end

module Set = Set.Make (MyFormula)

(** Apply function f to the logic expressions in an assertion, recursively when f_a returns (new_asrt, true). *)
let rec map
    (f_a_before : (t -> t * bool) option)
    (f_a_after : (t -> t) option)
    (f_e : (Expr.t -> Expr.t) option)
    (a : t) : t =
  (* Map recursively to assertions and expressions *)
  let map_a = map f_a_before f_a_after f_e in
  let map_e = Option.value ~default:(fun x -> x) f_e in
  let f_a_before = Option.value ~default:(fun x -> (x, true)) f_a_before in
  let f_a_after = Option.value ~default:(fun x -> x) f_a_after in
  let a', recurse = f_a_before a in

  if not recurse then a'
  else
    let a'' =
      match a' with
      | And (a1, a2)     -> And (map_a a1, map_a a2)
      | Or (a1, a2)      -> Or (map_a a1, map_a a2)
      | Not a            -> Not (map_a a)
      | True             -> True
      | False            -> False
      | Eq (e1, e2)      -> Eq (map_e e1, map_e e2)
      | Less (e1, e2)    -> Less (map_e e1, map_e e2)
      | LessEq (e1, e2)  -> LessEq (map_e e1, map_e e2)
      | StrLess (e1, e2) -> StrLess (map_e e1, map_e e2)
      | SetMem (e1, e2)  -> SetMem (map_e e1, map_e e2)
      | SetSub (e1, e2)  -> SetSub (map_e e1, map_e e2)
      | ForAll (bt, a)   -> ForAll (bt, map_a a)
    in
    f_a_after a''

let rec map_opt
    (f_a_before : (t -> t option * bool) option)
    (f_a_after : (t -> t) option)
    (f_e : (Expr.t -> Expr.t option) option)
    (a : t) : t option =
  (* Map recursively to assertions and expressions *)
  let map_a = map_opt f_a_before f_a_after f_e in
  let map_e = Option.value ~default:(fun x -> Some x) f_e in
  let f_a_before = Option.value ~default:(fun x -> (Some x, true)) f_a_before in
  let f_a_after = Option.value ~default:(fun x -> x) f_a_after in
  let a', recurse = f_a_before a in

  let aux_a_single a f =
    let ma = map_a a in
    Option.map f ma
  in

  let aux_a_double a1 a2 f =
    let ma1, ma2 = (map_a a1, map_a a2) in
    if ma1 = None || ma2 = None then None
    else Some (f (Option.get ma1) (Option.get ma2))
  in

  let aux_e e1 e2 f =
    let me1, me2 = (map_e e1, map_e e2) in
    if me1 = None || me2 = None then None
    else Some (f (Option.get me1) (Option.get me2))
  in

  match a' with
  | None    -> None
  | Some a' ->
      if not recurse then Some a'
      else
        let a'' =
          match a' with
          | And (a1, a2)     -> aux_a_double a1 a2 (fun a1 a2 -> And (a1, a2))
          | Or (a1, a2)      -> aux_a_double a1 a2 (fun a1 a2 -> Or (a1, a2))
          | Not a            -> aux_a_single a (fun a -> Not a)
          | True             -> Some True
          | False            -> Some False
          | Eq (e1, e2)      -> aux_e e1 e2 (fun e1 e2 -> Eq (e1, e2))
          | Less (e1, e2)    -> aux_e e1 e2 (fun e1 e2 -> Less (e1, e2))
          | LessEq (e1, e2)  -> aux_e e1 e2 (fun e1 e2 -> LessEq (e1, e2))
          | StrLess (e1, e2) -> aux_e e1 e2 (fun e1 e2 -> StrLess (e1, e2))
          | SetMem (e1, e2)  -> aux_e e1 e2 (fun e1 e2 -> SetMem (e1, e2))
          | SetSub (e1, e2)  -> aux_e e1 e2 (fun e1 e2 -> SetSub (e1, e2))
          | ForAll (bt, a)   -> aux_a_single a (fun a -> ForAll (bt, a))
        in
        Option.map f_a_after a''

let rec fold
    (feo : (Expr.t -> 'a) option)
    (f_ac : t -> 'b -> 'b -> 'a list -> 'a)
    (f_state : (t -> 'b -> 'b) option)
    (state : 'b)
    (asrt : t) : 'a =
  let new_state = (Option.value ~default:(fun _ x -> x) f_state) asrt state in
  let fold_a = fold feo f_ac f_state new_state in
  let f_ac = f_ac asrt new_state state in
  let fes les = Option.fold ~none:[] ~some:(fun fe -> List.map fe les) feo in

  match asrt with
  | True | False -> f_ac []
  | Eq (le1, le2)
  | Less (le1, le2)
  | LessEq (le1, le2)
  | StrLess (le1, le2)
  | SetMem (le1, le2)
  | SetSub (le1, le2) -> f_ac (fes [ le1; le2 ])
  | And (a1, a2) -> f_ac [ fold_a a1; fold_a a2 ]
  | Or (a1, a2) -> f_ac [ fold_a a1; fold_a a2 ]
  | Not a -> f_ac [ fold_a a ]
  | ForAll (_, a) -> f_ac [ fold_a a ]

(* Get all the logical variables in --a-- *)
let lvars (a : t) : SS.t =
  let fe_ac (le : Expr.t) _ _ (ac : string list list) : string list =
    match le with
    | Expr.LVar x -> [ x ]
    | _           -> List.concat ac
  in
  let fe = Expr.fold fe_ac None None in
  let fp_ac (a : t) _ _ (ac : string list list) : string list =
    match (a : t) with
    | ForAll (bt, _) ->
        (* Quantified variables need to be excluded *)
        let binders, _ = List.split bt in
        let ac_vars = SS.of_list (List.concat ac) in
        let binder_vars = SS.of_list binders in
        SS.elements (SS.diff ac_vars binder_vars)
    | _              -> List.concat ac
  in
  SS.of_list (fold (Some fe) fp_ac None None a)

(* Get all the program variables in --a-- *)
let pvars (a : t) : SS.t =
  let fe_ac le _ _ ac =
    match le with
    | Expr.PVar x -> [ x ]
    | _           -> List.concat ac
  in
  let fe = Expr.fold fe_ac None None in
  let f_ac _ _ _ ac = List.concat ac in
  SS.of_list (fold (Some fe) f_ac None None a)

(* Get all the abstract locations in --a-- *)
let alocs (a : t) : SS.t =
  let fe_ac le _ _ ac =
    match le with
    | Expr.ALoc l -> l :: List.concat ac
    | _           -> List.concat ac
  in
  let fe = Expr.fold fe_ac None None in
  let f_ac _ _ _ ac = List.concat ac in
  SS.of_list (fold (Some fe) f_ac None None a)

(* Get all the concrete locations in [a] *)
let clocs (a : t) : SS.t =
  let fe_ac le _ _ ac =
    match le with
    | Expr.Lit (Loc l) -> l :: List.concat ac
    | _                -> List.concat ac
  in
  let fe = Expr.fold fe_ac None None in
  let f_ac _ _ _ ac = List.concat ac in
  SS.of_list (fold (Some fe) f_ac None None a)

(* Get all the locations in [a] *)
let locs (a : t) : SS.t =
  let fe_ac le _ _ ac =
    match le with
    | Expr.ALoc l | Lit (Loc l) -> l :: List.concat ac
    | _                         -> List.concat ac
  in
  let fe = Expr.fold fe_ac None None in
  let f_ac _ _ _ ac = List.concat ac in
  SS.of_list (fold (Some fe) f_ac None None a)

let get_print_info (a : t) = (pvars a, lvars a, locs a)

(* Get all the logical expressions of --a-- of the form (Lit (LList lst)) and (EList lst)  *)
let lists (a : t) : Expr.t list =
  let f_ac _ _ _ ac = List.concat ac in
  let fe = Expr.lists in
  fold (Some fe) f_ac None None a

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
  fold (Some fe) f_ac None None a

let rec push_in_negations_off (a : t) : t =
  let f_off = push_in_negations_off in
  let f_on = push_in_negations_on in
  match a with
  | And (a1, a2)   -> And (f_off a1, f_off a2)
  | Or (a1, a2)    -> Or (f_off a1, f_off a2)
  | Not a1         -> f_on a1
  | ForAll (bt, a) -> ForAll (bt, f_off a)
  | _              -> a

and push_in_negations_on (a : t) : t =
  let f_off = push_in_negations_off in
  let f_on = push_in_negations_on in
  match a with
  | And (a1, a2) -> Or (f_on a1, f_on a2)
  | Or (a1, a2)  -> And (f_on a1, f_on a2)
  | True         -> False
  | False        -> True
  | Not a        -> f_off a
  | _            -> Not a

and push_in_negations (a : t) : t = push_in_negations_off a

let rec split_conjunct_formulae (f : t) : t list =
  match f with
  | And (f1, f2)      -> split_conjunct_formulae f1 @ split_conjunct_formulae f2
  | Not (Or (f1, f2)) -> split_conjunct_formulae (And (Not f1, Not f2))
  | f                 -> [ f ]

(****** Pretty Printing *********)

(* To avoid code redundancy, we write a pp function parametric on the Expr printing function.
    We then instantiate the function with Expr.pp and Expr.full_pp *)
let rec pp_parametric pp_expr fmt f =
  let pp_var_with_type fmt (x, t_opt) =
    Fmt.pf fmt "%s%a" x
      (Fmt.option (fun fm t -> Fmt.pf fm " : %s" (Type.str t)))
      t_opt
  in
  let pp = pp_parametric pp_expr in
  match f with
  (* a1 /\ a2 *)
  | And (a1, a2) -> Fmt.pf fmt "(%a /\\@ %a)" pp a1 pp a2
  (* a1 \/ a2 *)
  | Or (a1, a2) -> Fmt.pf fmt "(%a \\/@ %a)" pp a1 pp a2
  (* ! a *)
  | Not a -> Fmt.pf fmt "(! %a)" pp a
  (* true *)
  | True -> Fmt.string fmt "True"
  (* false *)
  | False -> Fmt.string fmt "False"
  (* e1 == e2 *)
  | Eq (e1, e2) -> Fmt.pf fmt "(%a == %a)" pp_expr e1 pp_expr e2
  (* e1 << e2 *)
  | Less (e1, e2) -> Fmt.pf fmt "(%a <# %a)" pp_expr e1 pp_expr e2
  (* e1 <<= e2 *)
  | LessEq (e1, e2) -> Fmt.pf fmt "(%a <=# %a)" pp_expr e1 pp_expr e2
  (* e1 <s< e2 *)
  | StrLess (e1, e2) -> Fmt.pf fmt "(%a s<# %a)" pp_expr e1 pp_expr e2
  (* forall vars . a *)
  | ForAll (lvars, a) ->
      Fmt.pf fmt "(forall %a . %a)"
        (Fmt.list ~sep:Fmt.comma pp_var_with_type)
        lvars pp a
  (* e1 --e-- e2 *)
  | SetMem (e1, e2) -> Fmt.pf fmt "(%a --e-- %a)" pp_expr e1 pp_expr e2
  (* e1 --s-- e2 *)
  | SetSub (e1, e2) -> Fmt.pf fmt "(%a --s-- %a)" pp_expr e1 pp_expr e2

let pp = pp_parametric Expr.pp

let full_pp = pp_parametric Expr.full_pp

let rec lift_logic_expr (e : Expr.t) : (t * t) option =
  let f = lift_logic_expr in
  match e with
  | LVar _ | PVar _ -> Some (Eq (e, Lit (Bool true)), Eq (e, Lit (Bool false)))
  | Lit (Bool true) -> Some (True, False)
  | Lit (Bool false) -> Some (False, True)
  | BinOp (e1, Equal, e2) ->
      let a = Eq (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, FLessThan, e2) ->
      let a = Less (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, SLessThan, e2) ->
      let a = StrLess (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, FLessThanEqual, e2) ->
      let a = LessEq (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, BSetMem, e2) ->
      let a = SetMem (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, BSetSub, e2) ->
      let a = SetSub (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, BAnd, e2) -> (
      match (f e1, f e2) with
      | Some (a1, na1), Some (a2, na2) -> Some (And (a1, a2), Or (na1, na2))
      | _ -> None)
  | BinOp (e1, BOr, e2) -> (
      match (f e1, f e2) with
      | Some (a1, na1), Some (a2, na2) -> Some (Or (a1, a2), And (na1, na2))
      | _ -> None)
  | UnOp (UNot, e') -> Option.map (fun (a, na) -> (na, a)) (f e')
  | _ -> None

let rec to_expr (a : t) : Expr.t option =
  let f = to_expr in
  match a with
  | True               -> Some (Expr.Lit (Bool true))
  | False              -> Some (Expr.Lit (Bool false))
  | Not a'             -> Option.map (fun a -> Expr.UnOp (UnOp.UNot, a)) (f a')
  | And (a1, a2)       -> (
      match (f a1, f a2) with
      | Some le1, Some le2 -> Some (Expr.BinOp (le1, BinOp.BAnd, le2))
      | _                  -> None)
  | Or (a1, a2)        -> (
      match (f a1, f a2) with
      | Some le1, Some le2 -> Some (Expr.BinOp (le1, BinOp.BAnd, le2))
      | _                  -> None)
  | ForAll _           -> None
  | Eq (le1, le2)      -> Some (Expr.BinOp (le1, BinOp.Equal, le2))
  | Less (le1, le2)    -> Some (Expr.BinOp (le1, BinOp.FLessThan, le2))
  | LessEq (le1, le2)  -> Some (Expr.BinOp (le1, BinOp.FLessThanEqual, le2))
  | StrLess (le1, le2) -> Some (Expr.BinOp (le1, BinOp.SLessThan, le2))
  | SetMem (le1, le2)  -> Some (Expr.BinOp (le1, BinOp.BSetMem, le2))
  | SetSub (le1, le2)  -> Some (Expr.BinOp (le1, BinOp.BSetSub, le2))

let rec conjunct (asrts : t list) : t =
  match asrts with
  | []           -> True
  | [ a ]        -> a
  | a :: r_asrts -> And (a, conjunct r_asrts)

let rec disjunct (asrts : t list) : t =
  match asrts with
  | []           -> True
  | [ a ]        -> a
  | a :: r_asrts -> Or (a, disjunct r_asrts)

let subst_expr_for_expr ~(to_subst : Expr.t) ~(subst_with : Expr.t) (a : t) : t
    =
  map None None (Some (Expr.subst_expr_for_expr ~to_subst ~subst_with)) a

let subst_clocs (subst : string -> Expr.t) (f : t) : t =
  map None None (Some (Expr.subst_clocs subst)) f

let rec get_disjuncts (fo : t) : t list =
  (* Printf.printf "I am getting disjuncts every day!!\n"; *)
  match fo with
  | Or (fo1, fo2) ->
      (* Printf.printf "More than one disjunct!\n"; *)
      get_disjuncts fo1 @ get_disjuncts fo2
  | _             -> [ fo ]

let strings_and_numbers =
  let v =
    object
      inherit [_] Visitors.reduce

      inherit Visitors.Utils.two_list_monoid

      method! visit_Num _ n = ([], [ n ])

      method! visit_String _ s = ([ s ], [])
    end
  in
  v#visit_formula ()

module Infix = struct
  let fnot a =
    match a with
    | True  -> False
    | False -> True
    | Not x -> x
    | _     -> Not a

  let forall params f = ForAll (params, f)

  let ( #== ) a b =
    match (a, b) with
    | Expr.Lit la, Expr.Lit lb -> of_bool (Literal.equal la lb)
    | a, b when a == b -> True
    | _ -> Eq (a, b)

  let ( #|| ) a b =
    match (a, b) with
    | True, _ | _, True   -> True
    | False, f | f, False -> f
    | _                   -> Or (a, b)

  let ( #&& ) a b =
    match (a, b) with
    | True, f | f, True   -> f
    | False, _ | _, False -> False
    | _                   -> And (a, b)

  let ( #< ) a b =
    match (a, b) with
    | Expr.Lit (Num x), Expr.Lit (Num y) -> of_bool (x < y)
    | _ -> Less (a, b)

  let ( #<= ) a b =
    match (a, b) with
    | Expr.Lit (Num x), Expr.Lit (Num y) -> of_bool (x <= y)
    | _ -> LessEq (a, b)

  let ( #> ) a b =
    match (a, b) with
    | Expr.Lit (Num x), Expr.Lit (Num y) -> of_bool (x > y)
    | _ -> fnot a #<= b

  let ( #>= ) a b =
    match (a, b) with
    | Expr.Lit (Num x), Expr.Lit (Num y) -> of_bool (x >= y)
    | _ -> fnot a #< b

  let ( #=> ) fa fb = (fnot fa) #|| fb
end
