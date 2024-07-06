type t = TypeDef__.formula =
  | True  (** Logical true *)
  | False  (** Logical false *)
  | Not of t  (** Logical negation *)
  | And of t * t  (** Logical conjunction *)
  | Or of t * t  (** Logical disjunction *)
  | Eq of Expr.t * Expr.t  (** Expression equality *)
  | Impl of t * t  (** Logical implication *)
  | FLess of Expr.t * Expr.t  (** Expression less-than for numbers *)
  | FLessEq of Expr.t * Expr.t  (** Expression less-than-or-equal for numbers *)
  | ILess of Expr.t * Expr.t  (** Expression less-than for integers *)
  | ILessEq of Expr.t * Expr.t
      (** Expression less-than-or-equal for integeres *)
  | StrLess of Expr.t * Expr.t  (** Expression less-than for strings *)
  | SetMem of Expr.t * Expr.t  (** Set membership *)
  | SetSub of Expr.t * Expr.t  (** Set subsetness *)
  | ForAll of (string * Type.t option) list * t  (** Forall *)
  | IsInt of Expr.t  (** IsInt e <=> (e : float) /\ (e % 1. == 0) *)
[@@deriving eq]

let to_yojson = TypeDef__.formula_to_yojson
let of_yojson = TypeDef__.formula_of_yojson
let compare = Stdlib.compare
let of_bool b = if b then True else False

module MyFormula = struct
  type nonrec t = t

  let compare = Stdlib.compare
end

module Set = Set.Make (MyFormula)

let list_lexprs_collector =
  object (self)
    inherit [_] Visitors.reduce as super
    method private zero = Expr.Set.empty
    method private plus = Expr.Set.union
    method! visit_'label () (_ : int) = self#zero
    method! visit_'annot () () = self#zero

    method! visit_expr () e =
      match e with
      | Lit (LList _)
      | EList _
      | NOp (LstCat, _)
      | UnOp ((Car | Cdr | LstLen), _) -> Expr.Set.singleton e
      | _ -> super#visit_expr () e
  end

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
      | And (a1, a2) -> And (map_a a1, map_a a2)
      | Or (a1, a2) -> Or (map_a a1, map_a a2)
      | Impl (a1, a2) -> Impl (map_a a1, map_a a2)
      | Not a -> Not (map_a a)
      | True -> True
      | False -> False
      | Eq (e1, e2) -> Eq (map_e e1, map_e e2)
      | FLess (e1, e2) -> FLess (map_e e1, map_e e2)
      | FLessEq (e1, e2) -> FLessEq (map_e e1, map_e e2)
      | ILess (e1, e2) -> ILess (map_e e1, map_e e2)
      | ILessEq (e1, e2) -> ILessEq (map_e e1, map_e e2)
      | StrLess (e1, e2) -> StrLess (map_e e1, map_e e2)
      | SetMem (e1, e2) -> SetMem (map_e e1, map_e e2)
      | SetSub (e1, e2) -> SetSub (map_e e1, map_e e2)
      | ForAll (bt, a) -> ForAll (bt, map_a a)
      | IsInt e -> IsInt (map_e e)
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
  | None -> None
  | Some a' ->
      if not recurse then Some a'
      else
        let a'' =
          match a' with
          | And (a1, a2) -> aux_a_double a1 a2 (fun a1 a2 -> And (a1, a2))
          | Or (a1, a2) -> aux_a_double a1 a2 (fun a1 a2 -> Or (a1, a2))
          | Impl (a1, a2) -> aux_a_double a1 a2 (fun a1 a2 -> Impl (a1, a2))
          | Not a -> aux_a_single a (fun a -> Not a)
          | True -> Some True
          | False -> Some False
          | Eq (e1, e2) -> aux_e e1 e2 (fun e1 e2 -> Eq (e1, e2))
          | ILess (e1, e2) -> aux_e e1 e2 (fun e1 e2 -> ILess (e1, e2))
          | ILessEq (e1, e2) -> aux_e e1 e2 (fun e1 e2 -> ILessEq (e1, e2))
          | FLess (e1, e2) -> aux_e e1 e2 (fun e1 e2 -> FLess (e1, e2))
          | FLessEq (e1, e2) -> aux_e e1 e2 (fun e1 e2 -> FLessEq (e1, e2))
          | StrLess (e1, e2) -> aux_e e1 e2 (fun e1 e2 -> StrLess (e1, e2))
          | SetMem (e1, e2) -> aux_e e1 e2 (fun e1 e2 -> SetMem (e1, e2))
          | SetSub (e1, e2) -> aux_e e1 e2 (fun e1 e2 -> SetSub (e1, e2))
          | ForAll (bt, a) -> aux_a_single a (fun a -> ForAll (bt, a))
          | IsInt e -> map_e e |> Option.map (fun e -> IsInt e)
        in
        Option.map f_a_after a''

(* Get all the logical variables in --a-- *)
let lvars (f : t) : SS.t =
  Visitors.Collectors.lvar_collector#visit_formula SS.empty f

(* Get all the program variables in --a-- *)
let pvars (f : t) : SS.t = Visitors.Collectors.pvar_collector#visit_formula () f

(* Get all the abstract locations in --a-- *)
let alocs (f : t) : SS.t = Visitors.Collectors.aloc_collector#visit_formula () f

(* Get all the concrete locations in [a] *)
let clocs (f : t) : SS.t = Visitors.Collectors.cloc_collector#visit_formula () f

(* Get all the locations in [a] *)
let locs (f : t) : SS.t = Visitors.Collectors.cloc_collector#visit_formula () f
let get_print_info (a : t) = (pvars a, lvars a, locs a)

(* Get all the logical expressions of --a-- of the form (Lit (LList lst)) and (EList lst)  *)
let lists (f : t) : Expr.t list =
  Visitors.Collectors.list_collector#visit_formula () f

(* Get all the logical expressions of --a-- that denote a list
   and are not logical variables *)
let list_lexprs (f : t) : Expr.Set.t = list_lexprs_collector#visit_formula () f

let rec push_in_negations_off (a : t) : t =
  let f_off = push_in_negations_off in
  let f_on = push_in_negations_on in
  match a with
  | And (a1, a2) -> And (f_off a1, f_off a2)
  | Or (a1, a2) -> Or (f_off a1, f_off a2)
  | Not a1 -> f_on a1
  | ForAll (bt, a) -> ForAll (bt, f_off a)
  | _ -> a

and push_in_negations_on (a : t) : t =
  let f_off = push_in_negations_off in
  let f_on = push_in_negations_on in
  match a with
  | And (a1, a2) -> Or (f_on a1, f_on a2)
  | Or (a1, a2) -> And (f_on a1, f_on a2)
  | True -> False
  | False -> True
  | Not a -> f_off a
  | _ -> Not a

and push_in_negations (a : t) : t = push_in_negations_off a

let rec split_conjunct_formulae (f : t) : t list =
  match f with
  | And (f1, f2) -> split_conjunct_formulae f1 @ split_conjunct_formulae f2
  | Not (Or (f1, f2)) -> split_conjunct_formulae (And (Not f1, Not f2))
  | f -> [ f ]

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
  (* a1 ==> a2 *)
  | Impl (a1, a2) -> Fmt.pf fmt "(%a ==> %a)" pp a1 pp a2
  (* ! a *)
  | Not a -> Fmt.pf fmt "(! %a)" pp a
  (* true *)
  | True -> Fmt.string fmt "True"
  (* false *)
  | False -> Fmt.string fmt "False"
  (* e1 == e2 *)
  | Eq (e1, e2) -> Fmt.pf fmt "@[(%a ==@ %a)@]" pp_expr e1 pp_expr e2
  (* e1 <#e2 *)
  | FLess (e1, e2) -> Fmt.pf fmt "(%a <# %a)" pp_expr e1 pp_expr e2
  (* e1 <=# e2 *)
  | FLessEq (e1, e2) -> Fmt.pf fmt "(%a <=# %a)" pp_expr e1 pp_expr e2
  (* e1 i<# e2 *)
  | ILess (e1, e2) -> Fmt.pf fmt "(%a i<# %a)" pp_expr e1 pp_expr e2
  (* e1 i<=# e2 *)
  | ILessEq (e1, e2) -> Fmt.pf fmt "(%a i<=# %a)" pp_expr e1 pp_expr e2
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
  | IsInt e -> Fmt.pf fmt "(is_int %a)" pp_expr e

let pp = pp_parametric Expr.pp
let full_pp = pp_parametric Expr.full_pp

let rec lift_logic_expr (e : Expr.t) : (t * t) option =
  let open Syntaxes.Option in
  let f = lift_logic_expr in
  match e with
  | LVar _ | PVar _ -> Some (Eq (e, Lit (Bool true)), Eq (e, Lit (Bool false)))
  | Lit (Bool true) -> Some (True, False)
  | Lit (Bool false) -> Some (False, True)
  | BinOp (e1, Equal, e2) ->
      let a = Eq (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, FLessThan, e2) ->
      let a = FLess (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, ILessThan, e2) ->
      let a = ILess (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, SLessThan, e2) ->
      let a = StrLess (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, FLessThanEqual, e2) ->
      let a = FLessEq (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, ILessThanEqual, e2) ->
      let a = ILessEq (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, BSetMem, e2) ->
      let a = SetMem (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, BSetSub, e2) ->
      let a = SetSub (e1, e2) in
      Some (a, Not a)
  | BinOp (e1, BAnd, e2) ->
      let* a1, na1 = f e1 in
      let+ a2, na2 = f e2 in
      (And (a1, a2), Or (na1, na2))
  | BinOp (e1, BOr, e2) ->
      let* a1, na1 = f e1 in
      let+ a2, na2 = f e2 in
      (Or (a1, a2), And (na1, na2))
  | BinOp (e1, BImpl, e2) ->
      let* a1, _ = f e1 in
      let+ a2, na2 = f e2 in
      (Impl (a1, a2), And (a1, na2))
  | UnOp (UNot, e') ->
      let+ a, na = f e' in
      (na, a)
  | Exists (bt, inner) as e ->
      let+ _, inner_neg = f inner in
      let neg = ForAll (bt, inner_neg) in
      (Eq (e, Expr.bool true), neg)
  | EForall (bt, e) ->
      let+ inner, _ = f e in
      let pos = ForAll (bt, inner) in
      let inner_neg = Expr.Infix.not e in
      let neg = Expr.Exists (bt, inner_neg) in
      (pos, Eq (neg, Expr.bool true))
  | _ -> None

let rec to_expr (a : t) : Expr.t option =
  let f = to_expr in
  match a with
  | True -> Some (Expr.Lit (Bool true))
  | False -> Some (Expr.Lit (Bool false))
  | Not a' -> Option.map (fun a -> Expr.UnOp (UnOp.UNot, a)) (f a')
  | And (a1, a2) -> (
      match (f a1, f a2) with
      | Some le1, Some le2 -> Some (Expr.BinOp (le1, BinOp.BAnd, le2))
      | _ -> None)
  | Or (a1, a2) -> (
      match (f a1, f a2) with
      | Some le1, Some le2 -> Some (Expr.BinOp (le1, BinOp.BOr, le2))
      | _ -> None)
  | Impl (a1, a2) -> (
      match (f (Not a1), f a2) with
      | Some e1, Some e2 -> Some (Expr.BinOp (e1, BinOp.BOr, e2))
      | _ -> None)
  | ForAll _ -> None
  | Eq (le1, le2) -> Some (Expr.BinOp (le1, BinOp.Equal, le2))
  | FLess (le1, le2) -> Some (Expr.BinOp (le1, BinOp.FLessThan, le2))
  | FLessEq (le1, le2) -> Some (Expr.BinOp (le1, BinOp.FLessThanEqual, le2))
  | ILess (le1, le2) -> Some (Expr.BinOp (le1, BinOp.ILessThan, le2))
  | ILessEq (le1, le2) -> Some (Expr.BinOp (le1, BinOp.ILessThanEqual, le2))
  | StrLess (le1, le2) -> Some (Expr.BinOp (le1, BinOp.SLessThan, le2))
  | SetMem (le1, le2) -> Some (Expr.BinOp (le1, BinOp.BSetMem, le2))
  | SetSub (le1, le2) -> Some (Expr.BinOp (le1, BinOp.BSetSub, le2))
  | IsInt e ->
      let is_float = Expr.type_eq e Type.NumberType in
      let is_whole =
        Expr.BinOp (Expr.fmod e (Expr.num 1.), BinOp.Equal, Expr.num 0.)
      in
      Some (Expr.BinOp (is_float, BinOp.BAnd, is_whole))

let rec disjunct (asrts : t list) : t =
  match asrts with
  | [] -> True
  | [ a ] -> a
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
  | _ -> [ fo ]

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
    | True -> False
    | False -> True
    | Not x -> x
    | _ -> Not a

  let forall params f = ForAll (params, f)

  let ( #== ) a b =
    match (a, b) with
    | Expr.Lit la, Expr.Lit lb -> of_bool (Literal.equal la lb)
    | a, b when Expr.equal a b -> True
    | _ -> Eq (a, b)

  let ( #|| ) a b =
    match (a, b) with
    | True, _ | _, True -> True
    | False, f | f, False -> f
    | _ -> Or (a, b)

  let ( #&& ) a b =
    match (a, b) with
    | True, f | f, True -> f
    | False, _ | _, False -> False
    | _ -> And (a, b)

  let ( #< ) a b =
    match (a, b) with
    | Expr.Lit (Int x), Expr.Lit (Int y) -> of_bool (x < y)
    | _ -> ILess (a, b)

  let ( #<= ) a b =
    match (a, b) with
    | Expr.Lit (Int x), Expr.Lit (Int y) -> of_bool (x <= y)
    | _ -> ILessEq (a, b)

  let ( #> ) a b =
    match (a, b) with
    | Expr.Lit (Int x), Expr.Lit (Int y) -> of_bool (x > y)
    | _ -> fnot a #<= b

  let ( #>= ) a b =
    match (a, b) with
    | Expr.Lit (Int x), Expr.Lit (Int y) -> of_bool (x >= y)
    | _ -> fnot a #< b

  let ( #<. ) a b =
    match (a, b) with
    | Expr.Lit (Num x), Expr.Lit (Num y) -> of_bool (x < y)
    | _ -> FLess (a, b)

  let ( #<=. ) a b =
    match (a, b) with
    | Expr.Lit (Num x), Expr.Lit (Num y) -> of_bool (x <= y)
    | _ -> FLessEq (a, b)

  let ( #>. ) a b =
    match (a, b) with
    | Expr.Lit (Num x), Expr.Lit (Num y) -> of_bool (x > y)
    | _ -> fnot a #<= b

  let ( #>=. ) a b =
    match (a, b) with
    | Expr.Lit (Num x), Expr.Lit (Num y) -> of_bool (x >= y)
    | _ -> fnot a #< b

  let ( #=> ) fa fb =
    match (fa, fb) with
    | True, _ -> fb
    | False, _ -> True
    | _, True -> True
    | _, False -> fnot fa
    | _ -> Impl (fa, fb)
end

let pvars_to_lvars (pf : t) : t =
  let fe = Expr.pvars_to_lvars in
  map None None (Some fe) pf

let rec conjunct (asrts : t list) : t =
  match asrts with
  | [] -> True
  | [ a ] -> a
  | a :: r_asrts -> Infix.( #&& ) a (conjunct r_asrts)
