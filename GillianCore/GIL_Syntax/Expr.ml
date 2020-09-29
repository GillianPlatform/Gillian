open Containers
open Names

(** GIL Expressions *)
type t = TypeDef__.expr =
  | Lit    of Literal.t  (** GIL literals           *)
  | PVar   of string  (** GIL program variables  *)
  | LVar   of LVar.t  (** GIL logical variables  *)
  | ALoc   of string  (** GIL abstract locations *)
  | UnOp   of UnOp.t * t  (** Unary operators         *)
  | BinOp  of t * BinOp.t * t  (** Binary operators        *)
  | LstSub of t * t * t  (** Sublist                 *)
  | NOp    of NOp.t * t list  (** n-ary operators         *)
  | EList  of t list  (** Lists of expressions    *)
  | ESet   of t list  (** Sets of expressions     *)

(** {3 builders} *)

let lit x = Lit x

let num n = lit (Num n)

let int n = lit (Int n)

let string s = lit (String s)

let typeof x = UnOp (TypeOf, x)

let list_nth x n = BinOp (x, LstNth, num (float_of_int n))

let list_length x = UnOp (LstLen, x)

let type_ t = Lit (Type t)

module Infix = struct
  let ( +. ) a b = BinOp (a, FPlus, b)

  let ( + ) a b = BinOp (a, IPlus, b)
end

module MyExpr = struct
  type nonrec t = t

  let compare = Stdlib.compare
end

module Set = Set.Make (MyExpr)
module Map = Map.Make (MyExpr)

let equal (e1 : t) (e2 : t) : bool = Stdlib.compare e1 e2 = 0

(** Map over expressions *)
let rec map (f_before : t -> t * bool) (f_after : (t -> t) option) (expr : t) :
    t =
  (* Apply the mapping *)
  let map_e = map f_before f_after in
  let f_after = Option.value ~default:(fun x -> x) f_after in

  let mapped_expr, recurse = f_before expr in
  if not recurse then mapped_expr
  else
    (* Map recursively to expressions *)
    let mapped_expr =
      match mapped_expr with
      | Lit _ | PVar _ | LVar _ | ALoc _ -> mapped_expr
      | UnOp (op, e) -> UnOp (op, map_e e)
      | BinOp (e1, op, e2) -> BinOp (map_e e1, op, map_e e2)
      | LstSub (e1, e2, e3) -> LstSub (map_e e1, map_e e2, map_e e3)
      | NOp (op, es) -> NOp (op, List.map map_e es)
      | EList es -> EList (List.map map_e es)
      | ESet es -> ESet (List.map map_e es)
    in
    f_after mapped_expr

(** Optional map over expressions *)
let rec map_opt
    (f_before : t -> t option * bool) (f_after : (t -> t) option) (expr : t) :
    t option =
  (* Apply the mapping *)
  let map_e = map_opt f_before f_after in
  let f_after = Option.value ~default:(fun x -> x) f_after in

  let aux args f =
    let args' = List.map map_e args in
    if List.exists (fun arg -> arg = None) args' then None
    else Some (f (List.map Option.get args'))
  in

  match f_before expr with
  | None, _                -> None
  | mapped_expr, false     -> mapped_expr
  | Some mapped_expr, true ->
      (* Map recursively to expressions *)
      let mapped_expr =
        match mapped_expr with
        | Lit _ | LVar _ | ALoc _ | PVar _ -> Some mapped_expr
        | UnOp (op, e) -> Option.map (fun e -> UnOp (op, e)) (map_e e)
        | BinOp (e1, op, e2) -> (
            match (map_e e1, map_e e2) with
            | Some e1', Some e2' -> Some (BinOp (e1', op, e2'))
            | _                  -> None )
        | LstSub (e1, e2, e3) -> (
            match (map_e e1, map_e e2, map_e e3) with
            | Some e1', Some e2', Some e3' -> Some (LstSub (e1', e2', e3'))
            | _ -> None )
        | NOp (op, les) -> aux les (fun les -> NOp (op, les))
        | EList les -> aux les (fun les -> EList les)
        | ESet les -> aux les (fun les -> ESet les)
      in
      Option.map f_after mapped_expr

(** Printer *)
let rec pp fmt e =
  match e with
  | Lit l -> Literal.pp fmt l
  | PVar v | LVar v | ALoc v -> Fmt.string fmt v
  | BinOp (e1, op, e2) -> (
      match op with
      | LstNth | StrNth -> Fmt.pf fmt "%s(%a, %a)" (BinOp.str op) pp e1 pp e2
      | _               -> Fmt.pf fmt "(%a %s %a)" pp e1 (BinOp.str op) pp e2 )
  | LstSub (e1, e2, e3) -> Fmt.pf fmt "l-sub(%a, %a, %a)" pp e1 pp e2 pp e3
  (* (uop e) *)
  | UnOp (op, e) -> Fmt.pf fmt "(%s %a)" (UnOp.str op) pp e
  | EList ll -> Fmt.pf fmt "{%a}" (Fmt.braces (Fmt.list ~sep:Fmt.comma pp)) ll
  (* -{ e1, e2, ... }- *)
  | ESet ll -> Fmt.pf fmt "-{ %a }-" (Fmt.list ~sep:Fmt.comma pp) ll
  | NOp (op, le) ->
      Fmt.pf fmt "%s %a" (NOp.str op)
        (Fmt.parens (Fmt.list ~sep:Fmt.comma pp))
        le

let rec full_pp fmt e =
  match e with
  | Lit _    -> Fmt.pf fmt "(Lit %a)" pp e
  | PVar _   -> Fmt.pf fmt "PVar %a" pp e
  | LVar _   -> Fmt.pf fmt "LVar %a" pp e
  | ALoc _   -> Fmt.pf fmt "ALoc %a" pp e
  | BinOp _  -> Fmt.pf fmt "(BinOp: %a)" pp e
  | UnOp _   -> Fmt.pf fmt "(UnOp %a)" pp e
  | LstSub _ -> Fmt.pf fmt "(LstSub %a)" pp e
  | NOp _    -> Fmt.pf fmt "(NOp %a)" pp e
  | EList ll -> Fmt.pf fmt "{{ @[%a@] }}" (Fmt.list ~sep:Fmt.comma full_pp) ll
  | _        -> pp fmt e

(** From expression to expression *)
let to_expr (le : t) : t = le

(** From expression to list, if possible *)
let to_list (le : t) : t list option =
  match le with
  | EList les       -> Some les
  | Lit (LList les) -> Some (List.map (fun x -> Lit x) les)
  | _               -> None

(** From list to expression *)
let from_list les = EList les

(** Fold *)
let rec fold
    (f_ac : t -> 'b -> 'b -> 'a list -> 'a)
    (f_state : (t -> 'b -> 'b) option)
    (state : 'b)
    (expr : t) : 'a =
  let new_state = (Option.value ~default:(fun _ x -> x) f_state) expr state in
  let fold_e = fold f_ac f_state new_state in
  let f_ac = f_ac expr new_state state in

  match expr with
  | Lit _ | LVar _ | ALoc _ | PVar _ -> f_ac []
  | UnOp (_, e) -> f_ac [ fold_e e ]
  | BinOp (e1, _, e2) -> f_ac [ fold_e e1; fold_e e2 ]
  | LstSub (e1, e2, e3) -> f_ac [ fold_e e1; fold_e e2; fold_e e3 ]
  | NOp (_, les) | EList les | ESet les -> f_ac (List.map fold_e les)

(** Get all the logical variables in --e-- *)
let lvars (le : t) : SS.t =
  let fe_ac le _ _ ac =
    match le with
    | LVar x -> [ x ]
    | _      -> List.concat ac
  in
  SS.of_list (fold fe_ac None None le)

(** Get all the abstract locations in --e-- *)
let alocs (le : t) : SS.t =
  let fe_ac le _ _ ac =
    match le with
    | ALoc x -> [ x ]
    | _      -> List.concat ac
  in
  SS.of_list (fold fe_ac None None le)

(** Get all the concrete locations in --e-- *)
let clocs (le : t) : SS.t =
  let fe_ac le _ _ ac =
    match le with
    | Lit (Loc l) -> l :: List.concat ac
    | _           -> List.concat ac
  in
  SS.of_list (fold fe_ac None None le)

(** Get all substitutables in --e-- *)
let substitutables (le : t) : SS.t =
  let fe_ac le _ _ ac =
    match le with
    | LVar x | ALoc x -> [ x ]
    | _               -> List.concat ac
  in
  SS.of_list (fold fe_ac None None le)

let rec is_concrete (le : t) : bool =
  let f = is_concrete in

  let rec loop les =
    match les with
    | []         -> true
    | le :: rest -> if f le then loop rest else false
  in

  match le with
  | Lit _ | PVar _ -> true
  | LVar _ | ALoc _ -> false
  | UnOp (_, e) -> loop [ e ]
  | BinOp (e1, _, e2) -> loop [ e1; e2 ]
  | LstSub (e1, e2, e3) -> loop [ e1; e2; e3 ]
  | NOp (_, les) | EList les | ESet les -> loop les

(** Get all the variables in --e-- *)
let vars (le : t) : SS.t =
  let fe_ac le _ _ ac =
    match le with
    | PVar x | LVar x | ALoc x | Lit (Loc x) -> [ x ]
    | _ -> List.concat ac
  in
  SS.of_list (fold fe_ac None None le)

(** Are all expressions in the list literals? *)
let all_literals les =
  List.for_all
    (fun x ->
      match x with
      | Lit _ -> true
      | _     -> false)
    les

(** Lifting literal lists to lists of expressions *)
let rec from_lit_list (lit : Literal.t) : t =
  let f = from_lit_list in
  match lit with
  | LList lst -> EList (List.map f lst)
  | _         -> Lit lit

(** Get all sub-expressions of --e-- of the form (Lit (LList lst)) and (EList lst)  *)
let lists (le : t) : t list =
  let fe_ac le _ _ ac =
    match le with
    | Lit (LList ls)    ->
        [ EList (List.map (fun x -> Lit x) ls) ] @ List.concat ac
    | EList _           -> le :: List.concat ac
    | NOp (LstCat, les) -> les @ List.concat ac
    | _                 -> List.concat ac
  in
  fold fe_ac None None le

let subst_clocs (subst : string -> t) (e : t) : t =
  let f_before e =
    match e with
    | Lit (Loc loc) -> (subst loc, false)
    | _             -> (e, true)
  in
  map f_before None e

let from_var_name (var_name : string) : t =
  if is_aloc_name var_name then ALoc var_name
  else if is_lvar_name var_name then LVar var_name
  else if is_pvar_name var_name then PVar var_name
  else raise (Failure "DEATH")

let loc_from_loc_name (loc_name : string) : t =
  if is_aloc_name loc_name then ALoc loc_name else Lit (Loc loc_name)

(** {2 Visitors} *)

let subst_expr_for_expr ~to_subst ~subst_with expr =
  let v =
    object
      inherit [_] Visitors.map as super

      method! visit_expr env e =
        if e = to_subst then subst_with else super#visit_expr env e
    end
  in
  v#visit_expr () expr

let base_elements (expr : t) : t list =
  let v =
    object
      inherit [_] Visitors.reduce as super

      inherit Visitors.Utils.list_monoid

      method! visit_literal =
        Literal.get_base_lits super#visit_literal (fun x -> Lit x)

      method! visit_expr env =
        function
        | (LVar _ | ALoc _) as e -> [ e ]
        | e                      -> super#visit_expr env e
    end
  in
  v#visit_expr () expr

let pvars (e : t) : SS.t =
  let v =
    object
      inherit [_] Visitors.reduce

      inherit Visitors.Utils.ss_monoid

      method! visit_PVar _ x = SS.singleton x
    end
  in
  v#visit_expr () e
