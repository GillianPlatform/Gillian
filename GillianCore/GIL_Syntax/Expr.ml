open Names

(** GIL Expressions *)
type t = TypeDef__.expr =
  | Lit of Literal.t  (** GIL literals           *)
  | PVar of string  (** GIL program variables  *)
  | LVar of LVar.t  (** GIL logical variables  *)
  | ALoc of string  (** GIL abstract locations *)
  | UnOp of UnOp.t * t  (** Unary operators         *)
  | BinOp of t * BinOp.t * t  (** Binary operators        *)
  | LstSub of t * t * t  (** Sublist or (list, start, len) *)
  | NOp of NOp.t * t list  (** n-ary operators         *)
  | EList of t list  (** Lists of expressions    *)
  | ESet of t list  (** Sets of expressions     *)
  | Exists of (string * Type.t option) list * t
      (** Existential quantification. This is now a circus because the separation between Formula and Expr doesn't make sense anymore. *)
  | EForall of (string * Type.t option) list * t
[@@deriving eq, ord]

let to_yojson = TypeDef__.expr_to_yojson
let of_yojson = TypeDef__.expr_of_yojson

(** {3 builders} *)

let lit x = Lit x
let num n = lit (Num n)
let num_int n = lit (Num (float_of_int n))
let int n = lit (Int (Z.of_int n))
let int_z z = lit (Int z)
let string s = lit (String s)
let bool b = lit (Bool b)
let zero_i = int_z Z.zero
let one_i = int_z Z.one

let num_to_int = function
  | Lit (Num n) -> int (int_of_float n)
  | e -> UnOp (NumToInt, e)

let int_to_num = function
  | Lit (Int n) -> num (Z.to_float n)
  | e -> UnOp (IntToNum, e)

let typeof x =
  match x with
  | ALoc _ | Lit (Loc _) -> Lit (Type ObjectType)
  | EList _ | Lit (LList _) -> Lit (Type ListType)
  | ESet _ -> Lit (Type SetType)
  | _ -> UnOp (TypeOf, x)

let list_nth x n =
  let short_circuit =
    match x with
    | EList l -> List.nth_opt l n
    | Lit (LList l) -> List.nth_opt l n |> Option.map lit
    | _ -> None
  in
  match short_circuit with
  | None -> BinOp (x, LstNth, int n)
  | Some x -> x

let list_nth_z x n = list_nth x (Z.to_int n)

let list_nth_e x n =
  match n with
  | Lit (Int n) -> list_nth_z x n
  | Lit (Num _) -> failwith "l-nth of list and Num!"
  | _ -> BinOp (x, LstNth, n)

let list_length x =
  match x with
  | EList l -> int (List.length l)
  | Lit (LList l) -> int (List.length l)
  | LstSub (_, _, len) -> len
  | _ -> UnOp (LstLen, x)

let list_repeat x len =
  match len with
  | Lit (Int i) when Z.lt i (Z.of_int 100) ->
      EList (List.init (Z.to_int i) (fun _ -> x))
  | _ -> BinOp (x, LstRepeat, len)

let list_sub ~lst ~start ~size =
  match (lst, start, size) with
  | EList el, Lit (Int starti), Lit (Int sizei) -> (
      match List_utils.list_sub el (Z.to_int starti) (Z.to_int sizei) with
      | None -> LstSub (lst, start, size)
      | Some sublst -> EList sublst)
  | Lit (LList ll), Lit (Int starti), Lit (Int sizei) -> (
      match List_utils.list_sub ll (Z.to_int starti) (Z.to_int sizei) with
      | None -> LstSub (lst, start, size)
      | Some sublst -> Lit (LList sublst))
  | _ -> LstSub (lst, start, size)

let list_cat la lb =
  let lift l = List.map (fun x -> Lit x) l in
  match (la, lb) with
  | Lit (LList la), Lit (LList lb) -> Lit (LList (la @ lb))
  | Lit (LList la), EList lb -> EList (lift la @ lb)
  | EList la, Lit (LList lb) -> EList (la @ lift lb)
  | EList la, EList lb -> EList (la @ lb)
  | NOp (LstCat, las), NOp (LstCat, lbs) -> NOp (LstCat, las @ lbs)
  | NOp (LstCat, las), lb -> NOp (LstCat, las @ [ lb ])
  | la, NOp (LstCat, lbs) -> NOp (LstCat, la :: lbs)
  | la, lb -> NOp (LstCat, [ la; lb ])

let list_cons el r =
  let sgl =
    match el with
    | Lit x -> Lit (LList [ x ])
    | e -> EList [ e ]
  in
  list_cat sgl r

let list el =
  if
    List.for_all
      (function
        | Lit _ -> true
        | _ -> false)
      el
  then
    Lit
      (LList
         (List.map
            (function
              | Lit l -> l
              | _ -> assert false)
            el))
  else EList el

let fmod a b =
  match (a, b) with
  | Lit (Num a), Lit (Num b) -> Lit (Num (mod_float a b))
  | _ -> BinOp (a, FMod, b)

let imod a b =
  match (a, b) with
  | Lit (Int a), Lit (Int b) -> Lit (Int (Z.( mod ) a b))
  | _, Lit (Int b) when Z.equal b Z.one -> zero_i
  | _ -> BinOp (a, IMod, b)

let type_ t = Lit (Type t)
let type_eq e t = BinOp (typeof e, Equal, type_ t)

module Infix = struct
  let ( +. ) a b =
    match (a, b) with
    | Lit (Num 0.), x | x, Lit (Num 0.) -> x
    | Lit (Num x), Lit (Num y) -> Lit (Num (x +. y))
    | BinOp (x, FPlus, Lit (Num y)), Lit (Num z)
    | BinOp (Lit (Num y), FPlus, x), Lit (Num z)
    | Lit (Num z), BinOp (x, FPlus, Lit (Num y))
    | Lit (Num z), BinOp (Lit (Num y), FPlus, x) ->
        BinOp (x, FPlus, Lit (Num (y +. z)))
    | _ -> BinOp (a, FPlus, b)

  let ( -. ) a b =
    match (a, b) with
    | x, Lit (Num 0.) -> x
    | Lit (Num 0.), x -> UnOp (FUnaryMinus, x)
    | Lit (Num x), Lit (Num y) -> Lit (Num (x -. y))
    | BinOp (x, FPlus, y), z when equal y z -> x
    | BinOp (x, FPlus, y), z when equal x z -> y
    | _ -> BinOp (a, FMinus, b)

  let ( *. ) a b =
    match (a, b) with
    | Lit (Num 0.), _ | _, Lit (Num 0.) -> Lit (Num 0.)
    | Lit (Num 1.), x | x, Lit (Num 1.) -> x
    | Lit (Num x), Lit (Num y) -> Lit (Num (x *. y))
    | _ -> BinOp (a, FTimes, b)

  let ( /. ) a b =
    match (a, b) with
    | x, Lit (Num 1.) -> x
    | BinOp (x, FTimes, y), z when equal y z -> x
    | BinOp (x, FTimes, y), z when equal x z -> y
    | Lit (Num x), Lit (Num y) -> Lit (Num (x /. y))
    | _ -> BinOp (a, FDiv, b)

  let ( - ) a b =
    match (a, b) with
    | x, Lit (Int z) when Z.equal Z.zero z -> x
    | Lit (Num 0.), x -> UnOp (IUnaryMinus, x)
    | Lit (Int x), Lit (Int y) -> Lit (Int (Z.sub x y))
    | BinOp (x, IPlus, y), z when equal y z -> x
    | BinOp (x, IPlus, y), z when equal x z -> y
    | x, y when equal x y -> zero_i
    | _ -> BinOp (a, IMinus, b)

  let rec ( + ) a b =
    let plus = ( + ) in
    let minus = ( - ) in
    let open! Z in
    match (a, b) with
    | Lit (Int z), x when equal z zero -> x
    | x, Lit (Int z) when equal z zero -> x
    | Lit (Int x), Lit (Int y) -> Lit (Int (x + y))
    | BinOp (Lit (Int x), IPlus, y), Lit (Int z)
    | Lit (Int z), BinOp (Lit (Int x), IPlus, y) ->
        BinOp (Lit (Int (x + z)), IPlus, y)
    | BinOp (y, IPlus, Lit (Int x)), Lit (Int z)
    | Lit (Int z), BinOp (y, IPlus, Lit (Int x)) ->
        BinOp (y, IPlus, Lit (Int (x + z)))
    | UnOp (IUnaryMinus, x), UnOp (IUnaryMinus, y) ->
        UnOp (IUnaryMinus, plus x y)
    | x, UnOp (IUnaryMinus, y) -> minus x y
    | _ -> BinOp (a, IPlus, b)

  let ( * ) a b =
    let open! Z in
    match (a, b) with
    | Lit (Int x), Lit (Int y) -> Lit (Int (x * y))
    | Lit (Int z), _ when equal zero z -> zero_i
    | _, Lit (Int z) when equal zero z -> zero_i
    | Lit (Int z), x when equal one z -> x
    | x, Lit (Int z) when equal one z -> x
    | _ -> BinOp (a, ITimes, b)

  let ( / ) a b =
    match (a, b) with
    | x, Lit (Int z) when Z.equal z Z.one -> x
    | x, BinOp (a, ITimes, b) when equal x a -> b
    | x, BinOp (a, ITimes, b) when equal x b -> a
    | BinOp (a, ITimes, b), x when equal x a -> b
    | BinOp (a, ITimes, b), x when equal x b -> a
    | Lit (Int x), Lit (Int y) -> Lit (Int (Z.div x y))
    | _ -> BinOp (a, IDiv, b)

  let ( << ) a b =
    match (a, b) with
    | Lit (Int z), _ when Z.equal z Z.zero -> a
    | _, Lit (Int z) when Z.equal z Z.zero -> a
    | Lit (Int x), Lit (Int y) -> Lit (Int (Z.shift_left x (Z.to_int y)))
    | _ -> BinOp (a, LeftShift, b)

  let ( ~- ) = function
    | Lit (Int z) -> Lit (Int (Z.neg z))
    | UnOp (IUnaryMinus, z) -> z
    | z -> UnOp (IUnaryMinus, z)

  let not a =
    match a with
    | Lit (Bool a) -> Lit (Bool (not a))
    | x -> UnOp (UNot, x)

  let ( @+ ) = list_cat
end

module MyExpr = struct
  type nonrec t = t

  let of_yojson = of_yojson
  let to_yojson = to_yojson
  let compare = compare
end

module Set = Set.Make (MyExpr)
module Map = Map.Make (MyExpr)

(** Map over expressions *)

(* let rec map (f_before : t -> t * bool) (f_after : (t -> t) option) (expr : t) :
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
     f_after mapped_expr *)

(** Optional map over expressions *)

let rec map_opt
    (f_before : t -> t option * bool)
    (f_after : (t -> t) option)
    (expr : t) : t option =
  (* Apply the mapping *)
  let map_e = map_opt f_before f_after in
  let f_after = Option.value ~default:(fun x -> x) f_after in

  let aux args f = List_utils.flaky_map map_e args |> Option.map f in

  match f_before expr with
  | None, _ -> None
  | mapped_expr, false -> mapped_expr
  | Some mapped_expr, true ->
      (* Map recursively to expressions *)
      let mapped_expr =
        match mapped_expr with
        | Lit _ | LVar _ | ALoc _ | PVar _ -> Some mapped_expr
        | UnOp (op, e) -> Option.map (fun e -> UnOp (op, e)) (map_e e)
        | BinOp (e1, op, e2) -> (
            match (map_e e1, map_e e2) with
            | Some e1', Some e2' -> Some (BinOp (e1', op, e2'))
            | _ -> None)
        | LstSub (e1, e2, e3) -> (
            match (map_e e1, map_e e2, map_e e3) with
            | Some e1', Some e2', Some e3' -> Some (LstSub (e1', e2', e3'))
            | _ -> None)
        | NOp (op, les) -> aux les (fun les -> NOp (op, les))
        | EList les -> aux les (fun les -> EList les)
        | ESet les -> aux les (fun les -> ESet les)
        | Exists (bt, e) -> (
            match map_e e with
            | Some e' -> Some (Exists (bt, e'))
            | _ -> None)
        | EForall (bt, e) -> (
            match map_e e with
            | Some e' -> Some (EForall (bt, e'))
            | _ -> None)
      in
      Option.map f_after mapped_expr

(** Printer *)
let rec pp fmt e =
  let pp_var_with_type fmt (x, t_opt) =
    Fmt.pf fmt "%s%a" x
      (Fmt.option (fun fm t -> Fmt.pf fm " : %s" (Type.str t)))
      t_opt
  in
  match e with
  | Lit l -> Literal.pp fmt l
  | PVar v | LVar v | ALoc v -> Fmt.string fmt v
  | BinOp (e1, op, e2) -> (
      match op with
      | LstNth | StrNth | LstRepeat ->
          Fmt.pf fmt "%s(%a, %a)" (BinOp.str op) pp e1 pp e2
      | _ -> Fmt.pf fmt "(%a %s %a)" pp e1 (BinOp.str op) pp e2)
  | LstSub (e1, e2, e3) -> Fmt.pf fmt "l-sub(%a, %a, %a)" pp e1 pp e2 pp e3
  (* (uop e) *)
  | UnOp (op, e) -> Fmt.pf fmt "(%s %a)" (UnOp.str op) pp e
  | EList ll -> Fmt.pf fmt "{{ %a }}" (Fmt.list ~sep:Fmt.comma pp) ll
  (* -{ e1, e2, ... }- *)
  | ESet ll -> Fmt.pf fmt "-{ %a }-" (Fmt.list ~sep:Fmt.comma pp) ll
  | NOp (op, le) ->
      Fmt.pf fmt "%s %a" (NOp.str op)
        (Fmt.parens (Fmt.list ~sep:Fmt.comma pp))
        le
  | Exists (bt, e) ->
      Fmt.pf fmt "(exists %a . %a)"
        (Fmt.list ~sep:Fmt.comma pp_var_with_type)
        bt pp e
  | EForall (bt, e) ->
      Fmt.pf fmt "(forall %a . %a)"
        (Fmt.list ~sep:Fmt.comma pp_var_with_type)
        bt pp e

let rec full_pp fmt e =
  match e with
  | Lit _ -> Fmt.pf fmt "Lit %a" pp e
  | PVar _ -> Fmt.pf fmt "PVar %a" pp e
  | LVar _ -> Fmt.pf fmt "LVar %a" pp e
  | ALoc _ -> Fmt.pf fmt "ALoc %a" pp e
  | BinOp (e1, op, e2) ->
      Fmt.pf fmt "(%a %s %a)" full_pp e1 (BinOp.str op) full_pp e2
  | UnOp (op, e) -> Fmt.pf fmt "(%s %a)" (UnOp.str op) pp e
  | LstSub (e1, e2, e3) ->
      Fmt.pf fmt "l-sub(%a, %a, %a)" full_pp e1 full_pp e2 full_pp e3
  | NOp _ -> Fmt.pf fmt "(NOp %a)" pp e
  | EList ll -> Fmt.pf fmt "{{ @[%a@] }}" (Fmt.list ~sep:Fmt.comma full_pp) ll
  | _ -> pp fmt e

(** From expression to expression *)
let to_expr (le : t) : t = le

(** From expression to list, if possible *)
let to_list (le : t) : t list option =
  match le with
  | EList les -> Some les
  | Lit (LList les) -> Some (List.map (fun x -> Lit x) les)
  | _ -> None

(** From list to expression *)
let from_list les = EList les

let to_literal = function
  | Lit lit -> Some lit
  | _ -> None

(** Get all the logical variables in --e-- *)
let lvars (le : t) : SS.t =
  Visitors.Collectors.lvar_collector#visit_expr SS.empty le

(** Get all the abstract locations in --e-- *)
let alocs (le : t) : SS.t = Visitors.Collectors.aloc_collector#visit_expr () le

(** Get all the concrete locations in --e-- *)
let clocs (le : t) : SS.t = Visitors.Collectors.cloc_collector#visit_expr () le

let locs (le : t) : SS.t = Visitors.Collectors.loc_collector#visit_expr () le

(** Get all substitutables in --e-- *)
let substitutables (le : t) : SS.t =
  Visitors.Collectors.substitutable_collector#visit_expr () le

let rec is_concrete (le : t) : bool =
  let f = is_concrete in

  let rec loop les =
    match les with
    | [] -> true
    | le :: rest -> if f le then loop rest else false
  in

  match le with
  | Lit _ | PVar _ -> true
  | LVar _ | ALoc _ | Exists _ | EForall _ -> false
  | UnOp (_, e) -> loop [ e ]
  | BinOp (e1, _, e2) -> loop [ e1; e2 ]
  | LstSub (e1, e2, e3) -> loop [ e1; e2; e3 ]
  | NOp (_, les) | EList les | ESet les -> loop les

let is_concrete_zero_i (le : t) : bool =
  match le with
  | Lit (Int z) -> Z.equal Z.zero z
  | _ -> false

(** Get all the variables in --e-- *)
let vars (le : t) : SS.t = Visitors.Collectors.var_collector#visit_expr () le

(** Are all expressions in the list literals? *)
let all_literals les =
  List.for_all
    (fun x ->
      match x with
      | Lit _ -> true
      | _ -> false)
    les

(** Lifting literal lists to lists of expressions *)
let rec from_lit_list (lit : Literal.t) : t =
  let f = from_lit_list in
  match lit with
  | LList lst -> EList (List.map f lst)
  | _ -> Lit lit

(** Get all sub-expressions of --e-- of the form (Lit (LList lst)) and (EList lst)  *)
let lists (le : t) : t list =
  Visitors.Collectors.list_collector#visit_expr () le

let subst_clocs (subst : string -> t) (e : t) : t =
  (new Visitors.Substs.subst_clocs subst)#visit_expr () e

let from_var_name (var_name : string) : t =
  if is_aloc_name var_name then ALoc var_name
  else if is_lvar_name var_name then LVar var_name
  else if is_pvar_name var_name then PVar var_name
  else Fmt.failwith "Invalid var name : %s" var_name

let loc_from_loc_name (loc_name : string) : t =
  if is_aloc_name loc_name then ALoc loc_name else Lit (Loc loc_name)

(** {2 Visitors} *)

let subst_expr_for_expr ~to_subst ~subst_with expr =
  let v =
    object
      inherit [_] Visitors.endo as super

      method! visit_expr env e =
        if equal e to_subst then subst_with else super#visit_expr env e
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
        | e -> super#visit_expr env e
    end
  in
  v#visit_expr () expr

let pvars (e : t) : SS.t = Visitors.Collectors.pvar_collector#visit_expr () e

let var_to_expr (x : string) : t =
  if Names.is_lvar_name x then LVar x
  else if is_aloc_name x then ALoc x
  else if is_pvar_name x then PVar x
  else raise (Failure ("var_to_expr: Impossible matchable: " ^ x))

let is_matchable (e : t) : bool =
  match e with
  | PVar _ | LVar _ | ALoc _ | UnOp (LstLen, PVar _) | UnOp (LstLen, LVar _) ->
      true
  | _ -> false

let rec pvars_to_lvars (e : t) : t =
  let f = pvars_to_lvars in
  match e with
  | PVar x -> LVar ("#__" ^ x)
  | UnOp (op, e) -> UnOp (op, f e)
  | BinOp (e1, op, e2) -> BinOp (f e1, op, f e2)
  | LstSub (e1, e2, e3) -> LstSub (f e1, f e2, f e3)
  | NOp (op, les) -> NOp (op, List.map f les)
  | EList les -> EList (List.map f les)
  | ESet les -> ESet (List.map f les)
  | _ -> e
