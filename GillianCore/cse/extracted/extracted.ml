
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some a -> Some (f a)
| None -> None

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (_, y) -> y

(** val uncurry : ('a1 -> 'a2 -> 'a3) -> ('a1 * 'a2) -> 'a3 **)

let uncurry f = function
| (x, y) -> f x y

(** val length : 'a1 list -> int **)

let rec length = function
| [] -> 0
| _ :: l' -> Stdlib.Int.succ (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

type comparison =
| Eq
| Lt
| Gt

(** val id : __ -> __ **)

let id x =
  x

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)



module Coq__1 = struct
 (** val add : int -> int -> int **)

 let rec add = (+)
end
include Coq__1

(** val compose : ('a2 -> 'a3) -> ('a1 -> 'a2) -> 'a1 -> 'a3 **)

let compose g f x =
  g (f x)

module Nat =
 struct
  (** val sub : int -> int -> int **)

  let rec sub n0 m =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> n0)
      (fun k ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> n0)
        (fun l -> sub k l)
        m)
      n0

  (** val ltb : int -> int -> bool **)

  let ltb n0 m =
    (<=) (Stdlib.Int.succ n0) m

  (** val divmod : int -> int -> int -> int -> int * int **)

  let rec divmod x y q0 u =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> (q0, u))
      (fun x' ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> divmod x' y (Stdlib.Int.succ q0) y)
        (fun u' -> divmod x' y q0 u')
        u)
      x

  (** val div : int -> int -> int **)

  let div x y =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> y)
      (fun y' -> fst (divmod x y' 0 y'))
      y

  (** val modulo : int -> int -> int **)

  let modulo x y =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> x)
      (fun y' -> sub y' (snd (divmod x y' 0 y')))
      y
 end

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH

  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  (** val succ_double_mask : mask -> mask **)

  let succ_double_mask = function
  | IsNul -> IsPos XH
  | IsPos p -> IsPos (XI p)
  | IsNeg -> IsNeg

  (** val double_mask : mask -> mask **)

  let double_mask = function
  | IsPos p -> IsPos (XO p)
  | x0 -> x0

  (** val double_pred_mask : positive -> mask **)

  let double_pred_mask = function
  | XI p -> IsPos (XO (XO p))
  | XO p -> IsPos (XO (pred_double p))
  | XH -> IsNul

  (** val sub_mask : positive -> positive -> mask **)

  let rec sub_mask x y =
    match x with
    | XI p ->
      (match y with
       | XI q0 -> double_mask (sub_mask p q0)
       | XO q0 -> succ_double_mask (sub_mask p q0)
       | XH -> IsPos (XO p))
    | XO p ->
      (match y with
       | XI q0 -> succ_double_mask (sub_mask_carry p q0)
       | XO q0 -> double_mask (sub_mask p q0)
       | XH -> IsPos (pred_double p))
    | XH -> (match y with
             | XH -> IsNul
             | _ -> IsNeg)

  (** val sub_mask_carry : positive -> positive -> mask **)

  and sub_mask_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q0 -> succ_double_mask (sub_mask_carry p q0)
       | XO q0 -> double_mask (sub_mask p q0)
       | XH -> IsPos (pred_double p))
    | XO p ->
      (match y with
       | XI q0 -> double_mask (sub_mask_carry p q0)
       | XO q0 -> succ_double_mask (sub_mask_carry p q0)
       | XH -> double_pred_mask p)
    | XH -> IsNeg

  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | XI p ->
      (match y with
       | XI q0 -> compare_cont r p q0
       | XO q0 -> compare_cont Gt p q0
       | XH -> Gt)
    | XO p ->
      (match y with
       | XI q0 -> compare_cont Lt p q0
       | XO q0 -> compare_cont r p q0
       | XH -> Gt)
    | XH -> (match y with
             | XH -> r
             | _ -> Lt)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq

  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)

  let rec iter_op op p a =
    match p with
    | XI p0 -> op a (iter_op op p0 (op a a))
    | XO p0 -> iter_op op p0 (op a a)
    | XH -> a

  (** val to_nat : positive -> int **)

  let to_nat x =
    iter_op add x (Stdlib.Int.succ 0)

  (** val of_succ_nat : int -> positive **)

  let rec of_succ_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> XH)
      (fun x -> succ (of_succ_nat x))
      n0
 end

module Coq_Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val add : positive -> positive -> positive **)

  let rec add x y =
    match x with
    | XI p ->
      (match y with
       | XI q0 -> XO (add_carry p q0)
       | XO q0 -> XI (add p q0)
       | XH -> XO (succ p))
    | XO p ->
      (match y with
       | XI q0 -> XI (add p q0)
       | XO q0 -> XO (add p q0)
       | XH -> XI p)
    | XH -> (match y with
             | XI q0 -> XO (succ q0)
             | XO q0 -> XI q0
             | XH -> XO XH)

  (** val add_carry : positive -> positive -> positive **)

  and add_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q0 -> XI (add_carry p q0)
       | XO q0 -> XO (add_carry p q0)
       | XH -> XI (succ p))
    | XO p ->
      (match y with
       | XI q0 -> XO (add_carry p q0)
       | XO q0 -> XI (add p q0)
       | XH -> XO (succ p))
    | XH ->
      (match y with
       | XI q0 -> XI (succ q0)
       | XO q0 -> XO (succ q0)
       | XH -> XI XH)

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH

  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  (** val succ_double_mask : mask -> mask **)

  let succ_double_mask = function
  | IsNul -> IsPos XH
  | IsPos p -> IsPos (XI p)
  | IsNeg -> IsNeg

  (** val double_mask : mask -> mask **)

  let double_mask = function
  | IsPos p -> IsPos (XO p)
  | x0 -> x0

  (** val double_pred_mask : positive -> mask **)

  let double_pred_mask = function
  | XI p -> IsPos (XO (XO p))
  | XO p -> IsPos (XO (pred_double p))
  | XH -> IsNul

  (** val sub_mask : positive -> positive -> mask **)

  let rec sub_mask x y =
    match x with
    | XI p ->
      (match y with
       | XI q0 -> double_mask (sub_mask p q0)
       | XO q0 -> succ_double_mask (sub_mask p q0)
       | XH -> IsPos (XO p))
    | XO p ->
      (match y with
       | XI q0 -> succ_double_mask (sub_mask_carry p q0)
       | XO q0 -> double_mask (sub_mask p q0)
       | XH -> IsPos (pred_double p))
    | XH -> (match y with
             | XH -> IsNul
             | _ -> IsNeg)

  (** val sub_mask_carry : positive -> positive -> mask **)

  and sub_mask_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q0 -> succ_double_mask (sub_mask_carry p q0)
       | XO q0 -> double_mask (sub_mask p q0)
       | XH -> IsPos (pred_double p))
    | XO p ->
      (match y with
       | XI q0 -> double_mask (sub_mask_carry p q0)
       | XO q0 -> succ_double_mask (sub_mask_carry p q0)
       | XH -> double_pred_mask p)
    | XH -> IsNeg

  (** val sub : positive -> positive -> positive **)

  let sub x y =
    match sub_mask x y with
    | IsPos z0 -> z0
    | _ -> XH

  (** val mul : positive -> positive -> positive **)

  let rec mul x y =
    match x with
    | XI p -> add y (XO (mul p y))
    | XO p -> XO (mul p y)
    | XH -> y

  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | XI p ->
      (match y with
       | XI q0 -> compare_cont r p q0
       | XO q0 -> compare_cont Gt p q0
       | XH -> Gt)
    | XO p ->
      (match y with
       | XI q0 -> compare_cont Lt p q0
       | XO q0 -> compare_cont r p q0
       | XH -> Gt)
    | XH -> (match y with
             | XH -> r
             | _ -> Lt)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq

  (** val pred : positive -> positive **)

  let pred = function
  | XI p -> XO p
  | XO p -> pred_double p
  | XH -> XH

  (** val size_nat : positive -> int **)

  let rec size_nat = function
  | XI p0 -> Stdlib.Int.succ (size_nat p0)
  | XO p0 -> Stdlib.Int.succ (size_nat p0)
  | XH -> Stdlib.Int.succ 0

  (** val ggcdn :
      int -> positive -> positive -> positive * (positive * positive) **)

  let rec ggcdn n0 a b =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> (XH, (a, b)))
      (fun n1 ->
      match a with
      | XI a' ->
        (match b with
         | XI b' ->
           (match compare a' b' with
            | Eq -> (a, (XH, XH))
            | Lt ->
              let (g, p) = ggcdn n1 (sub b' a') a in
              let (ba, aa) = p in (g, (aa, (add aa (XO ba))))
            | Gt ->
              let (g, p) = ggcdn n1 (sub a' b') b in
              let (ab, bb) = p in (g, ((add bb (XO ab)), bb)))
         | XO b0 ->
           let (g, p) = ggcdn n1 a b0 in
           let (aa, bb) = p in (g, (aa, (XO bb)))
         | XH -> (XH, (a, XH)))
      | XO a0 ->
        (match b with
         | XI _ ->
           let (g, p) = ggcdn n1 a0 b in
           let (aa, bb) = p in (g, ((XO aa), bb))
         | XO b0 -> let (g, p) = ggcdn n1 a0 b0 in ((XO g), p)
         | XH -> (XH, (a, XH)))
      | XH -> (XH, (XH, b)))
      n0

  (** val ggcd : positive -> positive -> positive * (positive * positive) **)

  let ggcd a b =
    ggcdn (Coq__1.add (size_nat a) (size_nat b)) a b

  (** val eq_dec : positive -> positive -> bool **)

  let rec eq_dec p x0 =
    match p with
    | XI p0 -> (match x0 with
                | XI p1 -> eq_dec p0 p1
                | _ -> false)
    | XO p0 -> (match x0 with
                | XO p1 -> eq_dec p0 p1
                | _ -> false)
    | XH -> (match x0 with
             | XH -> true
             | _ -> false)
 end

module N =
 struct
  (** val succ_double : n -> n **)

  let succ_double = function
  | N0 -> Npos XH
  | Npos p -> Npos (XI p)

  (** val double : n -> n **)

  let double = function
  | N0 -> N0
  | Npos p -> Npos (XO p)

  (** val sub : n -> n -> n **)

  let sub n0 m =
    match n0 with
    | N0 -> N0
    | Npos n' ->
      (match m with
       | N0 -> n0
       | Npos m' ->
         (match Pos.sub_mask n' m' with
          | Pos.IsPos p -> Npos p
          | _ -> N0))

  (** val compare : n -> n -> comparison **)

  let compare n0 m =
    match n0 with
    | N0 -> (match m with
             | N0 -> Eq
             | Npos _ -> Lt)
    | Npos n' -> (match m with
                  | N0 -> Gt
                  | Npos m' -> Pos.compare n' m')

  (** val leb : n -> n -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val pos_div_eucl : positive -> n -> n * n **)

  let rec pos_div_eucl a b =
    match a with
    | XI a' ->
      let (q0, r) = pos_div_eucl a' b in
      let r' = succ_double r in
      if leb b r' then ((succ_double q0), (sub r' b)) else ((double q0), r')
    | XO a' ->
      let (q0, r) = pos_div_eucl a' b in
      let r' = double r in
      if leb b r' then ((succ_double q0), (sub r' b)) else ((double q0), r')
    | XH ->
      (match b with
       | N0 -> (N0, (Npos XH))
       | Npos p -> (match p with
                    | XH -> ((Npos XH), N0)
                    | _ -> (N0, (Npos XH))))

  (** val add : n -> n -> n **)

  let add n0 m =
    match n0 with
    | N0 -> m
    | Npos p -> (match m with
                 | N0 -> n0
                 | Npos q0 -> Npos (Coq_Pos.add p q0))

  (** val mul : n -> n -> n **)

  let mul n0 m =
    match n0 with
    | N0 -> N0
    | Npos p -> (match m with
                 | N0 -> N0
                 | Npos q0 -> Npos (Coq_Pos.mul p q0))

  (** val div_eucl : n -> n -> n * n **)

  let div_eucl a b =
    match a with
    | N0 -> (N0, N0)
    | Npos na -> (match b with
                  | N0 -> (N0, a)
                  | Npos _ -> pos_div_eucl na b)

  (** val div : n -> n -> n **)

  let div a b =
    fst (div_eucl a b)

  (** val modulo : n -> n -> n **)

  let modulo a b =
    snd (div_eucl a b)

  (** val to_nat : n -> int **)

  let to_nat = function
  | N0 -> 0
  | Npos p -> Pos.to_nat p

  (** val of_nat : int -> n **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> N0)
      (fun n' -> Npos (Pos.of_succ_nat n'))
      n0

  (** val eq_dec : n -> n -> bool **)

  let eq_dec n0 m =
    match n0 with
    | N0 -> (match m with
             | N0 -> true
             | Npos _ -> false)
    | Npos p -> (match m with
                 | N0 -> false
                 | Npos p0 -> Coq_Pos.eq_dec p p0)
 end

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: l0 -> (f a) :: (map f l0)

(** val nth : int -> 'a1 list -> 'a1 -> 'a1 **)

let rec nth n0 l default =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> match l with
              | [] -> default
              | x :: _ -> x)
    (fun m -> match l with
              | [] -> default
              | _ :: l' -> nth m l' default)
    n0

(** val firstn : int -> 'a1 list -> 'a1 list **)

let rec firstn n0 l =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun n1 -> match l with
               | [] -> []
               | a :: l0 -> a :: (firstn n1 l0))
    n0

(** val skipn : int -> 'a1 list -> 'a1 list **)

let rec skipn n0 l =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> l)
    (fun n1 -> match l with
               | [] -> []
               | _ :: l0 -> skipn n1 l0)
    n0

(** val hd_error : 'a1 list -> 'a1 option **)

let hd_error = function
| [] -> None
| x :: _ -> Some x

(** val tl : 'a1 list -> 'a1 list **)

let tl = function
| [] -> []
| _ :: l' -> l'

(** val rev_append : 'a1 list -> 'a1 list -> 'a1 list **)

let rec rev_append l l' =
  match l with
  | [] -> l'
  | a :: l0 -> rev_append l0 (a :: l')

(** val list_eq_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

let rec list_eq_dec eq_dec0 l l' =
  match l with
  | [] -> (match l' with
           | [] -> true
           | _ :: _ -> false)
  | y :: l0 ->
    (match l' with
     | [] -> false
     | a :: l1 -> if eq_dec0 y a then list_eq_dec eq_dec0 l0 l1 else false)

(** val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1 **)

let rec fold_right f a0 = function
| [] -> a0
| b :: l0 -> f b (fold_right f a0 l0)

module Z =
 struct
  (** val of_nat : int -> z **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> Z0)
      (fun n1 -> Zpos (Pos.of_succ_nat n1))
      n0

  (** val to_pos : z -> positive **)

  let to_pos = function
  | Zpos p -> p
  | _ -> XH

  (** val sgn : z -> z **)

  let sgn = function
  | Z0 -> Z0
  | Zpos _ -> Zpos XH
  | Zneg _ -> Zneg XH

  (** val abs : z -> z **)

  let abs = function
  | Zneg p -> Zpos p
  | x -> x

  (** val ggcd : z -> z -> z * (z * z) **)

  let ggcd a b =
    match a with
    | Z0 -> ((abs b), (Z0, (sgn b)))
    | Zpos a0 ->
      (match b with
       | Z0 -> ((abs a), ((sgn a), Z0))
       | Zpos b0 ->
         let (g, p) = Coq_Pos.ggcd a0 b0 in
         let (aa, bb) = p in ((Zpos g), ((Zpos aa), (Zpos bb)))
       | Zneg b0 ->
         let (g, p) = Coq_Pos.ggcd a0 b0 in
         let (aa, bb) = p in ((Zpos g), ((Zpos aa), (Zneg bb))))
    | Zneg a0 ->
      (match b with
       | Z0 -> ((abs a), ((sgn a), Z0))
       | Zpos b0 ->
         let (g, p) = Coq_Pos.ggcd a0 b0 in
         let (aa, bb) = p in ((Zpos g), ((Zneg aa), (Zpos bb)))
       | Zneg b0 ->
         let (g, p) = Coq_Pos.ggcd a0 b0 in
         let (aa, bb) = p in ((Zpos g), ((Zneg aa), (Zneg bb))))
 end

(** val zero : char **)

let zero = '\000'

(** val one : char **)

let one = '\001'

(** val shift : bool -> char -> char **)

let shift = fun b c -> Char.chr (((Char.code c) lsl 1) land 255 + if b then 1 else 0)

(** val ascii_of_pos : positive -> char **)

let ascii_of_pos =
  let rec loop n0 p =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> zero)
      (fun n' ->
      match p with
      | XI p' -> shift true (loop n' p')
      | XO p' -> shift false (loop n' p')
      | XH -> one)
      n0
  in loop (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
       (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
       0))))))))

(** val ascii_of_N : n -> char **)

let ascii_of_N = function
| N0 -> zero
| Npos p -> ascii_of_pos p

(** val ascii_of_nat : int -> char **)

let ascii_of_nat a =
  ascii_of_N (N.of_nat a)

(** val n_of_digits : bool list -> n **)

let rec n_of_digits = function
| [] -> N0
| b :: l' ->
  N.add (if b then Npos XH else N0) (N.mul (Npos (XO XH)) (n_of_digits l'))

(** val n_of_ascii : char -> n **)

let n_of_ascii a =
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun a0 a1 a2 a3 a4 a5 a6 a7 ->
    n_of_digits
      (a0 :: (a1 :: (a2 :: (a3 :: (a4 :: (a5 :: (a6 :: (a7 :: [])))))))))
    a

(** val nat_of_ascii : char -> int **)

let nat_of_ascii a =
  N.to_nat (n_of_ascii a)

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

type decision = bool

(** val decide : decision -> bool **)

let decide decision0 =
  decision0

type ('a, 'b) relDecision = 'a -> 'b -> decision

(** val decide_rel : ('a1, 'a2) relDecision -> 'a1 -> 'a2 -> decision **)

let decide_rel relDecision0 =
  relDecision0

(** val zip_with : ('a1 -> 'a2 -> 'a3) -> 'a1 list -> 'a2 list -> 'a3 list **)

let rec zip_with f l1 l2 =
  match l1 with
  | [] -> []
  | x1 :: l3 ->
    (match l2 with
     | [] -> []
     | x2 :: l4 -> (f x1 x2) :: (zip_with f l3 l4))

(** val prod_map :
    ('a1 -> 'a2) -> ('a3 -> 'a4) -> ('a1 * 'a3) -> 'a2 * 'a4 **)

let prod_map f g p =
  ((f (fst p)), (g (snd p)))

type 'a empty = 'a

(** val empty0 : 'a1 empty -> 'a1 **)

let empty0 empty1 =
  empty1

type 'a union = 'a -> 'a -> 'a

(** val union0 : 'a1 union -> 'a1 -> 'a1 -> 'a1 **)

let union0 union1 =
  union1

(** val union_list : 'a1 empty -> 'a1 union -> 'a1 list -> 'a1 **)

let union_list h h0 =
  fold_right (union0 h0) (empty0 h)

type ('a, 'b) singleton = 'a -> 'b

(** val singleton0 : ('a1, 'a2) singleton -> 'a1 -> 'a2 **)

let singleton0 singleton1 =
  singleton1

(** val list_to_set :
    ('a1, 'a2) singleton -> 'a2 empty -> 'a2 union -> 'a1 list -> 'a2 **)

let rec list_to_set h h0 h1 = function
| [] -> empty0 h0
| x :: l0 -> union0 h1 (singleton0 h x) (list_to_set h h0 h1 l0)

type ('a, 'b) filter = __ -> ('a -> decision) -> 'b -> 'b

(** val filter0 : ('a1, 'a2) filter -> ('a1 -> decision) -> 'a2 -> 'a2 **)

let filter0 filter1 h x =
  filter1 __ h x

type 'm mRet = __ -> __ -> 'm

(** val mret : 'a1 mRet -> 'a2 -> 'a1 **)

let mret mRet0 x =
  Obj.magic mRet0 __ x

type 'm mBind = __ -> __ -> (__ -> 'm) -> 'm -> 'm

(** val mbind : 'a1 mBind -> ('a2 -> 'a1) -> 'a1 -> 'a1 **)

let mbind mBind0 x x0 =
  Obj.magic mBind0 __ __ x x0

type 'm fMap = __ -> __ -> (__ -> __) -> 'm -> 'm

(** val fmap : 'a1 fMap -> ('a2 -> 'a3) -> 'a1 -> 'a1 **)

let fmap fMap0 x x0 =
  Obj.magic fMap0 __ __ x x0

type 'm oMap = __ -> __ -> (__ -> __ option) -> 'm -> 'm

(** val omap : 'a1 oMap -> ('a2 -> 'a3 option) -> 'a1 -> 'a1 **)

let omap oMap0 x x0 =
  Obj.magic oMap0 __ __ x x0

type ('e, 'm) mThrow = __ -> 'e -> 'm

(** val mthrow : ('a1, 'a2) mThrow -> 'a1 -> 'a2 **)

let mthrow mThrow0 x =
  mThrow0 __ x

(** val guard_or : 'a1 -> ('a1, 'a2) mThrow -> 'a2 mRet -> decision -> 'a2 **)

let guard_or e h h0 h1 =
  if decide h1 then mret h0 __ else mthrow h e

type ('k, 'a, 'm) lookup = 'k -> 'm -> 'a option

(** val lookup0 : ('a1, 'a2, 'a3) lookup -> 'a1 -> 'a3 -> 'a2 option **)

let lookup0 lookup1 =
  lookup1

type ('k, 'a, 'm) singletonM = 'k -> 'a -> 'm

(** val singletonM0 : ('a1, 'a2, 'a3) singletonM -> 'a1 -> 'a2 -> 'a3 **)

let singletonM0 singletonM1 =
  singletonM1

type ('k, 'a, 'm) insert = 'k -> 'a -> 'm -> 'm

(** val insert0 : ('a1, 'a2, 'a3) insert -> 'a1 -> 'a2 -> 'a3 -> 'a3 **)

let insert0 insert1 =
  insert1

type ('k, 'a, 'm) partialAlter = ('a option -> 'a option) -> 'k -> 'm -> 'm

(** val partial_alter :
    ('a1, 'a2, 'a3) partialAlter -> ('a2 option -> 'a2 option) -> 'a1 -> 'a3
    -> 'a3 **)

let partial_alter partialAlter0 =
  partialAlter0

type 'm merge =
  __ -> __ -> __ -> (__ option -> __ option -> __ option) -> 'm -> 'm -> 'm

(** val merge0 :
    'a1 merge -> ('a2 option -> 'a3 option -> 'a4 option) -> 'a1 -> 'a1 -> 'a1 **)

let merge0 merge1 x x0 x1 =
  Obj.magic merge1 __ __ __ x x0 x1

type ('a, 'm) unionWith = ('a -> 'a -> 'a option) -> 'm -> 'm -> 'm

(** val union_with :
    ('a1, 'a2) unionWith -> ('a1 -> 'a1 -> 'a1 option) -> 'a2 -> 'a2 -> 'a2 **)

let union_with unionWith0 =
  unionWith0

type ('a, 'c) elements = 'c -> 'a list

(** val elements0 : ('a1, 'a2) elements -> 'a2 -> 'a1 list **)

let elements0 elements1 =
  elements1

type ('a, 'c) fresh = 'c -> 'a

(** val fresh0 : ('a1, 'a2) fresh -> 'a2 -> 'a1 **)

let fresh0 fresh1 =
  fresh1

type 'a infinite =
  ('a, 'a list) fresh
  (* singleton inductive, whose constructor was Build_Infinite *)

(** val not_dec : decision -> decision **)

let not_dec = function
| true -> false
| false -> true

(** val or_dec : decision -> decision -> decision **)

let or_dec p_dec q_dec =
  if p_dec then true else q_dec

(** val bool_eq_dec : (bool, bool) relDecision **)

let bool_eq_dec x y =
  if x then if y then true else false else if y then false else true

(** val unit_eq_dec : (unit, unit) relDecision **)

let unit_eq_dec _ _ =
  true

(** val prod_eq_dec :
    ('a1, 'a1) relDecision -> ('a2, 'a2) relDecision -> ('a1 * 'a2,
    'a1 * 'a2) relDecision **)

let prod_eq_dec eqDecision0 eqDecision1 x y =
  let (a, b) = x in
  let (a0, b0) = y in
  if decide_rel eqDecision0 a a0 then decide_rel eqDecision1 b b0 else false

(** val sum_eq_dec :
    ('a1, 'a1) relDecision -> ('a2, 'a2) relDecision -> (('a1, 'a2) sum,
    ('a1, 'a2) sum) relDecision **)

let sum_eq_dec eqDecision0 eqDecision1 x y =
  match x with
  | Inl a ->
    (match y with
     | Inl a0 -> decide_rel eqDecision0 a a0
     | Inr _ -> false)
  | Inr b ->
    (match y with
     | Inl _ -> false
     | Inr b0 -> decide_rel eqDecision1 b b0)

(** val bool_decide : decision -> bool **)

let bool_decide = function
| true -> true
| false -> false

type q = { qnum : z; qden : positive }

(** val qred : q -> q **)

let qred q0 =
  let { qnum = q1; qden = q2 } = q0 in
  let (r1, r2) = snd (Z.ggcd q1 (Zpos q2)) in
  { qnum = r1; qden = (Z.to_pos r2) }

type qc = q
  (* singleton inductive, whose constructor was Qcmake *)

(** val q2Qc : q -> qc **)

let q2Qc =
  qred

(** val from_option : ('a1 -> 'a2) -> 'a2 -> 'a1 option -> 'a2 **)

let from_option f y = function
| Some x -> f x
| None -> y

(** val some_dec : 'a1 option -> 'a1 option **)

let some_dec = function
| Some x -> Some x
| None -> None

(** val option_eq_dec :
    ('a1, 'a1) relDecision -> ('a1 option, 'a1 option) relDecision **)

let option_eq_dec dec mx my =
  match mx with
  | Some x ->
    (match my with
     | Some y -> decide (decide_rel dec x y)
     | None -> false)
  | None -> (match my with
             | Some _ -> false
             | None -> true)

(** val option_ret : __ -> __ option **)

let option_ret x =
  Some x

(** val option_bind : (__ -> __ option) -> __ option -> __ option **)

let option_bind f = function
| Some x -> f x
| None -> None

(** val option_fmap : (__ -> __) -> __ option -> __ option **)

let option_fmap =
  option_map

(** val option_mfail : unit -> __ option **)

let option_mfail _ =
  None

(** val option_union_with : ('a1, 'a1 option) unionWith **)

let option_union_with f mx my =
  match mx with
  | Some x -> (match my with
               | Some y -> f x y
               | None -> Some x)
  | None -> my

module Coq_Nat =
 struct
  (** val eq_dec : (int, int) relDecision **)

  let eq_dec =
    (=)
 end

module Coq0_Pos =
 struct
  (** val eq_dec : (positive, positive) relDecision **)

  let eq_dec =
    Coq_Pos.eq_dec

  (** val app : positive -> positive -> positive **)

  let rec app p1 = function
  | XI p3 -> XI (app p1 p3)
  | XO p3 -> XO (app p1 p3)
  | XH -> p1

  (** val reverse_go : positive -> positive -> positive **)

  let rec reverse_go p1 = function
  | XI p3 -> reverse_go (XI p1) p3
  | XO p3 -> reverse_go (XO p1) p3
  | XH -> p1

  (** val reverse : positive -> positive **)

  let reverse =
    reverse_go XH

  (** val dup : positive -> positive **)

  let rec dup = function
  | XI p' -> XI (XI (dup p'))
  | XO p' -> XO (XO (dup p'))
  | XH -> XH
 end

module Coq_N =
 struct
  (** val eq_dec : (n, n) relDecision **)

  let eq_dec =
    N.eq_dec

  (** val lt_dec : (n, n) relDecision **)

  let lt_dec x y =
    let filtered_var = N.compare x y in
    (match filtered_var with
     | Lt -> true
     | _ -> false)
 end

type qp = qc
  (* singleton inductive, whose constructor was mk_Qp *)

module Coq_list =
 struct
  (** val list_filter : ('a1 -> decision) -> 'a1 list -> 'a1 list **)

  let rec list_filter x = function
  | [] -> []
  | x0 :: l0 ->
    if decide (x x0)
    then x0 :: (filter0 (fun _ -> list_filter) x l0)
    else filter0 (fun _ -> list_filter) x l0

  (** val reverse : 'a1 list -> 'a1 list **)

  let reverse l =
    rev_append l []

  (** val list_elem_of_dec :
      ('a1, 'a1) relDecision -> ('a1, 'a1 list) relDecision **)

  let rec list_elem_of_dec dec x = function
  | [] -> false
  | y :: l0 ->
    if decide (decide_rel dec x y) then true else list_elem_of_dec dec x l0

  (** val list_eq_dec :
      ('a1, 'a1) relDecision -> ('a1 list, 'a1 list) relDecision **)

  let list_eq_dec =
    list_eq_dec
 end

module Coq0_list =
 struct
  (** val list_fmap : (__ -> __) -> __ list -> __ list **)

  let rec list_fmap f = function
  | [] -> []
  | x :: l0 -> (f x) :: (list_fmap f l0)

  (** val list_omap : (__ -> __ option) -> __ list -> __ list **)

  let rec list_omap f = function
  | [] -> []
  | x :: l0 ->
    (match f x with
     | Some y -> y :: (list_omap f l0)
     | None -> list_omap f l0)

  (** val list_bind : (__ -> __ list) -> __ list -> __ list **)

  let rec list_bind f = function
  | [] -> []
  | x :: l0 -> app (f x) (list_bind f l0)

  (** val mapM : 'a1 mBind -> 'a1 mRet -> ('a2 -> 'a1) -> 'a2 list -> 'a1 **)

  let rec mapM h h0 f = function
  | [] -> mret h0 []
  | x :: l0 ->
    mbind h (fun y -> mbind h (fun k -> mret h0 (y :: k)) (mapM h h0 f l0))
      (f x)
 end

module Coq1_list =
 struct
  (** val list_find : ('a1 -> decision) -> 'a1 list -> (int * 'a1) option **)

  let rec list_find h = function
  | [] -> None
  | x :: l0 ->
    if decide (h x)
    then Some (0, x)
    else fmap (Obj.magic (fun _ _ -> option_fmap))
           (prod_map (fun x0 -> Stdlib.Int.succ x0) id) (list_find h l0)

  (** val positives_flatten_go : positive list -> positive -> positive **)

  let rec positives_flatten_go xs acc =
    match xs with
    | [] -> acc
    | x :: xs0 ->
      positives_flatten_go xs0
        (Coq0_Pos.app (XO (XI acc)) (Coq0_Pos.reverse (Coq0_Pos.dup x)))

  (** val positives_flatten : positive list -> positive **)

  let positives_flatten xs =
    positives_flatten_go xs XH

  (** val positives_unflatten_go :
      positive -> positive list -> positive -> positive list option **)

  let rec positives_unflatten_go p acc_xs acc_elm =
    match p with
    | XI p0 ->
      (match p0 with
       | XI p' -> positives_unflatten_go p' acc_xs (XI acc_elm)
       | _ -> None)
    | XO p0 ->
      (match p0 with
       | XI p' -> positives_unflatten_go p' (acc_elm :: acc_xs) XH
       | XO p' -> positives_unflatten_go p' acc_xs (XO acc_elm)
       | XH -> None)
    | XH -> Some acc_xs

  (** val positives_unflatten : positive -> positive list option **)

  let positives_unflatten p =
    positives_unflatten_go p [] XH
 end

type 'a countable = { encode : ('a -> positive);
                      decode : (positive -> 'a option) }

(** val inj_countable :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> ('a2
    -> 'a1) -> ('a1 -> 'a2 option) -> 'a2 countable **)

let inj_countable _ h _ f g =
  { encode = (fun y -> h.encode (f y)); decode = (fun p ->
    mbind (Obj.magic (fun _ _ -> option_bind)) g ((Obj.magic h).decode p)) }

(** val unit_countable : unit countable **)

let unit_countable =
  { encode = (fun _ -> XH); decode = (fun _ -> Some ()) }

(** val option_countable :
    ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 option countable **)

let option_countable _ h =
  { encode = (fun o ->
    match o with
    | Some x -> Coq_Pos.succ (h.encode x)
    | None -> XH); decode = (fun p ->
    if decide (decide_rel Coq0_Pos.eq_dec p XH)
    then Some None
    else fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x -> Some x)
           ((Obj.magic h).decode (Coq_Pos.pred p))) }

(** val sum_countable :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> 'a2
    countable -> ('a1, 'a2) sum countable **)

let sum_countable _ h _ h0 =
  { encode = (fun xy ->
    match xy with
    | Inl x -> XO (h.encode x)
    | Inr y -> XI (h0.encode y)); decode = (fun p ->
    match p with
    | XI p0 ->
      let p1 = Obj.magic p0 in
      Obj.magic fmap (fun _ _ -> option_fmap) (fun x -> Inr x) (h0.decode p1)
    | XO p0 ->
      let p1 = Obj.magic p0 in
      Obj.magic fmap (fun _ _ -> option_fmap) (fun x -> Inl x) (h.decode p1)
    | XH -> None) }

(** val prod_encode_fst : positive -> positive **)

let rec prod_encode_fst = function
| XI p0 -> XI (XO (prod_encode_fst p0))
| XO p0 -> XO (XO (prod_encode_fst p0))
| XH -> XH

(** val prod_encode_snd : positive -> positive **)

let rec prod_encode_snd = function
| XI p0 -> XO (XI (prod_encode_snd p0))
| XO p0 -> XO (XO (prod_encode_snd p0))
| XH -> XO XH

(** val prod_encode : positive -> positive -> positive **)

let rec prod_encode p q0 =
  match p with
  | XI p0 ->
    (match q0 with
     | XI q1 -> XI (XI (prod_encode p0 q1))
     | XO q1 -> XI (XO (prod_encode p0 q1))
     | XH -> XI (XI (prod_encode_fst p0)))
  | XO p0 ->
    (match q0 with
     | XI q1 -> XO (XI (prod_encode p0 q1))
     | XO q1 -> XO (XO (prod_encode p0 q1))
     | XH -> XO (XI (prod_encode_fst p0)))
  | XH ->
    (match q0 with
     | XI q1 -> XI (XI (prod_encode_snd q1))
     | XO q1 -> XI (XO (prod_encode_snd q1))
     | XH -> XI XH)

(** val prod_decode_fst : positive -> positive option **)

let rec prod_decode_fst = function
| XI p0 ->
  (match p0 with
   | XI p1 ->
     Some (match prod_decode_fst p1 with
           | Some q0 -> XI q0
           | None -> XH)
   | XO p1 ->
     Some (match prod_decode_fst p1 with
           | Some q0 -> XI q0
           | None -> XH)
   | XH -> Some XH)
| XO p0 ->
  (match p0 with
   | XI p1 ->
     fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x -> XO x)
       (prod_decode_fst p1)
   | XO p1 ->
     fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x -> XO x)
       (prod_decode_fst p1)
   | XH -> None)
| XH -> Some XH

(** val prod_decode_snd : positive -> positive option **)

let rec prod_decode_snd = function
| XI p0 ->
  (match p0 with
   | XI p1 ->
     Some (match prod_decode_snd p1 with
           | Some q0 -> XI q0
           | None -> XH)
   | XO p1 ->
     fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x -> XO x)
       (prod_decode_snd p1)
   | XH -> Some XH)
| XO p0 ->
  (match p0 with
   | XI p1 ->
     Some (match prod_decode_snd p1 with
           | Some q0 -> XI q0
           | None -> XH)
   | XO p1 ->
     fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x -> XO x)
       (prod_decode_snd p1)
   | XH -> Some XH)
| XH -> None

(** val prod_countable :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> 'a2
    countable -> ('a1 * 'a2) countable **)

let prod_countable _ h _ h0 =
  { encode = (fun xy ->
    prod_encode (h.encode (fst xy)) (h0.encode (snd xy))); decode = (fun p ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun x ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun y -> Some (x, y))
        (mbind (Obj.magic (fun _ _ -> option_bind)) (Obj.magic h0).decode
          (Obj.magic prod_decode_snd p)))
      (mbind (Obj.magic (fun _ _ -> option_bind)) (Obj.magic h).decode
        (Obj.magic prod_decode_fst p))) }

(** val list_countable :
    ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 list countable **)

let list_countable _ h =
  { encode = (fun xs ->
    Coq1_list.positives_flatten
      (fmap (Obj.magic (fun _ _ -> Coq0_list.list_fmap)) h.encode
        (Obj.magic xs)));
    decode = (fun p ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun positives ->
      Coq0_list.mapM (Obj.magic (fun _ _ -> option_bind))
        (Obj.magic (fun _ -> option_ret)) (Obj.magic h).decode positives)
      (Obj.magic Coq1_list.positives_unflatten p)) }

(** val n_countable : n countable **)

let n_countable =
  { encode = (fun x -> match x with
                       | N0 -> XH
                       | Npos p -> Coq_Pos.succ p);
    decode = (fun p ->
    if decide (decide_rel Coq0_Pos.eq_dec p XH)
    then Some N0
    else Some (Npos (Coq_Pos.pred p))) }

(** val nat_countable : int countable **)

let nat_countable =
  { encode = (fun x -> n_countable.encode (N.of_nat x)); decode = (fun p ->
    fmap (Obj.magic (fun _ _ -> option_fmap)) N.to_nat
      ((Obj.magic n_countable).decode p)) }

type 't gen_tree =
| GenLeaf of 't
| GenNode of int * 't gen_tree list

(** val gen_tree_dec :
    ('a1, 'a1) relDecision -> ('a1 gen_tree, 'a1 gen_tree) relDecision **)

let rec gen_tree_dec eqDecision0 t1 t2 =
  match t1 with
  | GenLeaf x1 ->
    (match t2 with
     | GenLeaf x2 -> decide (decide_rel eqDecision0 x1 x2)
     | GenNode (_, _) -> false)
  | GenNode (n1, ts1) ->
    (match t2 with
     | GenLeaf _ -> false
     | GenNode (n2, ts2) ->
       if decide (decide_rel Coq_Nat.eq_dec n1 n2)
       then decide
              (decide_rel (Coq_list.list_eq_dec (gen_tree_dec eqDecision0))
                ts1 ts2)
       else false)

(** val gen_tree_to_list : 'a1 gen_tree -> (int * int, 'a1) sum list **)

let rec gen_tree_to_list = function
| GenLeaf x -> (Inr x) :: []
| GenNode (n0, ts) ->
  app
    (mbind (Obj.magic (fun _ _ -> Coq0_list.list_bind)) gen_tree_to_list
      (Obj.magic ts))
    ((Inl ((length ts), n0)) :: [])

(** val gen_tree_of_list :
    'a1 gen_tree list -> (int * int, 'a1) sum list -> 'a1 gen_tree option **)

let rec gen_tree_of_list k = function
| [] -> hd_error k
| s :: l0 ->
  (match s with
   | Inl p ->
     let (len, n0) = p in
     gen_tree_of_list ((GenNode (n0,
       (Coq_list.reverse (firstn len k)))) :: (skipn len k)) l0
   | Inr x -> gen_tree_of_list ((GenLeaf x) :: k) l0)

(** val gen_tree_countable :
    ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 gen_tree countable **)

let gen_tree_countable eqDecision0 h =
  inj_countable
    (Coq_list.list_eq_dec
      (sum_eq_dec (prod_eq_dec Coq_Nat.eq_dec Coq_Nat.eq_dec) eqDecision0))
    (list_countable
      (sum_eq_dec (prod_eq_dec Coq_Nat.eq_dec Coq_Nat.eq_dec) eqDecision0)
      (sum_countable (prod_eq_dec Coq_Nat.eq_dec Coq_Nat.eq_dec)
        (prod_countable Coq_Nat.eq_dec nat_countable Coq_Nat.eq_dec
          nat_countable)
        eqDecision0 h))
    (gen_tree_dec eqDecision0) gen_tree_to_list (gen_tree_of_list [])

(** val bool_cons_pos : bool -> positive -> positive **)

let bool_cons_pos b p =
  if b then XI p else XO p

(** val ascii_cons_pos : char -> positive -> positive **)

let ascii_cons_pos c p =
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun b0 b1 b2 b3 b4 b5 b6 b7 ->
    bool_cons_pos b0
      (bool_cons_pos b1
        (bool_cons_pos b2
          (bool_cons_pos b3
            (bool_cons_pos b4
              (bool_cons_pos b5 (bool_cons_pos b6 (bool_cons_pos b7 p))))))))
    c

(** val string_to_pos : char list -> positive **)

let rec string_to_pos = function
| [] -> XH
| c::s0 -> ascii_cons_pos c (string_to_pos s0)

(** val pos_to_string : positive -> char list **)

let rec pos_to_string = function
| XI p0 ->
  (match p0 with
   | XI p1 ->
     (match p1 with
      | XI p2 ->
        (match p2 with
         | XI p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\255'::(pos_to_string p7)
                     | XO p7 -> '\127'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\191'::(pos_to_string p7)
                     | XO p7 -> '?'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\223'::(pos_to_string p7)
                     | XO p7 -> '_'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\159'::(pos_to_string p7)
                     | XO p7 -> '\031'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\239'::(pos_to_string p7)
                     | XO p7 -> 'o'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\175'::(pos_to_string p7)
                     | XO p7 -> '/'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\207'::(pos_to_string p7)
                     | XO p7 -> 'O'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\143'::(pos_to_string p7)
                     | XO p7 -> '\015'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XO p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\247'::(pos_to_string p7)
                     | XO p7 -> 'w'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\183'::(pos_to_string p7)
                     | XO p7 -> '7'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\215'::(pos_to_string p7)
                     | XO p7 -> 'W'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\151'::(pos_to_string p7)
                     | XO p7 -> '\023'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\231'::(pos_to_string p7)
                     | XO p7 -> 'g'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\167'::(pos_to_string p7)
                     | XO p7 -> '\''::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\199'::(pos_to_string p7)
                     | XO p7 -> 'G'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\135'::(pos_to_string p7)
                     | XO p7 -> '\007'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XH -> [])
      | XO p2 ->
        (match p2 with
         | XI p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\251'::(pos_to_string p7)
                     | XO p7 -> '{'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\187'::(pos_to_string p7)
                     | XO p7 -> ';'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\219'::(pos_to_string p7)
                     | XO p7 -> '['::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\155'::(pos_to_string p7)
                     | XO p7 -> '\027'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\235'::(pos_to_string p7)
                     | XO p7 -> 'k'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\171'::(pos_to_string p7)
                     | XO p7 -> '+'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\203'::(pos_to_string p7)
                     | XO p7 -> 'K'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\139'::(pos_to_string p7)
                     | XO p7 -> '\011'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XO p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\243'::(pos_to_string p7)
                     | XO p7 -> 's'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\179'::(pos_to_string p7)
                     | XO p7 -> '3'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\211'::(pos_to_string p7)
                     | XO p7 -> 'S'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\147'::(pos_to_string p7)
                     | XO p7 -> '\019'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\227'::(pos_to_string p7)
                     | XO p7 -> 'c'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\163'::(pos_to_string p7)
                     | XO p7 -> '#'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\195'::(pos_to_string p7)
                     | XO p7 -> 'C'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\131'::(pos_to_string p7)
                     | XO p7 -> '\003'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XH -> [])
      | XH -> [])
   | XO p1 ->
     (match p1 with
      | XI p2 ->
        (match p2 with
         | XI p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\253'::(pos_to_string p7)
                     | XO p7 -> '}'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\189'::(pos_to_string p7)
                     | XO p7 -> '='::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\221'::(pos_to_string p7)
                     | XO p7 -> ']'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\157'::(pos_to_string p7)
                     | XO p7 -> '\029'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\237'::(pos_to_string p7)
                     | XO p7 -> 'm'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\173'::(pos_to_string p7)
                     | XO p7 -> '-'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\205'::(pos_to_string p7)
                     | XO p7 -> 'M'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\141'::(pos_to_string p7)
                     | XO p7 -> '\r'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XO p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\245'::(pos_to_string p7)
                     | XO p7 -> 'u'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\181'::(pos_to_string p7)
                     | XO p7 -> '5'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\213'::(pos_to_string p7)
                     | XO p7 -> 'U'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\149'::(pos_to_string p7)
                     | XO p7 -> '\021'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\229'::(pos_to_string p7)
                     | XO p7 -> 'e'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\165'::(pos_to_string p7)
                     | XO p7 -> '%'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\197'::(pos_to_string p7)
                     | XO p7 -> 'E'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\133'::(pos_to_string p7)
                     | XO p7 -> '\005'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XH -> [])
      | XO p2 ->
        (match p2 with
         | XI p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\249'::(pos_to_string p7)
                     | XO p7 -> 'y'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\185'::(pos_to_string p7)
                     | XO p7 -> '9'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\217'::(pos_to_string p7)
                     | XO p7 -> 'Y'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\153'::(pos_to_string p7)
                     | XO p7 -> '\025'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\233'::(pos_to_string p7)
                     | XO p7 -> 'i'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\169'::(pos_to_string p7)
                     | XO p7 -> ')'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\201'::(pos_to_string p7)
                     | XO p7 -> 'I'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\137'::(pos_to_string p7)
                     | XO p7 -> '\t'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XO p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\241'::(pos_to_string p7)
                     | XO p7 -> 'q'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\177'::(pos_to_string p7)
                     | XO p7 -> '1'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\209'::(pos_to_string p7)
                     | XO p7 -> 'Q'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\145'::(pos_to_string p7)
                     | XO p7 -> '\017'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\225'::(pos_to_string p7)
                     | XO p7 -> 'a'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\161'::(pos_to_string p7)
                     | XO p7 -> '!'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\193'::(pos_to_string p7)
                     | XO p7 -> 'A'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\129'::(pos_to_string p7)
                     | XO p7 -> '\001'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XH -> [])
      | XH -> [])
   | XH -> [])
| XO p0 ->
  (match p0 with
   | XI p1 ->
     (match p1 with
      | XI p2 ->
        (match p2 with
         | XI p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\254'::(pos_to_string p7)
                     | XO p7 -> '~'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\190'::(pos_to_string p7)
                     | XO p7 -> '>'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\222'::(pos_to_string p7)
                     | XO p7 -> '^'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\158'::(pos_to_string p7)
                     | XO p7 -> '\030'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\238'::(pos_to_string p7)
                     | XO p7 -> 'n'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\174'::(pos_to_string p7)
                     | XO p7 -> '.'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\206'::(pos_to_string p7)
                     | XO p7 -> 'N'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\142'::(pos_to_string p7)
                     | XO p7 -> '\014'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XO p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\246'::(pos_to_string p7)
                     | XO p7 -> 'v'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\182'::(pos_to_string p7)
                     | XO p7 -> '6'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\214'::(pos_to_string p7)
                     | XO p7 -> 'V'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\150'::(pos_to_string p7)
                     | XO p7 -> '\022'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\230'::(pos_to_string p7)
                     | XO p7 -> 'f'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\166'::(pos_to_string p7)
                     | XO p7 -> '&'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\198'::(pos_to_string p7)
                     | XO p7 -> 'F'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\134'::(pos_to_string p7)
                     | XO p7 -> '\006'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XH -> [])
      | XO p2 ->
        (match p2 with
         | XI p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\250'::(pos_to_string p7)
                     | XO p7 -> 'z'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\186'::(pos_to_string p7)
                     | XO p7 -> ':'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\218'::(pos_to_string p7)
                     | XO p7 -> 'Z'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\154'::(pos_to_string p7)
                     | XO p7 -> '\026'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\234'::(pos_to_string p7)
                     | XO p7 -> 'j'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\170'::(pos_to_string p7)
                     | XO p7 -> '*'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\202'::(pos_to_string p7)
                     | XO p7 -> 'J'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\138'::(pos_to_string p7)
                     | XO p7 -> '\n'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XO p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\242'::(pos_to_string p7)
                     | XO p7 -> 'r'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\178'::(pos_to_string p7)
                     | XO p7 -> '2'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\210'::(pos_to_string p7)
                     | XO p7 -> 'R'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\146'::(pos_to_string p7)
                     | XO p7 -> '\018'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\226'::(pos_to_string p7)
                     | XO p7 -> 'b'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\162'::(pos_to_string p7)
                     | XO p7 -> '"'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\194'::(pos_to_string p7)
                     | XO p7 -> 'B'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\130'::(pos_to_string p7)
                     | XO p7 -> '\002'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XH -> [])
      | XH -> [])
   | XO p1 ->
     (match p1 with
      | XI p2 ->
        (match p2 with
         | XI p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\252'::(pos_to_string p7)
                     | XO p7 -> '|'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\188'::(pos_to_string p7)
                     | XO p7 -> '<'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\220'::(pos_to_string p7)
                     | XO p7 -> '\\'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\156'::(pos_to_string p7)
                     | XO p7 -> '\028'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\236'::(pos_to_string p7)
                     | XO p7 -> 'l'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\172'::(pos_to_string p7)
                     | XO p7 -> ','::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\204'::(pos_to_string p7)
                     | XO p7 -> 'L'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\140'::(pos_to_string p7)
                     | XO p7 -> '\012'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XO p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\244'::(pos_to_string p7)
                     | XO p7 -> 't'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\180'::(pos_to_string p7)
                     | XO p7 -> '4'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\212'::(pos_to_string p7)
                     | XO p7 -> 'T'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\148'::(pos_to_string p7)
                     | XO p7 -> '\020'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\228'::(pos_to_string p7)
                     | XO p7 -> 'd'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\164'::(pos_to_string p7)
                     | XO p7 -> '$'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\196'::(pos_to_string p7)
                     | XO p7 -> 'D'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\132'::(pos_to_string p7)
                     | XO p7 -> '\004'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XH -> [])
      | XO p2 ->
        (match p2 with
         | XI p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\248'::(pos_to_string p7)
                     | XO p7 -> 'x'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\184'::(pos_to_string p7)
                     | XO p7 -> '8'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\216'::(pos_to_string p7)
                     | XO p7 -> 'X'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\152'::(pos_to_string p7)
                     | XO p7 -> '\024'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\232'::(pos_to_string p7)
                     | XO p7 -> 'h'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\168'::(pos_to_string p7)
                     | XO p7 -> '('::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\200'::(pos_to_string p7)
                     | XO p7 -> 'H'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\136'::(pos_to_string p7)
                     | XO p7 -> '\b'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XO p3 ->
           (match p3 with
            | XI p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\240'::(pos_to_string p7)
                     | XO p7 -> 'p'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\176'::(pos_to_string p7)
                     | XO p7 -> '0'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\208'::(pos_to_string p7)
                     | XO p7 -> 'P'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\144'::(pos_to_string p7)
                     | XO p7 -> '\016'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XO p4 ->
              (match p4 with
               | XI p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\224'::(pos_to_string p7)
                     | XO p7 -> '`'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\160'::(pos_to_string p7)
                     | XO p7 -> ' '::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XO p5 ->
                 (match p5 with
                  | XI p6 ->
                    (match p6 with
                     | XI p7 -> '\192'::(pos_to_string p7)
                     | XO p7 -> '@'::(pos_to_string p7)
                     | XH -> [])
                  | XO p6 ->
                    (match p6 with
                     | XI p7 -> '\128'::(pos_to_string p7)
                     | XO p7 -> '\000'::(pos_to_string p7)
                     | XH -> [])
                  | XH -> [])
               | XH -> [])
            | XH -> [])
         | XH -> [])
      | XH -> [])
   | XH -> [])
| XH -> []

module Ascii =
 struct
  (** val eq_dec : (char, char) relDecision **)

  let eq_dec =
    (=)
 end

module String =
 struct
  (** val eq_dec : (char list, char list) relDecision **)

  let rec eq_dec s x0 =
    match s with
    | [] -> (match x0 with
             | [] -> true
             | _::_ -> false)
    | a::s0 ->
      (match x0 with
       | [] -> false
       | a0::s1 ->
         if decide_rel Ascii.eq_dec a a0 then eq_dec s0 s1 else false)

  (** val countable : char list countable **)

  let countable =
    { encode = string_to_pos; decode = (fun p -> Some (pos_to_string p)) }
 end

type 'a pretty = 'a -> char list

(** val pretty0 : 'a1 pretty -> 'a1 -> char list **)

let pretty0 pretty1 =
  pretty1

(** val pretty_N_char : n -> char **)

let pretty_N_char = function
| N0 -> '0'
| Npos p ->
  (match p with
   | XI p0 ->
     (match p0 with
      | XI p1 -> (match p1 with
                  | XH -> '7'
                  | _ -> '9')
      | XO p1 -> (match p1 with
                  | XH -> '5'
                  | _ -> '9')
      | XH -> '3')
   | XO p0 ->
     (match p0 with
      | XI p1 -> (match p1 with
                  | XH -> '6'
                  | _ -> '9')
      | XO p1 ->
        (match p1 with
         | XI _ -> '9'
         | XO p2 -> (match p2 with
                     | XH -> '8'
                     | _ -> '9')
         | XH -> '4')
      | XH -> '2')
   | XH -> '1')

(** val pretty_N_go_help : n -> char list -> char list **)

let rec pretty_N_go_help x s =
  if decide (decide_rel Coq_N.lt_dec N0 x)
  then pretty_N_go_help (N.div x (Npos (XO (XI (XO XH)))))
         ((pretty_N_char (N.modulo x (Npos (XO (XI (XO XH))))))::s)
  else s

(** val pretty_N_go : n -> char list -> char list **)

let pretty_N_go =
  pretty_N_go_help

(** val pretty_N : n pretty **)

let pretty_N x =
  if decide (decide_rel Coq_N.eq_dec x N0) then '0'::[] else pretty_N_go x []

(** val pretty_nat : int pretty **)

let pretty_nat x =
  pretty0 pretty_N (N.of_nat x)

(** val pretty_positive : positive pretty **)

let pretty_positive x =
  pretty0 pretty_N (Npos x)

(** val pretty_Z : z pretty **)

let pretty_Z = function
| Z0 -> '0'::[]
| Zpos x0 -> pretty0 pretty_positive x0
| Zneg x0 -> append ('-'::[]) (pretty0 pretty_positive x0)

(** val search_infinite_go :
    (int -> 'a1) -> ('a1, 'a1) relDecision -> 'a1 list -> int -> (int -> __
    -> 'a1) -> 'a1 **)

let search_infinite_go f eqDecision0 xs n0 go =
  let x = f n0 in
  if decide (decide_rel (Coq_list.list_elem_of_dec eqDecision0) x xs)
  then go (Stdlib.Int.succ n0) __
  else x

(** val search_infinite :
    (int -> 'a1) -> ('a1, 'a1) relDecision -> 'a1 infinite **)

let search_infinite f eqDecision0 xs =
  let rec fix_F x =
    search_infinite_go f eqDecision0 xs x (fun y _ -> fix_F y)
  in fix_F 0

(** val string_infinite : char list infinite **)

let string_infinite =
  search_infinite (pretty0 pretty_nat) String.eq_dec

(** val set_filter :
    ('a1, 'a2) elements -> 'a2 empty -> ('a1, 'a2) singleton -> 'a2 union ->
    ('a1 -> decision) -> 'a2 -> 'a2 **)

let set_filter h h0 h1 h2 h3 x =
  list_to_set h1 h0 h2
    (filter0 (fun _ -> Coq_list.list_filter) h3 (elements0 h x))

(** val set_map :
    ('a1, 'a2) elements -> ('a3, 'a4) singleton -> 'a4 empty -> 'a4 union ->
    ('a1 -> 'a3) -> 'a2 -> 'a4 **)

let set_map h h0 h1 h2 f x =
  list_to_set (Obj.magic h0) h1 h2
    (fmap (Obj.magic (fun _ _ -> Coq0_list.list_fmap)) f (elements0 h x))

(** val set_bind :
    ('a1, 'a2) elements -> 'a3 empty -> 'a3 union -> ('a1 -> 'a3) -> 'a2 ->
    'a3 **)

let set_bind h h0 h1 f x =
  union_list h0 h1
    (fmap (Obj.magic (fun _ _ -> Coq0_list.list_fmap)) f
      (elements0 (Obj.magic h) x))

(** val set_fresh :
    ('a1, 'a2) elements -> ('a1, 'a1 list) fresh -> ('a1, 'a2) fresh **)

let set_fresh h h0 =
  compose (fresh0 h0) (elements0 h)

type ('k, 'a, 'm) mapFold = __ -> ('k -> 'a -> __ -> __) -> __ -> 'm -> __

(** val map_fold :
    ('a1, 'a2, 'a3) mapFold -> ('a1 -> 'a2 -> 'a4 -> 'a4) -> 'a4 -> 'a3 -> 'a4 **)

let map_fold mapFold0 x x0 x1 =
  Obj.magic mapFold0 __ x x0 x1

(** val map_insert :
    ('a1, 'a2, 'a3) partialAlter -> ('a1, 'a2, 'a3) insert **)

let map_insert h i x =
  partial_alter h (fun _ -> Some x) i

(** val map_singleton :
    ('a1, 'a2, 'a3) partialAlter -> 'a3 empty -> ('a1, 'a2, 'a3) singletonM **)

let map_singleton h h0 i x =
  insert0 (map_insert h) i x (empty0 h0)

(** val list_to_map :
    ('a1, 'a2, 'a3) insert -> 'a3 empty -> ('a1 * 'a2) list -> 'a3 **)

let list_to_map h h0 =
  fold_right (fun p -> insert0 h (fst p) (snd p)) (empty0 h0)

(** val map_to_list : ('a1, 'a2, 'a3) mapFold -> 'a3 -> ('a1 * 'a2) list **)

let map_to_list h =
  map_fold h (fun i x x0 -> (i, x) :: x0) []

(** val map_to_set :
    ('a1, 'a2, 'a3) mapFold -> ('a4, 'a5) singleton -> 'a5 empty -> 'a5 union
    -> ('a1 -> 'a2 -> 'a4) -> 'a3 -> 'a5 **)

let map_to_set h h0 h1 h2 f m =
  list_to_set (Obj.magic h0) h1 h2
    (fmap (Obj.magic (fun _ _ -> Coq0_list.list_fmap)) (uncurry f)
      (map_to_list h m))

(** val set_to_map :
    ('a1, 'a2) elements -> ('a3, 'a4, 'a5) insert -> 'a5 empty -> ('a1 ->
    'a3 * 'a4) -> 'a2 -> 'a5 **)

let set_to_map h h0 h1 f x =
  list_to_map h0 h1
    (fmap (Obj.magic (fun _ _ -> Coq0_list.list_fmap)) f
      (elements0 (Obj.magic h) x))

(** val map_union_with : 'a1 merge -> ('a2, 'a1) unionWith **)

let map_union_with h f =
  merge0 h (union_with option_union_with f)

(** val map_union : 'a1 merge -> 'a1 union **)

let map_union h =
  union_with (map_union_with h) (fun x _ -> Some x)

(** val map_img :
    ('a1, 'a2, 'a3) mapFold -> ('a2, 'a4) singleton -> 'a4 empty -> 'a4 union
    -> 'a3 -> 'a4 **)

let map_img h h0 h1 h2 =
  map_to_set h h0 h1 h2 (fun _ x -> x)

type 'munit mapset' =
  'munit
  (* singleton inductive, whose constructor was Mapset *)

(** val mapset_empty : (__ -> 'a1 empty) -> 'a1 mapset' empty **)

let mapset_empty h1 =
  empty0 (h1 __)

(** val mapset_singleton :
    (__ -> 'a2 empty) -> (__ -> ('a1, __, 'a2) partialAlter) -> ('a1, 'a2
    mapset') singleton **)

let mapset_singleton h1 h2 x =
  singletonM0 (map_singleton (Obj.magic h2 __) (h1 __)) x ()

(** val mapset_union : 'a1 merge -> 'a1 mapset' union **)

let mapset_union h4 x1 x2 =
  union0 (map_union h4) x1 x2

(** val mapset_elements :
    (__ -> ('a1, __, 'a2) mapFold) -> ('a1, 'a2 mapset') elements **)

let mapset_elements h5 x =
  fmap (Obj.magic (fun _ _ -> Coq0_list.list_fmap)) fst
    (Obj.magic map_to_list (h5 __) x)

(** val mapset_eq_dec :
    ('a1, 'a1) relDecision -> ('a1 mapset', 'a1 mapset') relDecision **)

let mapset_eq_dec eqDecision1 x1 x2 =
  decide (decide_rel eqDecision1 x1 x2)

(** val mapset_elem_of_dec :
    (__ -> ('a1, __, 'a2) lookup) -> ('a1, 'a2 mapset') relDecision **)

let mapset_elem_of_dec h0 x x0 =
  decide
    (decide_rel (Obj.magic option_eq_dec unit_eq_dec) (lookup0 (h0 __) x x0)
      (Some ()))

type 'a gmap_dep_ne =
| GNode001 of 'a gmap_dep_ne
| GNode010 of 'a
| GNode011 of 'a * 'a gmap_dep_ne
| GNode100 of 'a gmap_dep_ne
| GNode101 of 'a gmap_dep_ne * 'a gmap_dep_ne
| GNode110 of 'a gmap_dep_ne * 'a
| GNode111 of 'a gmap_dep_ne * 'a * 'a gmap_dep_ne

type 'a gmap_dep =
| GEmpty
| GNodes of 'a gmap_dep_ne

type ('k, 'a) gmap =
  'a gmap_dep
  (* singleton inductive, whose constructor was GMap *)

(** val gmap_dep_ne_eq_dec :
    ('a1, 'a1) relDecision -> ('a1 gmap_dep_ne, 'a1 gmap_dep_ne) relDecision **)

let rec gmap_dep_ne_eq_dec x t1 t2 =
  match t1 with
  | GNode001 r1 ->
    (match t2 with
     | GNode001 r2 -> gmap_dep_ne_eq_dec x r1 r2
     | _ -> false)
  | GNode010 x1 ->
    (match t2 with
     | GNode010 x2 -> decide (decide_rel x x1 x2)
     | _ -> false)
  | GNode011 (x1, r1) ->
    (match t2 with
     | GNode011 (x2, r2) ->
       if decide (decide_rel x x1 x2)
       then gmap_dep_ne_eq_dec x r1 r2
       else false
     | _ -> false)
  | GNode100 l1 ->
    (match t2 with
     | GNode100 l2 -> gmap_dep_ne_eq_dec x l1 l2
     | _ -> false)
  | GNode101 (l1, r1) ->
    (match t2 with
     | GNode101 (l2, r2) ->
       if gmap_dep_ne_eq_dec x l1 l2
       then gmap_dep_ne_eq_dec x r1 r2
       else false
     | _ -> false)
  | GNode110 (l1, x1) ->
    (match t2 with
     | GNode110 (l2, x2) ->
       if gmap_dep_ne_eq_dec x l1 l2
       then decide (decide_rel x x1 x2)
       else false
     | _ -> false)
  | GNode111 (l1, x1, r1) ->
    (match t2 with
     | GNode111 (l2, x2, r2) ->
       if gmap_dep_ne_eq_dec x l1 l2
       then if decide (decide_rel x x1 x2)
            then gmap_dep_ne_eq_dec x r1 r2
            else false
       else false
     | _ -> false)

(** val gmap_dep_eq_dec :
    ('a1, 'a1) relDecision -> ('a1 gmap_dep, 'a1 gmap_dep) relDecision **)

let gmap_dep_eq_dec x x0 y =
  match x0 with
  | GEmpty -> (match y with
               | GEmpty -> true
               | GNodes _ -> false)
  | GNodes g ->
    (match y with
     | GEmpty -> false
     | GNodes g0 -> decide_rel (gmap_dep_ne_eq_dec x) g g0)

(** val gmap_eq_dec :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision ->
    (('a1, 'a2) gmap, ('a1, 'a2) gmap) relDecision **)

let gmap_eq_dec _ _ x x0 y =
  decide_rel (gmap_dep_eq_dec x) x0 y

(** val gNode :
    'a1 gmap_dep -> (__ * 'a1) option -> 'a1 gmap_dep -> 'a1 gmap_dep **)

let gNode ml mx mr =
  match ml with
  | GEmpty ->
    (match mx with
     | Some p0 ->
       let (_, x) = p0 in
       (match mr with
        | GEmpty -> GNodes (GNode010 x)
        | GNodes r -> GNodes (GNode011 (x, r)))
     | None ->
       (match mr with
        | GEmpty -> GEmpty
        | GNodes r -> GNodes (GNode001 r)))
  | GNodes l ->
    (match mx with
     | Some p0 ->
       let (_, x) = p0 in
       (match mr with
        | GEmpty -> GNodes (GNode110 (l, x))
        | GNodes r -> GNodes (GNode111 (l, x, r)))
     | None ->
       (match mr with
        | GEmpty -> GNodes (GNode100 l)
        | GNodes r -> GNodes (GNode101 (l, r))))

(** val gmap_dep_ne_case :
    'a1 gmap_dep_ne -> ('a1 gmap_dep -> (__ * 'a1) option -> 'a1 gmap_dep ->
    'a2) -> 'a2 **)

let gmap_dep_ne_case t f =
  match t with
  | GNode001 r -> f GEmpty None (GNodes r)
  | GNode010 x -> f GEmpty (Some (__, x)) GEmpty
  | GNode011 (x, r) -> f GEmpty (Some (__, x)) (GNodes r)
  | GNode100 l -> f (GNodes l) None GEmpty
  | GNode101 (l, r) -> f (GNodes l) None (GNodes r)
  | GNode110 (l, x) -> f (GNodes l) (Some (__, x)) GEmpty
  | GNode111 (l, x, r) -> f (GNodes l) (Some (__, x)) (GNodes r)

(** val gmap_dep_ne_lookup : positive -> 'a1 gmap_dep_ne -> 'a1 option **)

let rec gmap_dep_ne_lookup i = function
| GNode001 r -> (match i with
                 | XI i0 -> gmap_dep_ne_lookup i0 r
                 | _ -> None)
| GNode010 x -> (match i with
                 | XH -> Some x
                 | _ -> None)
| GNode011 (x, r) ->
  (match i with
   | XI i0 -> gmap_dep_ne_lookup i0 r
   | XO _ -> None
   | XH -> Some x)
| GNode100 l -> (match i with
                 | XO i0 -> gmap_dep_ne_lookup i0 l
                 | _ -> None)
| GNode101 (l, r) ->
  (match i with
   | XI i0 -> gmap_dep_ne_lookup i0 r
   | XO i0 -> gmap_dep_ne_lookup i0 l
   | XH -> None)
| GNode110 (l, x) ->
  (match i with
   | XI _ -> None
   | XO i0 -> gmap_dep_ne_lookup i0 l
   | XH -> Some x)
| GNode111 (l, x, r) ->
  (match i with
   | XI i0 -> gmap_dep_ne_lookup i0 r
   | XO i0 -> gmap_dep_ne_lookup i0 l
   | XH -> Some x)

(** val gmap_dep_lookup : positive -> 'a1 gmap_dep -> 'a1 option **)

let gmap_dep_lookup i = function
| GEmpty -> None
| GNodes t -> gmap_dep_ne_lookup i t

(** val gmap_lookup :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2, ('a1, 'a2) gmap)
    lookup **)

let gmap_lookup _ h k mt =
  gmap_dep_lookup (h.encode k) mt

(** val gmap_empty :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2) gmap empty **)

let gmap_empty _ _ =
  GEmpty

(** val gmap_dep_ne_singleton : positive -> 'a1 -> 'a1 gmap_dep_ne **)

let rec gmap_dep_ne_singleton i x =
  match i with
  | XI i0 -> GNode001 (gmap_dep_ne_singleton i0 x)
  | XO i0 -> GNode100 (gmap_dep_ne_singleton i0 x)
  | XH -> GNode010 x

(** val gmap_partial_alter_aux :
    (positive -> __ -> 'a1 gmap_dep_ne -> 'a1 gmap_dep) -> ('a1 option -> 'a1
    option) -> positive -> 'a1 gmap_dep -> 'a1 gmap_dep **)

let gmap_partial_alter_aux go f i = function
| GEmpty ->
  (match f None with
   | Some x -> GNodes (gmap_dep_ne_singleton i x)
   | None -> GEmpty)
| GNodes t -> go i __ t

(** val gmap_dep_ne_partial_alter :
    ('a1 option -> 'a1 option) -> positive -> 'a1 gmap_dep_ne -> 'a1 gmap_dep **)

let rec gmap_dep_ne_partial_alter f i = function
| GNode001 r ->
  (match i with
   | XI i0 ->
     (match gmap_dep_ne_partial_alter f i0 r with
      | GEmpty -> GEmpty
      | GNodes r0 -> GNodes (GNode001 r0))
   | XO i0 ->
     (match f None with
      | Some x0 ->
        let l = gmap_dep_ne_singleton i0 x0 in GNodes (GNode101 (l, r))
      | None -> GNodes (GNode001 r))
   | XH ->
     (match f None with
      | Some a ->
        let p0 = (__, a) in let (_, x0) = p0 in GNodes (GNode011 (x0, r))
      | None -> GNodes (GNode001 r)))
| GNode010 x0 ->
  (match i with
   | XI i0 ->
     (match f None with
      | Some x1 ->
        let r = gmap_dep_ne_singleton i0 x1 in GNodes (GNode011 (x0, r))
      | None -> GNodes (GNode010 x0))
   | XO i0 ->
     (match f None with
      | Some x1 ->
        let l = gmap_dep_ne_singleton i0 x1 in GNodes (GNode110 (l, x0))
      | None -> GNodes (GNode010 x0))
   | XH ->
     (match f (Some x0) with
      | Some a -> let p0 = (__, a) in let (_, x1) = p0 in GNodes (GNode010 x1)
      | None -> GEmpty))
| GNode011 (x0, r) ->
  (match i with
   | XI i0 ->
     (match gmap_dep_ne_partial_alter f i0 r with
      | GEmpty -> GNodes (GNode010 x0)
      | GNodes r0 -> GNodes (GNode011 (x0, r0)))
   | XO i0 ->
     (match f None with
      | Some x1 ->
        let l = gmap_dep_ne_singleton i0 x1 in GNodes (GNode111 (l, x0, r))
      | None -> GNodes (GNode011 (x0, r)))
   | XH ->
     (match f (Some x0) with
      | Some a ->
        let p0 = (__, a) in let (_, x1) = p0 in GNodes (GNode011 (x1, r))
      | None -> GNodes (GNode001 r)))
| GNode100 l ->
  (match i with
   | XI i0 ->
     (match f None with
      | Some x0 ->
        let r = gmap_dep_ne_singleton i0 x0 in GNodes (GNode101 (l, r))
      | None -> GNodes (GNode100 l))
   | XO i0 ->
     (match gmap_dep_ne_partial_alter f i0 l with
      | GEmpty -> GEmpty
      | GNodes l0 -> GNodes (GNode100 l0))
   | XH ->
     (match f None with
      | Some a ->
        let p0 = (__, a) in let (_, x0) = p0 in GNodes (GNode110 (l, x0))
      | None -> GNodes (GNode100 l)))
| GNode101 (l, r) ->
  (match i with
   | XI i0 ->
     (match gmap_dep_ne_partial_alter f i0 r with
      | GEmpty -> GNodes (GNode100 l)
      | GNodes r0 -> GNodes (GNode101 (l, r0)))
   | XO i0 ->
     (match gmap_dep_ne_partial_alter f i0 l with
      | GEmpty -> GNodes (GNode001 r)
      | GNodes l0 -> GNodes (GNode101 (l0, r)))
   | XH ->
     (match f None with
      | Some a ->
        let p0 = (__, a) in let (_, x0) = p0 in GNodes (GNode111 (l, x0, r))
      | None -> GNodes (GNode101 (l, r))))
| GNode110 (l, x0) ->
  (match i with
   | XI i0 ->
     (match f None with
      | Some x1 ->
        let r = gmap_dep_ne_singleton i0 x1 in GNodes (GNode111 (l, x0, r))
      | None -> GNodes (GNode110 (l, x0)))
   | XO i0 ->
     (match gmap_dep_ne_partial_alter f i0 l with
      | GEmpty -> GNodes (GNode010 x0)
      | GNodes l0 -> GNodes (GNode110 (l0, x0)))
   | XH ->
     (match f (Some x0) with
      | Some a ->
        let p0 = (__, a) in let (_, x1) = p0 in GNodes (GNode110 (l, x1))
      | None -> GNodes (GNode100 l)))
| GNode111 (l, x0, r) ->
  (match i with
   | XI i0 ->
     (match gmap_dep_ne_partial_alter f i0 r with
      | GEmpty -> GNodes (GNode110 (l, x0))
      | GNodes r0 -> GNodes (GNode111 (l, x0, r0)))
   | XO i0 ->
     (match gmap_dep_ne_partial_alter f i0 l with
      | GEmpty -> GNodes (GNode011 (x0, r))
      | GNodes l0 -> GNodes (GNode111 (l0, x0, r)))
   | XH ->
     (match f (Some x0) with
      | Some a ->
        let p0 = (__, a) in let (_, x1) = p0 in GNodes (GNode111 (l, x1, r))
      | None -> GNodes (GNode101 (l, r))))

(** val gmap_dep_partial_alter :
    ('a1 option -> 'a1 option) -> positive -> 'a1 gmap_dep -> 'a1 gmap_dep **)

let gmap_dep_partial_alter f i x =
  gmap_partial_alter_aux (fun x0 _ -> gmap_dep_ne_partial_alter f x0) f i x

(** val gmap_partial_alter :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2, ('a1, 'a2) gmap)
    partialAlter **)

let gmap_partial_alter _ h f k pat =
  gmap_dep_partial_alter f (h.encode k) pat

(** val gmap_dep_ne_fmap :
    ('a1 -> 'a2) -> 'a1 gmap_dep_ne -> 'a2 gmap_dep_ne **)

let rec gmap_dep_ne_fmap f = function
| GNode001 r -> GNode001 (gmap_dep_ne_fmap f r)
| GNode010 x0 -> GNode010 (f x0)
| GNode011 (x0, r) -> GNode011 ((f x0), (gmap_dep_ne_fmap f r))
| GNode100 l -> GNode100 (gmap_dep_ne_fmap f l)
| GNode101 (l, r) -> GNode101 ((gmap_dep_ne_fmap f l), (gmap_dep_ne_fmap f r))
| GNode110 (l, x0) -> GNode110 ((gmap_dep_ne_fmap f l), (f x0))
| GNode111 (l, x0, r) ->
  GNode111 ((gmap_dep_ne_fmap f l), (f x0), (gmap_dep_ne_fmap f r))

(** val gmap_dep_fmap : ('a1 -> 'a2) -> 'a1 gmap_dep -> 'a2 gmap_dep **)

let gmap_dep_fmap f = function
| GEmpty -> GEmpty
| GNodes t -> GNodes (gmap_dep_ne_fmap f t)

(** val gmap_fmap :
    ('a1, 'a1) relDecision -> 'a1 countable -> (__ -> __) -> ('a1, __) gmap
    -> ('a1, __) gmap **)

let gmap_fmap _ _ =
  gmap_dep_fmap

(** val gmap_dep_omap_aux :
    ('a1 gmap_dep_ne -> 'a2 gmap_dep) -> 'a1 gmap_dep -> 'a2 gmap_dep **)

let gmap_dep_omap_aux go = function
| GEmpty -> GEmpty
| GNodes t' -> go t'

(** val gmap_dep_ne_omap :
    ('a1 -> 'a2 option) -> 'a1 gmap_dep_ne -> 'a2 gmap_dep **)

let rec gmap_dep_ne_omap f x =
  gmap_dep_ne_case x (fun ml mx mr ->
    gNode (gmap_dep_omap_aux (fun x0 -> gmap_dep_ne_omap f x0) ml)
      (mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
        let (_, x0) = pat in
        fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x1 -> (__, x1))
          (Obj.magic f x0))
        (Obj.magic mx))
      (gmap_dep_omap_aux (fun x0 -> gmap_dep_ne_omap f x0) mr))

(** val gmap_merge_aux :
    ('a1 gmap_dep_ne -> 'a2 gmap_dep_ne -> 'a3 gmap_dep) -> ('a1 option ->
    'a2 option -> 'a3 option) -> 'a1 gmap_dep -> 'a2 gmap_dep -> 'a3 gmap_dep **)

let gmap_merge_aux go f mt1 mt2 =
  match mt1 with
  | GEmpty ->
    (match mt2 with
     | GEmpty -> GEmpty
     | GNodes t2' -> gmap_dep_ne_omap (fun x -> f None (Some x)) t2')
  | GNodes t1' ->
    (match mt2 with
     | GEmpty -> gmap_dep_ne_omap (fun x -> f (Some x) None) t1'
     | GNodes t2' -> go t1' t2')

(** val diag_None' :
    ('a1 option -> 'a2 option -> 'a3 option) -> (__ * 'a1) option ->
    (__ * 'a2) option -> (__ * 'a3) option **)

let diag_None' f mx my =
  match mx with
  | Some p0 ->
    let (_, x) = p0 in
    (match my with
     | Some p1 ->
       let (_, y) = p1 in
       fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x0 -> (__, x0))
         (Obj.magic f (Some x) (Some y))
     | None ->
       fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x0 -> (__, x0))
         (Obj.magic f (Some x) None))
  | None ->
    (match my with
     | Some p0 ->
       let (_, y) = p0 in
       fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x -> (__, x))
         (Obj.magic f None (Some y))
     | None -> None)

(** val gmap_dep_ne_merge :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 gmap_dep_ne -> 'a2
    gmap_dep_ne -> 'a3 gmap_dep **)

let rec gmap_dep_ne_merge f x x0 =
  gmap_dep_ne_case x (fun ml1 mx1 mr1 ->
    gmap_dep_ne_case x0 (fun ml2 mx2 mr2 ->
      gNode
        (gmap_merge_aux (fun x1 x2 -> gmap_dep_ne_merge f x1 x2) f ml1 ml2)
        (diag_None' f mx1 mx2)
        (gmap_merge_aux (fun x1 x2 -> gmap_dep_ne_merge f x1 x2) f mr1 mr2)))

(** val gmap_dep_merge :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 gmap_dep -> 'a2 gmap_dep
    -> 'a3 gmap_dep **)

let gmap_dep_merge f =
  gmap_merge_aux (gmap_dep_ne_merge f) f

(** val gmap_merge :
    ('a1, 'a1) relDecision -> 'a1 countable -> (__ option -> __ option -> __
    option) -> ('a1, __) gmap -> ('a1, __) gmap -> ('a1, __) gmap **)

let gmap_merge _ _ =
  gmap_dep_merge

(** val gmap_fold_aux :
    (positive -> 'a2 -> 'a1 gmap_dep_ne -> 'a2) -> positive -> 'a2 -> 'a1
    gmap_dep -> 'a2 **)

let gmap_fold_aux go i y = function
| GEmpty -> y
| GNodes t -> go i y t

(** val gmap_dep_ne_fold :
    (positive -> 'a1 -> 'a2 -> 'a2) -> positive -> 'a2 -> 'a1 gmap_dep_ne ->
    'a2 **)

let rec gmap_dep_ne_fold f x x0 x1 =
  gmap_dep_ne_case x1 (fun ml mx mr ->
    gmap_fold_aux (fun x2 x3 x4 -> gmap_dep_ne_fold f x2 x3 x4) (XI x)
      (gmap_fold_aux (fun x2 x3 x4 -> gmap_dep_ne_fold f x2 x3 x4) (XO x)
        (match mx with
         | Some p0 -> let (_, x2) = p0 in f (Coq0_Pos.reverse x) x2 x0
         | None -> x0)
        ml)
      mr)

(** val gmap_dep_fold :
    (positive -> 'a1 -> 'a2 -> 'a2) -> positive -> 'a2 -> 'a1 gmap_dep -> 'a2 **)

let gmap_dep_fold f =
  gmap_fold_aux (gmap_dep_ne_fold f)

(** val gmap_fold :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1 -> 'a2 -> __ -> __) -> __
    -> ('a1, 'a2) gmap -> __ **)

let gmap_fold _ h f y pat =
  gmap_dep_fold (fun i x ->
    match h.decode i with
    | Some k -> f k x
    | None -> id) XH y pat

type 'k gset = ('k, unit) gmap mapset'

(** val gset_empty :
    ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 gset empty **)

let gset_empty eqDecision0 h =
  mapset_empty (fun _ -> gmap_empty eqDecision0 h)

(** val gset_singleton :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a1 gset) singleton **)

let gset_singleton eqDecision0 h =
  mapset_singleton (fun _ -> gmap_empty eqDecision0 h)
    (Obj.magic (fun _ -> gmap_partial_alter eqDecision0 h))

(** val gset_union :
    ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 gset union **)

let gset_union eqDecision0 h =
  mapset_union (Obj.magic (fun _ _ _ -> gmap_merge eqDecision0 h))

(** val gset_elements :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a1 gset) elements **)

let gset_elements eqDecision0 h =
  mapset_elements (Obj.magic (fun _ _ -> gmap_fold eqDecision0 h))

(** val gset_eq_dec :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1 gset, 'a1 gset)
    relDecision **)

let gset_eq_dec eqDecision0 h =
  mapset_eq_dec (gmap_eq_dec eqDecision0 h unit_eq_dec)

(** val gset_elem_of_dec :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a1 gset) relDecision **)

let gset_elem_of_dec eqDecision0 h =
  mapset_elem_of_dec (Obj.magic (fun _ -> gmap_lookup eqDecision0 h))

(** val fresh_string_go :
    char list -> (char list, 'a1) gmap -> n -> (n -> __ -> char list) ->
    char list **)

let fresh_string_go s m n0 go =
  let s' = append s (pretty0 pretty_N n0) in
  (match some_dec (lookup0 (gmap_lookup String.eq_dec String.countable) s' m) with
   | Some _ -> go (N.add (Npos XH) n0) __
   | None -> s')

(** val fresh_string : char list -> (char list, 'a1) gmap -> char list **)

let fresh_string s m =
  match lookup0 (gmap_lookup String.eq_dec String.countable) s m with
  | Some _ ->
    let rec fix_F x =
      fresh_string_go s m x (fun y _ -> fix_F y)
    in fix_F N0
  | None -> s

(** val fresh_string_of_set : char list -> char list gset -> char list **)

let fresh_string_of_set =
  fresh_string

(** val fresh_strings_of_set :
    char list -> int -> char list gset -> char list list **)

let rec fresh_strings_of_set s n0 x =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun n1 ->
    let x0 = fresh_string_of_set s x in
    x0 :: (fresh_strings_of_set s n1
            (union0 (gset_union String.eq_dec String.countable)
              (singleton0 (gset_singleton String.eq_dec String.countable) x0)
              x)))
    n0

(** val map_snd : ('a2 -> 'a3) -> ('a1 * 'a2) list -> ('a1 * 'a3) list **)

let map_snd f =
  map (prod_map (Obj.magic id) f)

(** val gset_to_gmap_with :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> 'a2
    countable -> ('a1 -> 'a2) -> ('a1 -> 'a3) -> 'a1 gset -> ('a2, 'a3) gmap **)

let gset_to_gmap_with eqDecision0 h eqDecision1 h0 f_key f_val x =
  set_to_map (gset_elements eqDecision0 h)
    (map_insert (gmap_partial_alter eqDecision1 h0))
    (gmap_empty eqDecision1 h0) (fun x0 -> ((f_key x0), (f_val x0))) x

type preval =
| PVNull
| PVGillianNone
| PVGillianEmpty
| PVGillianLoc of int
| PVBool of bool
| PVNat of int
| PVRat of qp
| PVString of char list
| PVADT of char list * char list * preval list
| PVList of preval list

type type0 =
| TVal
| TNull
| TGillianNone
| TGillianEmpty
| TGillianLoc
| TBool
| TNat
| TRat
| TString
| TADT of char list
| TList of type0

(** val adts : char list gset **)

let adts =
  map_img (Obj.magic (fun _ -> gmap_fold String.eq_dec String.countable))
    (gset_singleton String.eq_dec String.countable)
    (gset_empty String.eq_dec String.countable)
    (gset_union String.eq_dec String.countable)
    (fmap (Obj.magic (fun _ _ -> gmap_fmap String.eq_dec String.countable))
      snd (gmap_empty String.eq_dec String.countable))

(** val eval_type_wf : type0 -> bool **)

let rec eval_type_wf = function
| TADT adt ->
  bool_decide
    (decide_rel (gset_elem_of_dec String.eq_dec String.countable) adt adts)
| TList ty' -> eval_type_wf ty'
| _ -> true

type val0 = preval

type op1 =
| Op1Not
| Op1Length
| Op1IsInt
| Op1AsInt
| Op1AsNum

type op2 =
| Op2Eq
| Op2And
| Op2Add
| Op2Sub
| Op2Mul
| Op2Div
| Op2Mod
| Op2Lt
| Op2Cons
| Op2In
| Op2Cat
| Op2Nth
| Op2RAdd
| Op2RSub
| Op2RMul
| Op2RDiv
| Op2RLt
| Op2RLe

type 'a lV =
  'a -> char list gset
  (* singleton inductive, whose constructor was Build_LV *)

type pattern =
| PatVar
| PatCtor of char list

type lexp =
| LEVal of val0
| LEPVar of char list
| LEFLVar of char list
| LEBLVar of int * int
| LEADT of char list * char list * lexp list
| LEList of lexp list
| LEApp of char list * lexp list
| LEOp1 of op1 * lexp
| LEOp2 of lexp * op2 * lexp
| LEIn of lexp * type0
| LEIsCtor of lexp * char list
| LEMatch of char list * lexp * (pattern * lexp) list

(** val lv_lexp : lexp -> char list gset **)

let rec lv_lexp = function
| LEFLVar var ->
  singleton0 (gset_singleton String.eq_dec String.countable) var
| LEADT (_, _, es) ->
  union_list (gset_empty String.eq_dec String.countable)
    (gset_union String.eq_dec String.countable) (map lv_lexp es)
| LEList es ->
  union_list (gset_empty String.eq_dec String.countable)
    (gset_union String.eq_dec String.countable) (map lv_lexp es)
| LEApp (_, es) ->
  union_list (gset_empty String.eq_dec String.countable)
    (gset_union String.eq_dec String.countable) (map lv_lexp es)
| LEOp1 (_, e0) -> lv_lexp e0
| LEOp2 (e1, _, e2) ->
  union0 (gset_union String.eq_dec String.countable) (lv_lexp e1) (lv_lexp e2)
| LEIn (e0, _) -> lv_lexp e0
| LEIsCtor (e0, _) -> lv_lexp e0
| LEMatch (_, e0, cases) ->
  union0 (gset_union String.eq_dec String.countable) (lv_lexp e0)
    (union_list (gset_empty String.eq_dec String.countable)
      (gset_union String.eq_dec String.countable)
      (map (compose lv_lexp snd) cases))
| _ -> empty0 (gset_empty String.eq_dec String.countable)

(** val lV_lexp : lexp lV **)

let lV_lexp =
  lv_lexp

type lfunc = { lfunc_params : char list list; lfunc_params_ty : type0 list;
               lfunc_body : lexp; lfunc_ret_ty : type0; lfunc_pre : lexp }



type sexp =
| SEVal of val0
| SEFLVar of char list
| SEBLVar of int * int
| SEADT of char list * char list * sexp list
| SEList of sexp list
| SEApp of char list * sexp list
| SEOp1 of op1 * sexp
| SEOp2 of sexp * op2 * sexp
| SEIn of sexp * type0
| SEIsCtor of sexp * char list
| SEMatch of char list * sexp * (pattern * sexp) list

(** val sexp_to_lexp : sexp -> lexp **)

let rec sexp_to_lexp = function
| SEVal v -> LEVal v
| SEFLVar var -> LEFLVar var
| SEBLVar (i, j) -> LEBLVar (i, j)
| SEADT (adt, ctor, es) -> LEADT (adt, ctor, (map sexp_to_lexp es))
| SEList es -> LEList (map sexp_to_lexp es)
| SEApp (f, es) -> LEApp (f, (map sexp_to_lexp es))
| SEOp1 (op, e0) -> LEOp1 (op, (sexp_to_lexp e0))
| SEOp2 (e1, op, e2) -> LEOp2 ((sexp_to_lexp e1), op, (sexp_to_lexp e2))
| SEIn (e0, t) -> LEIn ((sexp_to_lexp e0), t)
| SEIsCtor (e0, ctor) -> LEIsCtor ((sexp_to_lexp e0), ctor)
| SEMatch (adt_t, e0, cases) ->
  LEMatch (adt_t, (sexp_to_lexp e0), (map_snd sexp_to_lexp cases))

(** val lV_sexp : sexp lV **)

let lV_sexp =
  compose lV_lexp sexp_to_lexp

(** val sexp_open : int -> sexp list -> sexp -> sexp **)

let rec sexp_open k us e = match e with
| SEBLVar (i, j) ->
  if decide (decide_rel Coq_Nat.eq_dec i k) then nth j us e else e
| SEADT (adt, ctor, es) -> SEADT (adt, ctor, (map (sexp_open k us) es))
| SEList es -> SEList (map (sexp_open k us) es)
| SEApp (f, es) -> SEApp (f, (map (sexp_open k us) es))
| SEOp1 (op, e0) -> SEOp1 (op, (sexp_open k us e0))
| SEOp2 (e1, op, e2) -> SEOp2 ((sexp_open k us e1), op, (sexp_open k us e2))
| SEIn (e0, topt) -> SEIn ((sexp_open k us e0), topt)
| SEIsCtor (e0, ctor) -> SEIsCtor ((sexp_open k us e0), ctor)
| SEMatch (t, e0, cases) ->
  let e' = sexp_open k us e0 in
  let cases' =
    map (fun pat ->
      let (p, e1) = pat in (p, (sexp_open (Stdlib.Int.succ k) us e1))) cases
  in
  SEMatch (t, e', cases')
| x -> x

(** val sexp_match_cases_has_patvar : (pattern * sexp) list -> bool **)

let rec sexp_match_cases_has_patvar = function
| [] -> false
| p :: pes0 ->
  let (p0, _) = p in
  (match p0 with
   | PatVar -> true
   | PatCtor _ -> sexp_match_cases_has_patvar pes0)

(** val list_eq_dec_elem_of :
    'a1 list -> 'a1 list -> ('a1 -> 'a1 -> __ -> __ -> bool) -> bool **)

let rec list_eq_dec_elem_of l ys helem =
  match l with
  | [] -> (match ys with
           | [] -> true
           | _ :: _ -> false)
  | y :: l0 ->
    (match ys with
     | [] -> false
     | a :: l1 ->
       let iH = list_eq_dec_elem_of l0 l1 in
       let htail = iH (fun x y0 _ _ -> helem x y0 __ __) in
       let helem0 = helem y a in
       let hhead = helem0 __ __ in if hhead then htail else false)

type index =
| IdxNum of int
| IdxSym of char list

(** val index_eq_decision : (index, index) relDecision **)

let index_eq_decision x y =
  match x with
  | IdxNum n0 ->
    (match y with
     | IdxNum n1 -> decide_rel Coq_Nat.eq_dec n0 n1
     | IdxSym _ -> false)
  | IdxSym s ->
    (match y with
     | IdxNum _ -> false
     | IdxSym s0 -> decide_rel String.eq_dec s s0)

(** val index_countable : index countable **)

let index_countable =
  inj_countable (sum_eq_dec Coq_Nat.eq_dec String.eq_dec)
    (sum_countable Coq_Nat.eq_dec nat_countable String.eq_dec
      String.countable)
    index_eq_decision (fun i ->
    match i with
    | IdxNum n0 -> Inl n0
    | IdxSym s -> Inr s) (fun code ->
    match code with
    | Inl n0 -> Some (IdxNum n0)
    | Inr s -> Some (IdxSym s))

type identifier =
| IdSimple of char list
| IdIndexed of char list * index list

(** val identifier_eq_decision : (identifier, identifier) relDecision **)

let identifier_eq_decision x y =
  match x with
  | IdSimple s ->
    (match y with
     | IdSimple s0 -> decide_rel String.eq_dec s s0
     | IdIndexed (_, _) -> false)
  | IdIndexed (s, idx) ->
    (match y with
     | IdSimple _ -> false
     | IdIndexed (s0, idx0) ->
       if decide_rel String.eq_dec s s0
       then decide_rel (Coq_list.list_eq_dec index_eq_decision) idx idx0
       else false)

(** val identifier_countable : identifier countable **)

let identifier_countable =
  inj_countable
    (sum_eq_dec String.eq_dec
      (prod_eq_dec String.eq_dec (Coq_list.list_eq_dec index_eq_decision)))
    (sum_countable String.eq_dec String.countable
      (prod_eq_dec String.eq_dec (Coq_list.list_eq_dec index_eq_decision))
      (prod_countable String.eq_dec String.countable
        (Coq_list.list_eq_dec index_eq_decision)
        (list_countable index_eq_decision index_countable)))
    identifier_eq_decision (fun i ->
    match i with
    | IdSimple s -> Inl s
    | IdIndexed (s, idx) -> Inr (s, idx)) (fun code ->
    match code with
    | Inl s -> Some (IdSimple s)
    | Inr p -> let (s, idx) = p in Some (IdIndexed (s, idx)))

(** val identifier_add_prefix : char list -> identifier -> identifier **)

let identifier_add_prefix prefix = function
| IdSimple s -> IdSimple (append prefix s)
| IdIndexed (s, idx) -> IdIndexed ((append prefix s), idx)

type sort =
| SParam of identifier
| SApp of identifier * sort list

(** val sort_eq_decision : (sort, sort) relDecision **)

let rec sort_eq_decision x x0 =
  match x with
  | SParam u ->
    (match x0 with
     | SParam u0 -> decide_rel identifier_eq_decision u u0
     | SApp (_, _) -> false)
  | SApp (s, _UU03c4_s) ->
    (match x0 with
     | SParam _ -> false
     | SApp (s0, _UU03c4_s0) ->
       if decide_rel identifier_eq_decision s s0
       then list_eq_dec_elem_of _UU03c4_s _UU03c4_s0 (fun x1 y0 _ _ ->
              sort_eq_decision x1 y0)
       else false)

(** val sort_to_gen_tree : sort -> identifier gen_tree **)

let rec sort_to_gen_tree = function
| SParam u -> GenLeaf u
| SApp (sym, _UU03c4_s) ->
  GenNode (0, ((GenLeaf sym) :: (map sort_to_gen_tree _UU03c4_s)))

(** val gen_tree_to_sort : identifier gen_tree -> sort option **)

let rec gen_tree_to_sort = function
| GenLeaf u -> Some (SParam u)
| GenNode (n0, l) ->
  ((fun fO fS n -> if n=0 then fO () else fS (n-1))
     (fun _ ->
     match l with
     | [] -> None
     | g :: ts ->
       (match g with
        | GenLeaf sym ->
          mbind (Obj.magic (fun _ _ -> option_bind)) (fun _UU03c4_s -> Some
            (SApp (sym, _UU03c4_s)))
            (Coq0_list.mapM (Obj.magic (fun _ _ -> option_bind))
              (Obj.magic (fun _ -> option_ret)) gen_tree_to_sort ts)
        | GenNode (_, _) -> None))
     (fun _ -> None)
     n0)

(** val sort_countable : sort countable **)

let sort_countable =
  inj_countable (gen_tree_dec identifier_eq_decision)
    (gen_tree_countable identifier_eq_decision identifier_countable)
    sort_eq_decision sort_to_gen_tree gen_tree_to_sort

(** val s_bool : identifier **)

let s_bool =
  IdSimple ('B'::('o'::('o'::('l'::[]))))

(** val _UU03c3__bool : sort **)

let _UU03c3__bool =
  SApp (s_bool, [])

type pattern0 =
| PVar
| PApp of identifier * int

(** val pattern_eq_dec : (pattern0, pattern0) relDecision **)

let pattern_eq_dec x y =
  match x with
  | PVar -> (match y with
             | PVar -> true
             | PApp (_, _) -> false)
  | PApp (c, arity) ->
    (match y with
     | PVar -> false
     | PApp (c0, arity0) ->
       if decide_rel identifier_eq_decision c c0
       then decide_rel Coq_Nat.eq_dec arity arity0
       else false)

(** val pattern_countable : pattern0 countable **)

let pattern_countable =
  inj_countable
    (sum_eq_dec unit_eq_dec
      (prod_eq_dec identifier_eq_decision Coq_Nat.eq_dec))
    (sum_countable unit_eq_dec unit_countable
      (prod_eq_dec identifier_eq_decision Coq_Nat.eq_dec)
      (prod_countable identifier_eq_decision identifier_countable
        Coq_Nat.eq_dec nat_countable))
    pattern_eq_dec (fun p ->
    match p with
    | PVar -> Inl ()
    | PApp (c, a) -> Inr (c, a)) (fun code ->
    match code with
    | Inl _ -> Some PVar
    | Inr p -> let (c, a) = p in Some (PApp (c, a)))

(** val pattern_constructor : pattern0 -> identifier option **)

let pattern_constructor = function
| PVar -> None
| PApp (c, _) -> Some c

type term =
| TFVar of char list
| TBVar of int * int
| TApp of identifier * sort option * term list
| TLambda of sort * term
| TExists of sort * term
| TForall of sort * term
| TLet of term list * term
| TMatch of term * (pattern0 * term) list

(** val term_eq_decision : (term, term) relDecision **)

let rec term_eq_decision = function
| TFVar x0 ->
  (fun x1 ->
    match x1 with
    | TFVar x2 -> decide_rel String.eq_dec x0 x2
    | _ -> false)
| TBVar (i, j) ->
  (fun x0 ->
    match x0 with
    | TBVar (i0, j0) ->
      if decide_rel Coq_Nat.eq_dec i i0
      then decide_rel Coq_Nat.eq_dec j j0
      else false
    | _ -> false)
| TApp (f, _UU03c3_, ts) ->
  (fun x0 ->
    match x0 with
    | TApp (f0, _UU03c3_0, ts0) ->
      if decide_rel identifier_eq_decision f f0
      then if decide_rel (option_eq_dec sort_eq_decision) _UU03c3_ _UU03c3_0
           then list_eq_dec_elem_of ts ts0 (fun x1 y0 _ _ ->
                  term_eq_decision x1 y0)
           else false
      else false
    | _ -> false)
| TLambda (_UU03c3_, t) ->
  let h = term_eq_decision t in
  (fun x0 ->
  match x0 with
  | TLambda (_UU03c3_0, t0) ->
    if decide_rel sort_eq_decision _UU03c3_ _UU03c3_0 then h t0 else false
  | _ -> false)
| TExists (_UU03c3_, t) ->
  let h = term_eq_decision t in
  (fun x0 ->
  match x0 with
  | TExists (_UU03c3_0, t0) ->
    if decide_rel sort_eq_decision _UU03c3_ _UU03c3_0 then h t0 else false
  | _ -> false)
| TForall (_UU03c3_, t) ->
  let h = term_eq_decision t in
  (fun x0 ->
  match x0 with
  | TForall (_UU03c3_0, t0) ->
    if decide_rel sort_eq_decision _UU03c3_ _UU03c3_0 then h t0 else false
  | _ -> false)
| TLet (binds, t) ->
  (fun x0 ->
    match x0 with
    | TLet (binds0, t0) ->
      if list_eq_dec_elem_of binds binds0 (fun x1 y0 _ _ ->
           term_eq_decision x1 y0)
      then term_eq_decision t t0
      else false
    | _ -> false)
| TMatch (t, cases) ->
  (fun x0 ->
    match x0 with
    | TMatch (t0, cases0) ->
      if term_eq_decision t t0
      then list_eq_dec_elem_of cases cases0 (fun pt1 pt2 _ _ ->
             let (p, t1) = pt1 in
             let (p0, t2) = pt2 in
             let s = decide (decide_rel pattern_eq_dec p p0) in
             if s then term_eq_decision t1 t2 else false)
      else false
    | _ -> false)

(** val term_encode :
    term -> (((char list, int * int) sum, ((int * identifier) * sort option,
    sort) sum) sum, ((sort, sort) sum, (int, pattern0 list) sum) sum) sum list **)

let rec term_encode = function
| TFVar x -> (Inl (Inl (Inl x))) :: []
| TBVar (i, j) -> (Inl (Inl (Inr (i, j)))) :: []
| TApp (f, _UU03c3_, ts) ->
  app
    (mbind (Obj.magic (fun _ _ -> Coq0_list.list_bind)) term_encode
      (Obj.magic ts))
    ((Inl (Inr (Inl (((length ts), f), _UU03c3_)))) :: [])
| TLambda (_UU03c3_, t0) ->
  app (term_encode t0) ((Inl (Inr (Inr _UU03c3_))) :: [])
| TExists (_UU03c3_, t0) ->
  app (term_encode t0) ((Inr (Inl (Inl _UU03c3_))) :: [])
| TForall (_UU03c3_, t0) ->
  app (term_encode t0) ((Inr (Inl (Inr _UU03c3_))) :: [])
| TLet (ts, t0) ->
  app
    (mbind (Obj.magic (fun _ _ -> Coq0_list.list_bind)) term_encode
      (Obj.magic ts))
    (app (term_encode t0) ((Inr (Inr (Inl (length ts)))) :: []))
| TMatch (t0, pts) ->
  let ps = map fst pts in
  app (term_encode t0)
    (app
      (mbind (Obj.magic (fun _ _ -> Coq0_list.list_bind))
        (compose term_encode snd) (Obj.magic pts))
      ((Inr (Inr (Inr ps))) :: []))

(** val term_decode :
    term list -> (((char list, int * int) sum, ((int * identifier) * sort
    option, sort) sum) sum, ((sort, sort) sum, (int, pattern0 list) sum) sum)
    sum list -> term option **)

let rec term_decode stack = function
| [] -> hd_error stack
| s :: code' ->
  (match s with
   | Inl s0 ->
     (match s0 with
      | Inl s1 ->
        (match s1 with
         | Inl x -> term_decode ((TFVar x) :: stack) code'
         | Inr p ->
           let (i, j) = p in term_decode ((TBVar (i, j)) :: stack) code')
      | Inr s1 ->
        (match s1 with
         | Inl p ->
           let (p0, _UU03c3_) = p in
           let (n0, f) = p0 in
           let ts = Coq_list.reverse (firstn n0 stack) in
           let stack' = skipn n0 stack in
           term_decode ((TApp (f, _UU03c3_, ts)) :: stack') code'
         | Inr _UU03c3_ ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun t ->
             let stack' = tl stack in
             term_decode ((TLambda (_UU03c3_, t)) :: stack') code')
             (hd_error stack)))
   | Inr s0 ->
     (match s0 with
      | Inl s1 ->
        (match s1 with
         | Inl _UU03c3_ ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun t ->
             let stack' = tl stack in
             term_decode ((TExists (_UU03c3_, t)) :: stack') code')
             (hd_error stack)
         | Inr _UU03c3_ ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun t ->
             let stack' = tl stack in
             term_decode ((TForall (_UU03c3_, t)) :: stack') code')
             (hd_error stack))
      | Inr s1 ->
        (match s1 with
         | Inl n0 ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun t ->
             let stack' = tl stack in
             let ts = Coq_list.reverse (firstn n0 stack') in
             let stack'' = skipn n0 stack' in
             term_decode ((TLet (ts, t)) :: stack'') code') (hd_error stack)
         | Inr ps ->
           let n0 = length ps in
           let ts = Coq_list.reverse (firstn n0 stack) in
           let stack' = skipn n0 stack in
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun t ->
             let stack'' = tl stack' in
             term_decode ((TMatch (t,
               (zip_with (fun x x0 -> (x, x0)) ps ts))) :: stack'') code')
             (hd_error stack'))))

(** val term_countable : term countable **)

let term_countable =
  inj_countable
    (Coq_list.list_eq_dec
      (sum_eq_dec
        (sum_eq_dec
          (sum_eq_dec String.eq_dec
            (prod_eq_dec Coq_Nat.eq_dec Coq_Nat.eq_dec))
          (sum_eq_dec
            (prod_eq_dec (prod_eq_dec Coq_Nat.eq_dec identifier_eq_decision)
              (option_eq_dec sort_eq_decision))
            sort_eq_decision))
        (sum_eq_dec (sum_eq_dec sort_eq_decision sort_eq_decision)
          (sum_eq_dec Coq_Nat.eq_dec (Coq_list.list_eq_dec pattern_eq_dec)))))
    (list_countable
      (sum_eq_dec
        (sum_eq_dec
          (sum_eq_dec String.eq_dec
            (prod_eq_dec Coq_Nat.eq_dec Coq_Nat.eq_dec))
          (sum_eq_dec
            (prod_eq_dec (prod_eq_dec Coq_Nat.eq_dec identifier_eq_decision)
              (option_eq_dec sort_eq_decision))
            sort_eq_decision))
        (sum_eq_dec (sum_eq_dec sort_eq_decision sort_eq_decision)
          (sum_eq_dec Coq_Nat.eq_dec (Coq_list.list_eq_dec pattern_eq_dec))))
      (sum_countable
        (sum_eq_dec
          (sum_eq_dec String.eq_dec
            (prod_eq_dec Coq_Nat.eq_dec Coq_Nat.eq_dec))
          (sum_eq_dec
            (prod_eq_dec (prod_eq_dec Coq_Nat.eq_dec identifier_eq_decision)
              (option_eq_dec sort_eq_decision))
            sort_eq_decision))
        (sum_countable
          (sum_eq_dec String.eq_dec
            (prod_eq_dec Coq_Nat.eq_dec Coq_Nat.eq_dec))
          (sum_countable String.eq_dec String.countable
            (prod_eq_dec Coq_Nat.eq_dec Coq_Nat.eq_dec)
            (prod_countable Coq_Nat.eq_dec nat_countable Coq_Nat.eq_dec
              nat_countable))
          (sum_eq_dec
            (prod_eq_dec (prod_eq_dec Coq_Nat.eq_dec identifier_eq_decision)
              (option_eq_dec sort_eq_decision))
            sort_eq_decision)
          (sum_countable
            (prod_eq_dec (prod_eq_dec Coq_Nat.eq_dec identifier_eq_decision)
              (option_eq_dec sort_eq_decision))
            (prod_countable
              (prod_eq_dec Coq_Nat.eq_dec identifier_eq_decision)
              (prod_countable Coq_Nat.eq_dec nat_countable
                identifier_eq_decision identifier_countable)
              (option_eq_dec sort_eq_decision)
              (option_countable sort_eq_decision sort_countable))
            sort_eq_decision sort_countable))
        (sum_eq_dec (sum_eq_dec sort_eq_decision sort_eq_decision)
          (sum_eq_dec Coq_Nat.eq_dec (Coq_list.list_eq_dec pattern_eq_dec)))
        (sum_countable (sum_eq_dec sort_eq_decision sort_eq_decision)
          (sum_countable sort_eq_decision sort_countable sort_eq_decision
            sort_countable)
          (sum_eq_dec Coq_Nat.eq_dec (Coq_list.list_eq_dec pattern_eq_dec))
          (sum_countable Coq_Nat.eq_dec nat_countable
            (Coq_list.list_eq_dec pattern_eq_dec)
            (list_countable pattern_eq_dec pattern_countable)))))
    term_eq_decision term_encode (term_decode [])

(** val fv : term -> char list gset **)

let rec fv = function
| TFVar x -> singleton0 (gset_singleton String.eq_dec String.countable) x
| TBVar (_, _) -> empty0 (gset_empty String.eq_dec String.countable)
| TApp (_, _, ts) ->
  union_list (gset_empty String.eq_dec String.countable)
    (gset_union String.eq_dec String.countable) (map fv ts)
| TLambda (_, t0) -> fv t0
| TExists (_, t0) -> fv t0
| TForall (_, t0) -> fv t0
| TLet (ts, t0) ->
  union0 (gset_union String.eq_dec String.countable) (fv t0)
    (union_list (gset_empty String.eq_dec String.countable)
      (gset_union String.eq_dec String.countable) (map fv ts))
| TMatch (t0, pts) ->
  union0 (gset_union String.eq_dec String.countable) (fv t0)
    (union_list (gset_empty String.eq_dec String.countable)
      (gset_union String.eq_dec String.countable) (map (compose fv snd) pts))

(** val term_open : int -> term list -> term -> term **)

let rec term_open k us t = match t with
| TFVar x -> TFVar x
| TBVar (i, j) ->
  if decide (decide_rel Coq_Nat.eq_dec i k) then nth j us t else t
| TApp (f, _UU03c3_, ts) -> TApp (f, _UU03c3_, (map (term_open k us) ts))
| TLambda (_UU03c4_, t0) ->
  TLambda (_UU03c4_, (term_open (Stdlib.Int.succ k) us t0))
| TExists (_UU03c4_, t0) ->
  TExists (_UU03c4_, (term_open (Stdlib.Int.succ k) us t0))
| TForall (_UU03c4_, t0) ->
  TForall (_UU03c4_, (term_open (Stdlib.Int.succ k) us t0))
| TLet (ts, t0) ->
  let ts' = map (term_open k us) ts in
  TLet (ts', (term_open (Stdlib.Int.succ k) us t0))
| TMatch (t0, pts) ->
  let t' = term_open k us t0 in
  let pts' =
    map (fun pat ->
      let (p, t1) = pat in (p, (term_open (Stdlib.Int.succ k) us t1))) pts
  in
  TMatch (t', pts')

(** val term_close : char list list -> int -> term -> term **)

let rec term_close ys k = function
| TFVar x ->
  (match Coq1_list.list_find (decide_rel String.eq_dec x) ys with
   | Some p -> let (j, _) = p in TBVar (k, j)
   | None -> TFVar x)
| TBVar (i, j) -> TBVar (i, j)
| TApp (f, _UU03c3_, ts) -> TApp (f, _UU03c3_, (map (term_close ys k) ts))
| TLambda (_UU03c4_, t0) ->
  TLambda (_UU03c4_, (term_close ys (Stdlib.Int.succ k) t0))
| TExists (_UU03c4_, t0) ->
  TExists (_UU03c4_, (term_close ys (Stdlib.Int.succ k) t0))
| TForall (_UU03c4_, t0) ->
  TForall (_UU03c4_, (term_close ys (Stdlib.Int.succ k) t0))
| TLet (ts, t0) ->
  let ts' = map (term_close ys k) ts in
  TLet (ts', (term_close ys (Stdlib.Int.succ k) t0))
| TMatch (t0, pts) ->
  let t' = term_close ys k t0 in
  let match_case_open = fun pat ->
    let (p, t1) = pat in (p, (term_close ys (Stdlib.Int.succ k) t1))
  in
  TMatch (t', (map match_case_open pts))

(** val term_subst : (char list, term) gmap -> term -> term **)

let rec term_subst subst = function
| TFVar x ->
  (match lookup0 (gmap_lookup String.eq_dec String.countable) x subst with
   | Some t' -> t'
   | None -> TFVar x)
| TBVar (i, j) -> TBVar (i, j)
| TApp (f, _UU03c3_, ts) -> TApp (f, _UU03c3_, (map (term_subst subst) ts))
| TLambda (_UU03c4_, t0) -> TLambda (_UU03c4_, (term_subst subst t0))
| TExists (_UU03c4_, t0) -> TExists (_UU03c4_, (term_subst subst t0))
| TForall (_UU03c4_, t0) -> TForall (_UU03c4_, (term_subst subst t0))
| TLet (ts, t0) ->
  let ts' = map (term_subst subst) ts in TLet (ts', (term_subst subst t0))
| TMatch (t0, pts) ->
  let t' = term_subst subst t0 in
  let match_case_subst = fun pat ->
    let (p, t1) = pat in (p, (term_subst subst t1))
  in
  TMatch (t', (map match_case_subst pts))

(** val f_true : identifier **)

let f_true =
  IdSimple ('t'::('r'::('u'::('e'::[]))))

(** val f_false : identifier **)

let f_false =
  IdSimple ('f'::('a'::('l'::('s'::('e'::[])))))

(** val f_not : identifier **)

let f_not =
  IdSimple ('n'::('o'::('t'::[])))

(** val f_impl : identifier **)

let f_impl =
  IdSimple ('='::('>'::[]))

(** val f_and : identifier **)

let f_and =
  IdSimple ('a'::('n'::('d'::[])))

(** val f_eq : identifier **)

let f_eq =
  IdSimple ('='::[])

(** val f_ite : identifier **)

let f_ite =
  IdSimple ('i'::('t'::('e'::[])))

(** val true_ : term **)

let true_ =
  TApp (f_true, None, [])

(** val false_ : term **)

let false_ =
  TApp (f_false, None, [])

(** val bool_ : bool -> term **)

let bool_ = function
| true -> true_
| false -> false_

(** val not_ : term -> term **)

let not_ t =
  TApp (f_not, None, (t :: []))

(** val impl_ : term -> term -> term **)

let impl_ t1 t2 =
  TApp (f_impl, None, (t1 :: (t2 :: [])))

(** val and_ : term -> term -> term **)

let and_ t1 t2 =
  TApp (f_and, None, (t1 :: (t2 :: [])))

(** val ands_ : term list -> term -> term **)

let ands_ ts t =
  fold_right and_ t ts

(** val eq_ : term -> term -> term **)

let eq_ t1 t2 =
  TApp (f_eq, None, (t1 :: (t2 :: [])))

(** val ite_ : term -> term -> term -> term **)

let ite_ t1 t2 t3 =
  TApp (f_ite, None, (t1 :: (t2 :: (t3 :: []))))

(** val s_int : identifier **)

let s_int =
  IdSimple ('I'::('n'::('t'::[])))

(** val _UU03c3__int : sort **)

let _UU03c3__int =
  SApp (s_int, [])

(** val s_real : identifier **)

let s_real =
  IdSimple ('R'::('e'::('a'::('l'::[]))))

(** val _UU03c3__real : sort **)

let _UU03c3__real =
  SApp (s_real, [])

(** val f_minus : identifier **)

let f_minus =
  IdSimple ('-'::[])

(** val f_plus : identifier **)

let f_plus =
  IdSimple ('+'::[])

(** val f_times : identifier **)

let f_times =
  IdSimple ('*'::[])

(** val f_idiv : identifier **)

let f_idiv =
  IdSimple ('d'::('i'::('v'::[])))

(** val f_div : identifier **)

let f_div =
  IdSimple ('/'::[])

(** val f_mod : identifier **)

let f_mod =
  IdSimple ('m'::('o'::('d'::[])))

(** val f_leq : identifier **)

let f_leq =
  IdSimple ('<'::('='::[]))

(** val f_lt : identifier **)

let f_lt =
  IdSimple ('<'::[])

(** val f_geq : identifier **)

let f_geq =
  IdSimple ('>'::('='::[]))

(** val f_to_real : identifier **)

let f_to_real =
  IdSimple ('t'::('o'::('_'::('r'::('e'::('a'::('l'::[])))))))

(** val f_to_int : identifier **)

let f_to_int =
  IdSimple ('t'::('o'::('_'::('i'::('n'::('t'::[]))))))

(** val f_is_int : identifier **)

let f_is_int =
  IdSimple ('i'::('s'::('_'::('i'::('n'::('t'::[]))))))

(** val f_int_literal : z -> identifier **)

let f_int_literal i =
  IdSimple
    (append
      ('i'::('n'::('t'::('_'::('l'::('i'::('t'::('e'::('r'::('a'::('l'::('_'::[]))))))))))))
      (pretty0 pretty_Z i))

(** val f_decimal_literal : qc -> identifier **)

let f_decimal_literal q0 =
  IdSimple
    (append
      ('d'::('e'::('c'::('i'::('m'::('a'::('l'::('_'::('l'::('i'::('t'::('e'::('r'::('a'::('l'::('_'::[]))))))))))))))))
      (append (pretty0 pretty_Z q0.qnum)
        (append ('/'::[]) (pretty0 pretty_positive q0.qden))))

(** val minus_ : term -> term -> term **)

let minus_ t1 t2 =
  TApp (f_minus, None, (t1 :: (t2 :: [])))

(** val plus_ : term -> term -> term **)

let plus_ t1 t2 =
  TApp (f_plus, None, (t1 :: (t2 :: [])))

(** val times_ : term -> term -> term **)

let times_ t1 t2 =
  TApp (f_times, None, (t1 :: (t2 :: [])))

(** val idiv_ : term -> term -> term **)

let idiv_ t1 t2 =
  TApp (f_idiv, None, (t1 :: (t2 :: [])))

(** val div_ : term -> term -> term **)

let div_ t1 t2 =
  TApp (f_div, None, (t1 :: (t2 :: [])))

(** val mod_ : term -> term -> term **)

let mod_ t1 t2 =
  TApp (f_mod, None, (t1 :: (t2 :: [])))

(** val leq_ : term -> term -> term **)

let leq_ t1 t2 =
  TApp (f_leq, None, (t1 :: (t2 :: [])))

(** val lt_ : term -> term -> term **)

let lt_ t1 t2 =
  TApp (f_lt, None, (t1 :: (t2 :: [])))

(** val geq_ : term -> term -> term **)

let geq_ t1 t2 =
  TApp (f_geq, None, (t1 :: (t2 :: [])))

(** val to_real : term -> term **)

let to_real t =
  TApp (f_to_real, None, (t :: []))

(** val to_int : term -> term **)

let to_int t =
  TApp (f_to_int, None, (t :: []))

(** val is_int : term -> term **)

let is_int t =
  TApp (f_is_int, None, (t :: []))

(** val int_literal : z -> term **)

let int_literal i =
  TApp ((f_int_literal i), None, [])

(** val decimal_literal : qc -> term **)

let decimal_literal q0 =
  TApp ((f_decimal_literal q0), None, [])

(** val zero_int : term **)

let zero_int =
  int_literal Z0

(** val zero_real : term **)

let zero_real =
  decimal_literal (q2Qc { qnum = Z0; qden = XH })

(** val s_string : identifier **)

let s_string =
  IdSimple ('S'::('t'::('r'::('i'::('n'::('g'::[]))))))

(** val _UU03c3__string : sort **)

let _UU03c3__string =
  SApp (s_string, [])

(** val quote_char : char **)

let quote_char =
  ascii_of_nat (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))))))))

(** val quote_string : char list **)

let quote_string =
  quote_char::[]

(** val hex_digit : int -> char **)

let hex_digit n0 =
  ascii_of_nat
    (if Nat.ltb n0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))
     then add (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            0)))))))))))))))))))))))))))))))))))))))))))))))) n0
     else add (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
            0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
            n0)

(** val plain_char : int -> bool **)

let plain_char n0 =
  (&&)
    ((&&)
      ((&&)
        ((<=) (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ 0)))))))))))))))))))))))))))))))) n0)
        ((<=) n0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      (negb
        ((=) n0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          0)))))))))))))))))))))))))))))))))))))
    (negb
      ((=) n0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ
        0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val escape_ascii : char -> char list **)

let escape_ascii c =
  let n0 = nat_of_ascii c in
  if plain_char n0
  then c::[]
  else append ('\\'::('u'::('{'::[])))
         ((hex_digit
            (Nat.div n0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
              (Stdlib.Int.succ 0))))))))))))))))))::((hex_digit
                                                       (Nat.modulo n0
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         0))))))))))))))))))::('}'::[])))

(** val escape_string : char list -> char list **)

let rec escape_string = function
| [] -> []
| c::s' -> append (escape_ascii c) (escape_string s')

(** val f_string_literal : char list -> char list **)

let f_string_literal s =
  append quote_string (append (escape_string s) quote_string)

(** val string_literal : char list -> term **)

let string_literal s =
  TApp ((IdSimple (f_string_literal s)), None, [])

(** val s_seq : identifier **)

let s_seq =
  IdSimple ('S'::('e'::('q'::[])))

(** val _UU03c4__seq : sort -> sort **)

let _UU03c4__seq _UU03c4_ =
  SApp (s_seq, (_UU03c4_ :: []))

(** val f_seq_empty : identifier **)

let f_seq_empty =
  IdSimple ('s'::('e'::('q'::('.'::('e'::('m'::('p'::('t'::('y'::[])))))))))

(** val f_seq_unit : identifier **)

let f_seq_unit =
  IdSimple ('s'::('e'::('q'::('.'::('u'::('n'::('i'::('t'::[]))))))))

(** val f_seq_concat : identifier **)

let f_seq_concat =
  IdSimple ('s'::('e'::('q'::('.'::('+'::('+'::[]))))))

(** val f_seq_len : identifier **)

let f_seq_len =
  IdSimple ('s'::('e'::('q'::('.'::('l'::('e'::('n'::[])))))))

(** val f_seq_nth : identifier **)

let f_seq_nth =
  IdSimple ('s'::('e'::('q'::('.'::('n'::('t'::('h'::[])))))))

(** val f_seq_contains : identifier **)

let f_seq_contains =
  IdSimple
    ('s'::('e'::('q'::('.'::('c'::('o'::('n'::('t'::('a'::('i'::('n'::('s'::[]))))))))))))

(** val f_seq_map : identifier **)

let f_seq_map =
  IdSimple ('s'::('e'::('q'::('.'::('m'::('a'::('p'::[])))))))

(** val seq_empty : sort -> term **)

let seq_empty _UU03c3_ =
  TApp (f_seq_empty, (Some (_UU03c4__seq _UU03c3_)), [])

(** val seq_unit : term -> term **)

let seq_unit t =
  TApp (f_seq_unit, None, (t :: []))

(** val seq_concat : term -> term -> term **)

let seq_concat t1 t2 =
  TApp (f_seq_concat, None, (t1 :: (t2 :: [])))

(** val seq_len : term -> term **)

let seq_len t =
  TApp (f_seq_len, None, (t :: []))

(** val seq_nth : term -> term -> term **)

let seq_nth t1 t2 =
  TApp (f_seq_nth, None, (t1 :: (t2 :: [])))

(** val seq_contains : term -> term -> term **)

let seq_contains t1 t2 =
  TApp (f_seq_contains, None, (t1 :: (t2 :: [])))

(** val seq_map : term -> term -> term **)

let seq_map t1 t2 =
  TApp (f_seq_map, None, (t1 :: (t2 :: [])))

(** val s_null : identifier **)

let s_null =
  IdSimple ('N'::('u'::('l'::('l'::[]))))

(** val _UU03c3__null : sort **)

let _UU03c3__null =
  SApp (s_null, [])

(** val s_val : identifier **)

let s_val =
  IdSimple ('V'::('a'::('l'::[])))

(** val _UU03c3__val : sort **)

let _UU03c3__val =
  SApp (s_val, [])

(** val s_maybe_val : identifier **)

let s_maybe_val =
  IdSimple ('M'::('a'::('y'::('b'::('e'::('V'::('a'::('l'::[]))))))))

(** val _UU03c3__maybe_val : sort **)

let _UU03c3__maybe_val =
  SApp (s_maybe_val, [])

(** val s_adt : char list -> identifier **)

let s_adt _UU03b4_ =
  IdSimple
    (append ('D'::('a'::('t'::('a'::('t'::('y'::('p'::('e'::[]))))))))
      _UU03b4_)

(** val _UU03c3__adt : char list -> sort **)

let _UU03c3__adt _UU03b4_ =
  SApp ((s_adt _UU03b4_), [])

(** val _UU03c3__list : sort **)

let _UU03c3__list =
  _UU03c4__seq _UU03c3__val

(** val encode_type : type0 -> sort **)

let rec encode_type = function
| TVal -> _UU03c3__val
| TNull -> _UU03c3__null
| TGillianNone -> _UU03c3__val
| TGillianEmpty -> _UU03c3__val
| TGillianLoc -> _UU03c3__val
| TBool -> _UU03c3__bool
| TNat -> _UU03c3__int
| TRat -> _UU03c3__real
| TString -> _UU03c3__string
| TADT _UU03b4_ -> _UU03c3__adt _UU03b4_
| TList _UU03c4_0 -> _UU03c4__seq (encode_type _UU03c4_0)

(** val f_repr_nat : identifier **)

let f_repr_nat =
  IdSimple ('r'::('e'::('p'::('r'::('N'::('a'::('t'::[])))))))

(** val f_repr_rat : identifier **)

let f_repr_rat =
  IdSimple ('r'::('e'::('p'::('r'::('R'::('a'::('t'::[])))))))

(** val f_repr_val : identifier **)

let f_repr_val =
  IdSimple ('r'::('e'::('p'::('r'::('V'::('a'::('l'::[])))))))

(** val f_repr_adt : char list -> identifier **)

let f_repr_adt _UU03b4_ =
  IdSimple
    (append ('r'::('e'::('p'::('r'::('A'::('D'::('T'::[]))))))) _UU03b4_)

(** val repr_nat : term -> term **)

let repr_nat t =
  TApp (f_repr_nat, None, (t :: []))

(** val repr_rat : term -> term **)

let repr_rat t =
  TApp (f_repr_rat, None, (t :: []))

(** val repr_val : term -> term **)

let repr_val t =
  TApp (f_repr_val, None, (t :: []))

(** val repr_adt : char list -> term -> term **)

let repr_adt _UU03b4_ t =
  TApp ((f_repr_adt _UU03b4_), None, (t :: []))

(** val f_lfunc : char list -> identifier **)

let f_lfunc f =
  IdSimple (append ('l'::('f'::('u'::('n'::('c'::[]))))) f)

(** val f_lfunc_pre : char list -> identifier **)

let f_lfunc_pre f =
  IdSimple
    (append ('p'::('r'::('e'::('_'::('l'::('f'::('u'::('n'::('c'::[])))))))))
      f)

(** val lfunc_ : char list -> sort option -> term list -> term **)

let lfunc_ f ts x =
  TApp ((f_lfunc f), ts, x)

(** val lfunc_pre_ : char list -> sort option -> term list -> term **)

let lfunc_pre_ f ts x =
  TApp ((f_lfunc_pre f), ts, x)

(** val c_null : identifier **)

let c_null =
  IdSimple ('n'::('u'::('l'::('l'::[]))))

(** val null : term **)

let null =
  TApp (c_null, None, [])

(** val c_null_val : identifier **)

let c_null_val =
  IdSimple ('n'::('u'::('l'::('l'::('V'::('a'::('l'::[])))))))

(** val c_gillian_none_val : identifier **)

let c_gillian_none_val =
  IdSimple ('n'::('o'::('n'::('e'::[]))))

(** val c_gillian_empty_val : identifier **)

let c_gillian_empty_val =
  IdSimple ('e'::('m'::('p'::('t'::('y'::[])))))

(** val c_gillian_loc_val : identifier **)

let c_gillian_loc_val =
  IdSimple ('l'::('o'::('c'::[])))

(** val c_bool_val : identifier **)

let c_bool_val =
  IdSimple ('b'::('o'::('o'::('l'::[]))))

(** val c_nat_val : identifier **)

let c_nat_val =
  IdSimple ('n'::('a'::('t'::[])))

(** val c_rat_val : identifier **)

let c_rat_val =
  IdSimple ('r'::('a'::('t'::[])))

(** val c_string_val : identifier **)

let c_string_val =
  IdSimple ('s'::('t'::('r'::('i'::('n'::('g'::[]))))))

(** val c_list_val : identifier **)

let c_list_val =
  IdSimple ('l'::('i'::('s'::('t'::[]))))

(** val c_adt_val : char list -> identifier **)

let c_adt_val _UU03b4_ =
  IdSimple (append ('a'::('d'::('t'::[]))) _UU03b4_)

(** val null_val_of : term -> term **)

let null_val_of t =
  TApp (c_null_val, None, (t :: []))

(** val null_val : term **)

let null_val =
  null_val_of null

(** val gillian_none_val : term **)

let gillian_none_val =
  TApp (c_gillian_none_val, None, [])

(** val gillian_empty_val : term **)

let gillian_empty_val =
  TApp (c_gillian_empty_val, None, [])

(** val gillian_loc_val : term -> term **)

let gillian_loc_val t =
  TApp (c_gillian_loc_val, None, (t :: []))

(** val bool_val : term -> term **)

let bool_val t =
  TApp (c_bool_val, None, (t :: []))

(** val nat_val : term -> term **)

let nat_val t =
  TApp (c_nat_val, None, (t :: []))

(** val rat_val : term -> term **)

let rat_val t =
  TApp (c_rat_val, None, (t :: []))

(** val string_val : term -> term **)

let string_val t =
  TApp (c_string_val, None, (t :: []))

(** val list_val : term -> term **)

let list_val t =
  TApp (c_list_val, None, (t :: []))

(** val adt_val : char list -> term -> term **)

let adt_val _UU03b4_ t =
  TApp ((c_adt_val _UU03b4_), None, (t :: []))

(** val c_some_val : identifier **)

let c_some_val =
  IdSimple ('S'::('o'::('m'::('e'::[]))))

(** val c_constructor_adt : char list -> identifier **)

let c_constructor_adt c =
  IdSimple
    (append
      ('c'::('o'::('n'::('s'::('t'::('r'::('u'::('c'::('t'::('o'::('r'::[])))))))))))
      c)

(** val constructor_adt : char list -> sort option -> term list -> term **)

let constructor_adt c ts x =
  TApp ((c_constructor_adt c), ts, x)

(** val constructors_for_adt_sort_map : (identifier, identifier gset) gmap **)

let constructors_for_adt_sort_map =
  let update = fun c pat m ->
    let (_, _UU03b4_) = pat in
    let s__UU03b4_ = s_adt _UU03b4_ in
    let prev =
      from_option (Obj.magic id)
        (empty0 (gset_empty identifier_eq_decision identifier_countable))
        (lookup0 (gmap_lookup identifier_eq_decision identifier_countable)
          s__UU03b4_ m)
    in
    insert0
      (map_insert
        (Obj.magic gmap_partial_alter identifier_eq_decision
          identifier_countable))
      s__UU03b4_
      (union0 (gset_union identifier_eq_decision identifier_countable) prev
        (singleton0
          (gset_singleton identifier_eq_decision identifier_countable)
          (c_constructor_adt c)))
      m
  in
  map_fold (fun _ -> gmap_fold String.eq_dec String.countable)
    (Obj.magic update)
    (empty0 (gmap_empty identifier_eq_decision identifier_countable))
    (gmap_empty String.eq_dec String.countable)

(** val g_null_val : identifier **)

let g_null_val =
  IdSimple ('g'::('e'::('t'::('N'::('u'::('l'::('l'::[])))))))

(** val g_gillian_loc_val : identifier **)

let g_gillian_loc_val =
  IdSimple ('g'::('e'::('t'::('L'::('o'::('c'::[]))))))

(** val g_bool_val : identifier **)

let g_bool_val =
  IdSimple ('g'::('e'::('t'::('B'::('o'::('o'::('l'::[])))))))

(** val g_nat_val : identifier **)

let g_nat_val =
  IdSimple ('g'::('e'::('t'::('N'::('a'::('t'::[]))))))

(** val g_rat_val : identifier **)

let g_rat_val =
  IdSimple ('g'::('e'::('t'::('R'::('a'::('t'::[]))))))

(** val g_string_val : identifier **)

let g_string_val =
  IdSimple ('g'::('e'::('t'::('S'::('t'::('r'::[]))))))

(** val g_list_val : identifier **)

let g_list_val =
  IdSimple ('g'::('e'::('t'::('L'::('i'::('s'::('t'::[])))))))

(** val g_adt_val : char list -> identifier **)

let g_adt_val _UU03b4_ =
  IdSimple (append ('g'::('e'::('t'::('A'::('D'::('T'::[])))))) _UU03b4_)

(** val get_null_val : term -> term **)

let get_null_val t =
  TApp (g_null_val, None, (t :: []))

(** val get_bool_val : term -> term **)

let get_bool_val t =
  TApp (g_bool_val, None, (t :: []))

(** val get_nat_val : term -> term **)

let get_nat_val t =
  TApp (g_nat_val, None, (t :: []))

(** val get_rat_val : term -> term **)

let get_rat_val t =
  TApp (g_rat_val, None, (t :: []))

(** val get_string_val : term -> term **)

let get_string_val t =
  TApp (g_string_val, None, (t :: []))

(** val get_list_val : term -> term **)

let get_list_val t =
  TApp (g_list_val, None, (t :: []))

(** val get_adt_val : char list -> term -> term **)

let get_adt_val _UU03b4_ t =
  TApp ((g_adt_val _UU03b4_), None, (t :: []))

(** val g_some_val : identifier **)

let g_some_val =
  IdSimple ('g'::('e'::('t'::('S'::('o'::('m'::('e'::[])))))))

(** val get_some_val : term -> term **)

let get_some_val t =
  TApp (g_some_val, None, (t :: []))

(** val p_null_val : identifier **)

let p_null_val =
  identifier_add_prefix ('i'::('s'::('-'::[]))) c_null_val

(** val p_gillian_none_val : identifier **)

let p_gillian_none_val =
  identifier_add_prefix ('i'::('s'::('-'::[]))) c_gillian_none_val

(** val p_gillian_empty_val : identifier **)

let p_gillian_empty_val =
  identifier_add_prefix ('i'::('s'::('-'::[]))) c_gillian_empty_val

(** val p_gillian_loc_val : identifier **)

let p_gillian_loc_val =
  identifier_add_prefix ('i'::('s'::('-'::[]))) c_gillian_loc_val

(** val p_bool_val : identifier **)

let p_bool_val =
  identifier_add_prefix ('i'::('s'::('-'::[]))) c_bool_val

(** val p_nat_val : identifier **)

let p_nat_val =
  identifier_add_prefix ('i'::('s'::('-'::[]))) c_nat_val

(** val p_rat_val : identifier **)

let p_rat_val =
  identifier_add_prefix ('i'::('s'::('-'::[]))) c_rat_val

(** val p_string_val : identifier **)

let p_string_val =
  identifier_add_prefix ('i'::('s'::('-'::[]))) c_string_val

(** val p_list_val : identifier **)

let p_list_val =
  identifier_add_prefix ('i'::('s'::('-'::[]))) c_list_val

(** val p_adt_val : char list -> identifier **)

let p_adt_val _UU03b4_ =
  identifier_add_prefix ('i'::('s'::('-'::[]))) (c_adt_val _UU03b4_)

(** val is_null_val : term -> term **)

let is_null_val t =
  TApp (p_null_val, None, (t :: []))

(** val is_gillian_none_val : term -> term **)

let is_gillian_none_val t =
  TApp (p_gillian_none_val, None, (t :: []))

(** val is_gillian_empty_val : term -> term **)

let is_gillian_empty_val t =
  TApp (p_gillian_empty_val, None, (t :: []))

(** val is_gillian_loc_val : term -> term **)

let is_gillian_loc_val t =
  TApp (p_gillian_loc_val, None, (t :: []))

(** val is_bool_val : term -> term **)

let is_bool_val t =
  TApp (p_bool_val, None, (t :: []))

(** val is_nat_val : term -> term **)

let is_nat_val t =
  TApp (p_nat_val, None, (t :: []))

(** val is_rat_val : term -> term **)

let is_rat_val t =
  TApp (p_rat_val, None, (t :: []))

(** val is_string_val : term -> term **)

let is_string_val t =
  TApp (p_string_val, None, (t :: []))

(** val is_list_val : term -> term **)

let is_list_val t =
  TApp (p_list_val, None, (t :: []))

(** val is_adt_val : char list -> term -> term **)

let is_adt_val _UU03b4_ t =
  TApp ((p_adt_val _UU03b4_), None, (t :: []))

(** val p_some_val : identifier **)

let p_some_val =
  identifier_add_prefix ('i'::('s'::('-'::[]))) c_some_val

(** val is_some_val : term -> term **)

let is_some_val t =
  TApp (p_some_val, None, (t :: []))

(** val p_constructor_adt : char list -> identifier **)

let p_constructor_adt c =
  identifier_add_prefix ('i'::('s'::('-'::[]))) (c_constructor_adt c)

(** val is_constructor_adt : char list -> term -> term **)

let is_constructor_adt c t =
  TApp ((p_constructor_adt c), None, (t :: []))

(** val tester_for_constructor_adt_map : (identifier, identifier) gmap **)

let tester_for_constructor_adt_map =
  let update = fun c _ m ->
    insert0
      (map_insert
        (gmap_partial_alter identifier_eq_decision identifier_countable))
      (c_constructor_adt c) (p_constructor_adt c) m
  in
  map_fold (fun _ -> gmap_fold String.eq_dec String.countable) update
    (empty0 (gmap_empty identifier_eq_decision identifier_countable))
    (gmap_empty String.eq_dec String.countable)

(** val lookup_tester_for_constructor_adt : identifier -> identifier gset **)

let lookup_tester_for_constructor_adt c =
  match lookup0 (gmap_lookup identifier_eq_decision identifier_countable) c
          tester_for_constructor_adt_map with
  | Some p ->
    singleton0 (gset_singleton identifier_eq_decision identifier_countable) p
  | None -> empty0 (gset_empty identifier_eq_decision identifier_countable)

(** val is_type : type0 -> term -> term **)

let rec is_type _UU03c4_ t =
  match _UU03c4_ with
  | TVal -> true_
  | TNull -> is_null_val t
  | TGillianNone -> is_gillian_none_val t
  | TGillianEmpty -> is_gillian_empty_val t
  | TGillianLoc -> is_gillian_loc_val t
  | TBool -> is_bool_val t
  | TNat -> is_nat_val t
  | TRat -> is_rat_val t
  | TString -> is_string_val t
  | TADT _UU03b4_ -> is_adt_val _UU03b4_ t
  | TList _UU03c4_' ->
    let t' = get_list_val t in
    let fresh_var =
      fresh0
        (set_fresh (gset_elements String.eq_dec String.countable)
          string_infinite)
        (fv t)
    in
    let i = TFVar fresh_var in
    let in_bounds = and_ (geq_ i zero_int) (lt_ i (seq_len t')) in
    let nth_elem = seq_nth t' i in
    let elem_type_check = impl_ in_bounds (is_type _UU03c4_' nth_elem) in
    and_ (is_list_val t) (TForall (_UU03c3__int,
      (term_close (fresh_var :: []) 0 elem_type_check)))

(** val repr_list_body : (term -> term) -> term -> term **)

let repr_list_body repr_elem t =
  let i_var =
    fresh0
      (set_fresh (gset_elements String.eq_dec String.countable)
        string_infinite)
      (fv t)
  in
  let i = TFVar i_var in
  let in_bounds = and_ (geq_ i zero_int) (lt_ i (seq_len t)) in
  TForall (_UU03c3__int,
  (term_close (i_var :: []) 0 (impl_ in_bounds (repr_elem (seq_nth t i)))))

(** val repr_type_guard : type0 -> term -> term **)

let rec repr_type_guard _UU03c4_ t =
  match _UU03c4_ with
  | TVal -> repr_val t
  | TNat -> repr_nat t
  | TRat -> repr_rat t
  | TADT _UU03b4_ -> repr_adt _UU03b4_ t
  | TList _UU03c4_0 -> repr_list_body (repr_type_guard _UU03c4_0) t
  | _ -> true_

(** val to_val_base :
    term -> sort -> term gset -> ((term * sort) * term gset) option **)

let to_val_base t _UU03c3_ _UU03a6_ =
  let cases =
    insert0 (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
      _UU03c3__val ((t, _UU03c3__val), _UU03a6_)
      (insert0
        (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
        _UU03c3__maybe_val (((get_some_val t), _UU03c3__val),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (is_some_val t))))
        (insert0
          (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
          _UU03c3__null (((null_val_of t), _UU03c3__val), _UU03a6_)
          (insert0
            (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
            _UU03c3__bool (((bool_val t), _UU03c3__val), _UU03a6_)
            (insert0
              (map_insert
                (gmap_partial_alter sort_eq_decision sort_countable))
              _UU03c3__int (((nat_val t), _UU03c3__val), _UU03a6_)
              (insert0
                (map_insert
                  (gmap_partial_alter sort_eq_decision sort_countable))
                _UU03c3__real (((rat_val t), _UU03c3__val), _UU03a6_)
                (insert0
                  (map_insert
                    (gmap_partial_alter sort_eq_decision sort_countable))
                  _UU03c3__string (((string_val t), _UU03c3__val), _UU03a6_)
                  (singletonM0
                    (map_singleton
                      (gmap_partial_alter sort_eq_decision sort_countable)
                      (gmap_empty sort_eq_decision sort_countable))
                    _UU03c3__list (((list_val t), _UU03c3__val), _UU03a6_))))))))
  in
  let adt_cases =
    gset_to_gmap_with String.eq_dec String.countable sort_eq_decision
      sort_countable _UU03c3__adt (fun _UU03b4_ -> (((adt_val _UU03b4_ t),
      _UU03c3__val), _UU03a6_)) adts
  in
  lookup0 (gmap_lookup sort_eq_decision sort_countable) _UU03c3_
    (union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge sort_eq_decision sort_countable)))
      cases adt_cases)

(** val to_val_curried :
    term -> sort -> term gset -> ((term * sort) * term gset) option **)

let rec to_val_curried t _UU03c3_ _UU03a6_ =
  match _UU03c3_ with
  | SParam _ -> to_val_base t _UU03c3_ _UU03a6_
  | SApp (s, _UU03c4_s) ->
    (match _UU03c4_s with
     | [] -> to_val_base t _UU03c3_ _UU03a6_
     | _UU03c3_0 :: l ->
       (match l with
        | [] ->
          if decide
               (decide_rel identifier_eq_decision s (IdSimple
                 ('S'::('e'::('q'::[])))))
          then let x =
                 fresh_string_of_set []
                   (set_bind (gset_elements term_eq_decision term_countable)
                     (gset_empty String.eq_dec String.countable)
                     (gset_union String.eq_dec String.countable) fv _UU03a6_)
               in
               let elem = TFVar x in
               mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc ->
                 let (y, _) = enc in
                 let (to_val_elem, _) = y in
                 let elem_to_val = TLambda (_UU03c3_0,
                   (term_close (x :: []) 0 to_val_elem))
                 in
                 Some (((list_val (seq_map elem_to_val t)), _UU03c3__val),
                 _UU03a6_)) (to_val_curried elem _UU03c3_0 _UU03a6_)
          else to_val_base t (SApp (s, (_UU03c3_0 :: []))) _UU03a6_
        | _ :: _ -> to_val_base t _UU03c3_ _UU03a6_))

(** val to_val :
    ((term * sort) * term gset) -> ((term * sort) * term gset) option **)

let to_val = function
| (p, _UU03a6_) -> let (t, _UU03c3_) = p in to_val_curried t _UU03c3_ _UU03a6_

(** val guarded_val : term gset -> term -> term **)

let guarded_val _UU03a6_ t =
  ite_
    (ands_
      (elements0 (gset_elements term_eq_decision term_countable) _UU03a6_)
      true_)
    t null_val

(** val residual_guard : term gset -> term **)

let residual_guard _UU03a6_ =
  ands_ (elements0 (gset_elements term_eq_decision term_countable) _UU03a6_)
    true_

(** val list_coercion_residual : term -> char list -> term gset -> term **)

let list_coercion_residual input elem_var _UU03a6__elem =
  let i_var =
    fresh0
      (set_fresh (gset_elements String.eq_dec String.countable)
        string_infinite)
      (union0 (gset_union String.eq_dec String.countable)
        (union0 (gset_union String.eq_dec String.countable) (fv input)
          (singleton0 (gset_singleton String.eq_dec String.countable)
            elem_var))
        (set_bind (gset_elements term_eq_decision term_countable)
          (gset_empty String.eq_dec String.countable)
          (gset_union String.eq_dec String.countable) fv _UU03a6__elem))
  in
  let i = TFVar i_var in
  let in_bounds = and_ (geq_ i zero_int) (lt_ i (seq_len input)) in
  let elem = seq_nth input i in
  let elem_residuals =
    term_subst
      (singletonM0
        (map_singleton (gmap_partial_alter String.eq_dec String.countable)
          (gmap_empty String.eq_dec String.countable))
        elem_var elem)
      (residual_guard _UU03a6__elem)
  in
  TForall (_UU03c3__int,
  (term_close (i_var :: []) 0 (impl_ in_bounds elem_residuals)))

(** val to_null :
    ((term * sort) * term gset) -> ((term * sort) * term gset) option **)

let to_null = function
| (p, _UU03a6_) ->
  let (t, _UU03c3_) = p in
  let t_some = get_some_val t in
  let cases =
    insert0 (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
      _UU03c3__null ((t, _UU03c3__null), _UU03a6_)
      (insert0
        (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
        _UU03c3__val (((get_null_val t), _UU03c3__null),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (is_null_val t))))
        (singletonM0
          (map_singleton (gmap_partial_alter sort_eq_decision sort_countable)
            (gmap_empty sort_eq_decision sort_countable))
          _UU03c3__maybe_val (((get_null_val t_some), _UU03c3__null),
          (union0 (gset_union term_eq_decision term_countable)
            (union0 (gset_union term_eq_decision term_countable) _UU03a6_
              (singleton0 (gset_singleton term_eq_decision term_countable)
                (is_some_val t)))
            (singleton0 (gset_singleton term_eq_decision term_countable)
              (is_null_val t_some))))))
  in
  lookup0 (gmap_lookup sort_eq_decision sort_countable) _UU03c3_ cases

(** val to_gillian_value :
    (term -> term) -> ((term * sort) * term gset) -> ((term * sort) * term
    gset) option **)

let to_gillian_value tester = function
| (p, _UU03a6_) ->
  let (t, _UU03c3_) = p in
  let t_some = get_some_val t in
  let cases =
    insert0 (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
      _UU03c3__val ((t, _UU03c3__val),
      (union0 (gset_union term_eq_decision term_countable) _UU03a6_
        (singleton0 (gset_singleton term_eq_decision term_countable)
          (tester t))))
      (singletonM0
        (map_singleton (gmap_partial_alter sort_eq_decision sort_countable)
          (gmap_empty sort_eq_decision sort_countable)) _UU03c3__maybe_val
        ((t_some, _UU03c3__val),
        (union0 (gset_union term_eq_decision term_countable)
          (union0 (gset_union term_eq_decision term_countable) _UU03a6_
            (singleton0 (gset_singleton term_eq_decision term_countable)
              (is_some_val t)))
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (tester t_some)))))
  in
  lookup0 (gmap_lookup sort_eq_decision sort_countable) _UU03c3_ cases

(** val to_gillian_none :
    ((term * sort) * term gset) -> ((term * sort) * term gset) option **)

let to_gillian_none =
  to_gillian_value is_gillian_none_val

(** val to_gillian_empty :
    ((term * sort) * term gset) -> ((term * sort) * term gset) option **)

let to_gillian_empty =
  to_gillian_value is_gillian_empty_val

(** val to_gillian_loc :
    ((term * sort) * term gset) -> ((term * sort) * term gset) option **)

let to_gillian_loc =
  to_gillian_value is_gillian_loc_val

(** val to_bool :
    ((term * sort) * term gset) -> ((term * sort) * term gset) option **)

let to_bool = function
| (p, _UU03a6_) ->
  let (t, _UU03c3_) = p in
  let t_some = get_some_val t in
  let cases =
    insert0 (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
      _UU03c3__bool ((t, _UU03c3__bool), _UU03a6_)
      (insert0
        (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
        _UU03c3__val (((get_bool_val t), _UU03c3__bool),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (is_bool_val t))))
        (singletonM0
          (map_singleton (gmap_partial_alter sort_eq_decision sort_countable)
            (gmap_empty sort_eq_decision sort_countable))
          _UU03c3__maybe_val (((get_bool_val t_some), _UU03c3__bool),
          (union0 (gset_union term_eq_decision term_countable)
            (union0 (gset_union term_eq_decision term_countable) _UU03a6_
              (singleton0 (gset_singleton term_eq_decision term_countable)
                (is_some_val t)))
            (singleton0 (gset_singleton term_eq_decision term_countable)
              (is_bool_val t_some))))))
  in
  lookup0 (gmap_lookup sort_eq_decision sort_countable) _UU03c3_ cases

(** val to_nat0 :
    ((term * sort) * term gset) -> ((term * sort) * term gset) option **)

let to_nat0 = function
| (p, _UU03a6_) ->
  let (t, _UU03c3_) = p in
  let t_some = get_some_val t in
  let cases =
    insert0 (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
      _UU03c3__int ((t, _UU03c3__int), _UU03a6_)
      (insert0
        (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
        _UU03c3__val (((get_nat_val t), _UU03c3__int),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (is_nat_val t))))
        (singletonM0
          (map_singleton (gmap_partial_alter sort_eq_decision sort_countable)
            (gmap_empty sort_eq_decision sort_countable))
          _UU03c3__maybe_val (((get_nat_val t_some), _UU03c3__int),
          (union0 (gset_union term_eq_decision term_countable)
            (union0 (gset_union term_eq_decision term_countable) _UU03a6_
              (singleton0 (gset_singleton term_eq_decision term_countable)
                (is_some_val t)))
            (singleton0 (gset_singleton term_eq_decision term_countable)
              (is_nat_val t_some))))))
  in
  lookup0 (gmap_lookup sort_eq_decision sort_countable) _UU03c3_ cases

(** val to_rat :
    ((term * sort) * term gset) -> ((term * sort) * term gset) option **)

let to_rat = function
| (p, _UU03a6_) ->
  let (t, _UU03c3_) = p in
  let t_some = get_some_val t in
  let cases =
    insert0 (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
      _UU03c3__real ((t, _UU03c3__real), _UU03a6_)
      (insert0
        (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
        _UU03c3__val (((get_rat_val t), _UU03c3__real),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (is_rat_val t))))
        (singletonM0
          (map_singleton (gmap_partial_alter sort_eq_decision sort_countable)
            (gmap_empty sort_eq_decision sort_countable))
          _UU03c3__maybe_val (((get_rat_val t_some), _UU03c3__real),
          (union0 (gset_union term_eq_decision term_countable)
            (union0 (gset_union term_eq_decision term_countable) _UU03a6_
              (singleton0 (gset_singleton term_eq_decision term_countable)
                (is_some_val t)))
            (singleton0 (gset_singleton term_eq_decision term_countable)
              (is_rat_val t_some))))))
  in
  lookup0 (gmap_lookup sort_eq_decision sort_countable) _UU03c3_ cases

(** val to_string :
    ((term * sort) * term gset) -> ((term * sort) * term gset) option **)

let to_string = function
| (p, _UU03a6_) ->
  let (t, _UU03c3_) = p in
  let t_some = get_some_val t in
  let cases =
    insert0 (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
      _UU03c3__string ((t, _UU03c3__string), _UU03a6_)
      (insert0
        (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
        _UU03c3__val (((get_string_val t), _UU03c3__string),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (is_string_val t))))
        (singletonM0
          (map_singleton (gmap_partial_alter sort_eq_decision sort_countable)
            (gmap_empty sort_eq_decision sort_countable))
          _UU03c3__maybe_val (((get_string_val t_some), _UU03c3__string),
          (union0 (gset_union term_eq_decision term_countable)
            (union0 (gset_union term_eq_decision term_countable) _UU03a6_
              (singleton0 (gset_singleton term_eq_decision term_countable)
                (is_some_val t)))
            (singleton0 (gset_singleton term_eq_decision term_countable)
              (is_string_val t_some))))))
  in
  lookup0 (gmap_lookup sort_eq_decision sort_countable) _UU03c3_ cases

(** val to_adt :
    char list -> ((term * sort) * term gset) -> ((term * sort) * term gset)
    option **)

let to_adt _UU03b4_ = function
| (p, _UU03a6_) ->
  let (t, _UU03c3_) = p in
  let t_some = get_some_val t in
  let cases =
    insert0 (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
      (_UU03c3__adt _UU03b4_) ((t, (_UU03c3__adt _UU03b4_)), _UU03a6_)
      (insert0
        (map_insert (gmap_partial_alter sort_eq_decision sort_countable))
        _UU03c3__val (((get_adt_val _UU03b4_ t), (_UU03c3__adt _UU03b4_)),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (is_adt_val _UU03b4_ t))))
        (singletonM0
          (map_singleton (gmap_partial_alter sort_eq_decision sort_countable)
            (gmap_empty sort_eq_decision sort_countable))
          _UU03c3__maybe_val (((get_adt_val _UU03b4_ t_some),
          (_UU03c3__adt _UU03b4_)),
          (union0 (gset_union term_eq_decision term_countable)
            (union0 (gset_union term_eq_decision term_countable) _UU03a6_
              (singleton0 (gset_singleton term_eq_decision term_countable)
                (is_some_val t)))
            (singleton0 (gset_singleton term_eq_decision term_countable)
              (is_adt_val _UU03b4_ t_some))))))
  in
  lookup0 (gmap_lookup sort_eq_decision sort_countable) _UU03c3_ cases

(** val to_type_curried_list_binder : term gset -> term -> char list **)

let to_type_curried_list_binder _UU03a6_ t =
  fresh_string_of_set []
    (union0 (gset_union String.eq_dec String.countable)
      (set_bind (gset_elements term_eq_decision term_countable)
        (gset_empty String.eq_dec String.countable)
        (gset_union String.eq_dec String.countable) fv _UU03a6_)
      (fv t))

(** val to_type_curried :
    type0 -> term -> sort -> term gset -> ((term * sort) * term gset) option **)

let rec to_type_curried _UU03c4_ t _UU03c3_ _UU03a6_ =
  match _UU03c4_ with
  | TVal -> to_val ((t, _UU03c3_), _UU03a6_)
  | TNull -> to_null ((t, _UU03c3_), _UU03a6_)
  | TGillianNone -> to_gillian_none ((t, _UU03c3_), _UU03a6_)
  | TGillianEmpty -> to_gillian_empty ((t, _UU03c3_), _UU03a6_)
  | TGillianLoc -> to_gillian_loc ((t, _UU03c3_), _UU03a6_)
  | TBool -> to_bool ((t, _UU03c3_), _UU03a6_)
  | TNat -> to_nat0 ((t, _UU03c3_), _UU03a6_)
  | TRat -> to_rat ((t, _UU03c3_), _UU03a6_)
  | TString -> to_string ((t, _UU03c3_), _UU03a6_)
  | TADT s -> to_adt s ((t, _UU03c3_), _UU03a6_)
  | TList t0 ->
    (match _UU03c3_ with
     | SParam _ -> None
     | SApp (s, _UU03c4_s) ->
       (match _UU03c4_s with
        | [] ->
          if decide
               (decide_rel identifier_eq_decision s (IdSimple
                 ('V'::('a'::('l'::[])))))
          then let x = to_type_curried_list_binder _UU03a6_ t in
               let elem = TFVar x in
               mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc ->
                 let (y, _UU03a6__elem) = enc in
                 let (to_type_elem, _) = y in
                 let elem_to_type = TLambda (_UU03c3__val,
                   (term_close (x :: []) 0 to_type_elem))
                 in
                 let val_list = get_list_val t in
                 Some (((seq_map elem_to_type val_list),
                 (_UU03c4__seq (encode_type t0))),
                 (union0 (gset_union term_eq_decision term_countable)
                   (union0 (gset_union term_eq_decision term_countable)
                     _UU03a6_
                     (singleton0
                       (gset_singleton term_eq_decision term_countable)
                       (is_list_val t)))
                   (singleton0
                     (gset_singleton term_eq_decision term_countable)
                     (list_coercion_residual val_list x _UU03a6__elem)))))
                 (to_type_curried t0 elem _UU03c3__val _UU03a6_)
          else if decide
                    (decide_rel sort_eq_decision (SApp (s, []))
                      _UU03c3__maybe_val)
               then let t_some = get_some_val t in
                    let x = to_type_curried_list_binder _UU03a6_ t in
                    let elem = TFVar x in
                    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc ->
                      let (y, _UU03a6__elem) = enc in
                      let (to_type_elem, _) = y in
                      let elem_to_type = TLambda (_UU03c3__val,
                        (term_close (x :: []) 0 to_type_elem))
                      in
                      let val_list = get_list_val t_some in
                      Some (((seq_map elem_to_type val_list),
                      (_UU03c4__seq (encode_type t0))),
                      (union0 (gset_union term_eq_decision term_countable)
                        (union0 (gset_union term_eq_decision term_countable)
                          (union0
                            (gset_union term_eq_decision term_countable)
                            _UU03a6_
                            (singleton0
                              (gset_singleton term_eq_decision term_countable)
                              (is_some_val t)))
                          (singleton0
                            (gset_singleton term_eq_decision term_countable)
                            (is_list_val t_some)))
                        (singleton0
                          (gset_singleton term_eq_decision term_countable)
                          (list_coercion_residual val_list x _UU03a6__elem)))))
                      (to_type_curried t0 elem _UU03c3__val _UU03a6_)
               else None
        | _UU03c3_' :: l ->
          (match l with
           | [] ->
             mbind (Obj.magic (fun _ _ -> option_bind)) (fun _ ->
               if decide
                    (decide_rel sort_eq_decision _UU03c3_' (encode_type t0))
               then Some ((t, (_UU03c4__seq _UU03c3_')), _UU03a6_)
               else let x = to_type_curried_list_binder _UU03a6_ t in
                    let elem = TFVar x in
                    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc ->
                      let (y, _UU03a6__elem) = enc in
                      let (to_type_elem, _) = y in
                      let elem_to_type = TLambda (_UU03c3_',
                        (term_close (x :: []) 0 to_type_elem))
                      in
                      Some (((seq_map elem_to_type t),
                      (_UU03c4__seq (encode_type t0))),
                      (union0 (gset_union term_eq_decision term_countable)
                        _UU03a6_
                        (singleton0
                          (gset_singleton term_eq_decision term_countable)
                          (list_coercion_residual t x _UU03a6__elem)))))
                      (to_type_curried t0 elem _UU03c3_' _UU03a6_))
               (guard_or () (Obj.magic (fun _ -> option_mfail))
                 (Obj.magic (fun _ -> option_ret))
                 (decide_rel identifier_eq_decision s (IdSimple
                   ('S'::('e'::('q'::[]))))))
           | _ :: _ -> None)))

(** val to_list :
    type0 -> ((term * sort) * term gset) -> ((term * sort) * term gset) option **)

let to_list _UU03c4_ = function
| (p, _UU03a6_) ->
  let (t, _UU03c3_) = p in
  to_type_curried (TList _UU03c4_) t _UU03c3_ _UU03a6_

(** val to_type :
    type0 -> ((term * sort) * term gset) -> ((term * sort) * term gset) option **)

let to_type _UU03c4_ = function
| (p, _UU03a6_) ->
  let (t, _UU03c3_) = p in to_type_curried _UU03c4_ t _UU03c3_ _UU03a6_

(** val encode_preval : preval -> ((term * sort) * term gset) option **)

let rec encode_preval = function
| PVNull ->
  Some ((null, _UU03c3__null),
    (empty0 (gset_empty term_eq_decision term_countable)))
| PVGillianNone ->
  Some ((gillian_none_val, _UU03c3__val),
    (empty0 (gset_empty term_eq_decision term_countable)))
| PVGillianEmpty ->
  Some ((gillian_empty_val, _UU03c3__val),
    (empty0 (gset_empty term_eq_decision term_countable)))
| PVGillianLoc i ->
  let rec positive_of_int n0 =
    if n0 <= 1 then XH
    else
      let q = n0 / 2 in
      if n0 mod 2 = 0 then XO (positive_of_int q) else XI (positive_of_int q)
  in
  let z = if i <= 0 then Z0 else Zpos (positive_of_int i) in
  Some (((gillian_loc_val (int_literal z)), _UU03c3__val),
    (empty0 (gset_empty term_eq_decision term_countable)))
| PVBool b ->
  Some (((bool_ b), _UU03c3__bool),
    (empty0 (gset_empty term_eq_decision term_countable)))
| PVNat n0 ->
  Some (((int_literal (Z.of_nat n0)), _UU03c3__int),
    (empty0 (gset_empty term_eq_decision term_countable)))
| PVRat q0 ->
  Some (((decimal_literal q0), _UU03c3__real),
    (empty0 (gset_empty term_eq_decision term_countable)))
| PVString s ->
  Some (((string_literal s), _UU03c3__string),
    (empty0 (gset_empty term_eq_decision term_countable)))
| PVADT (_, c, vs) ->
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
    let (_UU03c4_s, _UU03b4_') = pat in
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun _ ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun encs ->
        let ts =
          map (fun pat0 -> let (y, _) = pat0 in let (t, _) = y in t) encs
        in
        let _UU03a6_s =
          map (fun pat0 -> let (_, _UU03a6_) = pat0 in _UU03a6_) encs
        in
        Some (((constructor_adt c None ts), (_UU03c3__adt _UU03b4_')),
        (union_list (gset_empty term_eq_decision term_countable)
          (gset_union term_eq_decision term_countable) _UU03a6_s)))
        (Coq0_list.mapM (Obj.magic (fun _ _ -> option_bind))
          (Obj.magic (fun _ -> option_ret)) (Obj.magic id)
          (zip_with (fun v0 _UU03c4_ ->
            mbind (Obj.magic (fun _ _ -> option_bind)) (to_type _UU03c4_)
              (encode_preval v0))
            vs _UU03c4_s)))
      (guard_or () (Obj.magic (fun _ -> option_mfail))
        (Obj.magic (fun _ -> option_ret))
        (decide_rel Coq_Nat.eq_dec (length vs) (length _UU03c4_s))))
    (lookup0 (Obj.magic gmap_lookup String.eq_dec String.countable) c
      (gmap_empty String.eq_dec String.countable))
| PVList vs ->
  fold_right (fun v0 acc ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc_val ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc_list ->
        let (y, _UU03a6_) = enc_list in
        let (t, _) = y in
        let (y0, _UU03a6_') = enc_val in
        let (t', _) = y0 in
        Some (((seq_concat (seq_unit t') t), _UU03c3__list),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_
          _UU03a6_')))
        acc)
      (mbind (Obj.magic (fun _ _ -> option_bind)) to_val (encode_preval v0)))
    (Some (((seq_empty _UU03c3__val), _UU03c3__list),
    (empty0 (gset_empty term_eq_decision term_countable)))) vs

(** val encode_lvar :
    type0 option -> char list -> ((term * sort) * term gset) option **)

let encode_lvar _UU03c4_ x =
  let x' = TFVar x in
  (match _UU03c4_ with
   | Some _UU03c4_0 ->
     Some ((x', (encode_type _UU03c4_0)),
       (empty0 (gset_empty term_eq_decision term_countable)))
   | None ->
     Some ((x', _UU03c3__maybe_val),
       (empty0 (gset_empty term_eq_decision term_countable))))

(** val encode_op1 :
    op1 -> ((term * sort) * term gset) -> ((term * sort) * term gset) option **)

let encode_op1 op enc =
  match op with
  | Op1Not ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc' ->
      let (y, _UU03a6_) = enc' in
      let (t, _) = y in Some (((not_ t), _UU03c3__bool), _UU03a6_))
      (to_bool enc)
  | Op1Length ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc' ->
      let (y, _UU03a6_) = enc' in
      let (t, _) = y in Some (((seq_len t), _UU03c3__int), _UU03a6_))
      (to_list TVal enc)
  | Op1IsInt ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc' ->
      let (y, _UU03a6_) = enc' in
      let (t, _) = y in Some (((is_int t), _UU03c3__bool), _UU03a6_))
      (to_rat enc)
  | Op1AsInt ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc' ->
      let (y, _UU03a6_) = enc' in
      let (t, _) = y in Some (((to_int t), _UU03c3__int), _UU03a6_))
      (to_rat enc)
  | Op1AsNum ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc' ->
      let (y, _UU03a6_) = enc' in
      let (t, _) = y in
      Some (((to_real t), _UU03c3__real),
      (union0 (gset_union term_eq_decision term_countable) _UU03a6_
        (singleton0 (gset_singleton term_eq_decision term_countable)
          (lt_ zero_int t)))))
      (to_nat0 enc)

(** val encode_op2 :
    op2 -> ((term * sort) * term gset) -> ((term * sort) * term gset) ->
    ((term * sort) * term gset) option **)

let encode_op2 op enc1 enc2 =
  match op with
  | Op2Eq ->
    let (p, _UU03a6_1) = enc1 in
    let (t1, _UU03c3_1) = p in
    let (p0, _UU03a6_2) = enc2 in
    let (t2, _UU03c3_2) = p0 in
    if decide (decide_rel sort_eq_decision _UU03c3_1 _UU03c3_2)
    then if decide (decide_rel sort_eq_decision _UU03c3_1 _UU03c3__maybe_val)
         then mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
                mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
                  let (y, _UU03a6_3) = enc1' in
                  let (t3, _) = y in
                  let (y0, _UU03a6_4) = enc2' in
                  let (t4, _) = y0 in
                  Some (((eq_ t3 t4), _UU03c3__bool),
                  (union0 (gset_union term_eq_decision term_countable)
                    _UU03a6_3 _UU03a6_4)))
                  (to_val enc2))
                (to_val enc1)
         else Some (((eq_ t1 t2), _UU03c3__bool),
                (union0 (gset_union term_eq_decision term_countable)
                  _UU03a6_1 _UU03a6_2))
    else mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
             let (y, _UU03a6_3) = enc1' in
             let (t3, _) = y in
             let (y0, _UU03a6_4) = enc2' in
             let (t4, _) = y0 in
             Some (((eq_ t3 t4), _UU03c3__bool),
             (union0 (gset_union term_eq_decision term_countable) _UU03a6_3
               _UU03a6_4)))
             (to_val enc2))
           (to_val enc1)
  | Op2And ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((and_ t1 t2), _UU03c3__bool),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
          _UU03a6_2)))
        (to_bool enc2))
      (to_bool enc1)
  | Op2Add ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((plus_ t1 t2), _UU03c3__int),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
          _UU03a6_2)))
        (to_nat0 enc2))
      (to_nat0 enc1)
  | Op2Sub ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        let diff = minus_ t1 t2 in
        Some (((ite_ (lt_ diff zero_int) zero_int diff), _UU03c3__int),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
          _UU03a6_2)))
        (to_nat0 enc2))
      (to_nat0 enc1)
  | Op2Mul ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((times_ t1 t2), _UU03c3__int),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
          _UU03a6_2)))
        (to_nat0 enc2))
      (to_nat0 enc1)
  | Op2Div ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((idiv_ t1 t2), _UU03c3__int),
        (union0 (gset_union term_eq_decision term_countable)
          (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
            _UU03a6_2)
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (not_ (eq_ t2 zero_int))))))
        (to_nat0 enc2))
      (to_nat0 enc1)
  | Op2Mod ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((mod_ t1 t2), _UU03c3__int),
        (union0 (gset_union term_eq_decision term_countable)
          (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
            _UU03a6_2)
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (not_ (eq_ t2 zero_int))))))
        (to_nat0 enc2))
      (to_nat0 enc1)
  | Op2Lt ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((lt_ t1 t2), _UU03c3__bool),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
          _UU03a6_2)))
        (to_nat0 enc2))
      (to_nat0 enc1)
  | Op2Cons ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((seq_concat (seq_unit t1) t2), _UU03c3__list),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
          _UU03a6_2)))
        (to_list TVal enc2))
      (to_val enc1)
  | Op2In ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((seq_contains t2 (seq_unit t1)), _UU03c3__bool),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
          _UU03a6_2)))
        (to_list TVal enc2))
      (to_val enc1)
  | Op2Cat ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((seq_concat t1 t2), _UU03c3__list),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
          _UU03a6_2)))
        (to_list TVal enc2))
      (to_list TVal enc1)
  | Op2Nth ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((seq_nth t1 t2), _UU03c3__val),
        (union0 (gset_union term_eq_decision term_countable)
          (union0 (gset_union term_eq_decision term_countable)
            (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
              _UU03a6_2)
            (singleton0 (gset_singleton term_eq_decision term_countable)
              (leq_ zero_int t2)))
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (lt_ t2 (seq_len t1))))))
        (to_nat0 enc2))
      (to_list TVal enc1)
  | Op2RAdd ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((plus_ t1 t2), _UU03c3__real),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
          _UU03a6_2)))
        (to_rat enc2))
      (to_rat enc1)
  | Op2RSub ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((minus_ t1 t2), _UU03c3__real),
        (union0 (gset_union term_eq_decision term_countable)
          (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
            _UU03a6_2)
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (lt_ t2 t1)))))
        (to_rat enc2))
      (to_rat enc1)
  | Op2RMul ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((times_ t1 t2), _UU03c3__real),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
          _UU03a6_2)))
        (to_rat enc2))
      (to_rat enc1)
  | Op2RDiv ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((div_ t1 t2), _UU03c3__real),
        (union0 (gset_union term_eq_decision term_countable)
          (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
            _UU03a6_2)
          (singleton0 (gset_singleton term_eq_decision term_countable)
            (not_ (eq_ t2 zero_real))))))
        (to_rat enc2))
      (to_rat enc1)
  | Op2RLt ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((lt_ t1 t2), _UU03c3__bool),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
          _UU03a6_2)))
        (to_rat enc2))
      (to_rat enc1)
  | Op2RLe ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2' ->
        let (y, _UU03a6_1) = enc1' in
        let (t1, _) = y in
        let (y0, _UU03a6_2) = enc2' in
        let (t2, _) = y0 in
        Some (((leq_ t1 t2), _UU03c3__bool),
        (union0 (gset_union term_eq_decision term_countable) _UU03a6_1
          _UU03a6_2)))
        (to_rat enc2))
      (to_rat enc1)

(** val encode_sexp_list :
    (char list, type0) gmap -> sexp list -> ((char list, type0) gmap -> sexp
    -> __ -> ((term * sort) * term gset) option) -> ((term * sort) * term
    gset) list option **)

let rec encode_sexp_list t es encode0 =
  match es with
  | [] -> Some []
  | s :: l ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun encs -> Some
        (enc :: encs)) (encode_sexp_list t l (fun t0 e _ -> encode0 t0 e __)))
      (Obj.magic encode0 t s __)

(** val encode_sexp_match_cases :
    (char list, type0) gmap -> char list -> term -> (pattern * sexp) list ->
    ((char list, type0) gmap -> sexp -> sexp -> __ -> __ ->
    ((term * sort) * term gset) option) -> (((pattern0 * term)
    list * (pattern0 * term) list) * term gset) option **)

let rec encode_sexp_match_cases t _UU03b4_ t0 pes encode0 =
  match pes with
  | [] ->
    Some (([], []), (empty0 (gset_empty term_eq_decision term_countable)))
  | p :: l ->
    let (p0, s) = p in
    (match p0 with
     | PatVar ->
       let pts_res__UU03a6_s_opt =
         encode_sexp_match_cases t _UU03b4_ t0 l (fun t1 e e' _ _ ->
           encode0 t1 e e' __ __)
       in
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun pts_res__UU03a6_s ->
         let (y, _UU03a6_s) = pts_res__UU03a6_s in
         let (pts, residual_pts) = y in
         let x =
           fresh0
             (set_fresh (gset_elements String.eq_dec String.countable)
               string_infinite)
             (lV_sexp s)
         in
         let e' = sexp_open 0 ((SEFLVar x) :: []) s in
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc ->
           let (y0, _UU03a6__i) = enc in
           let (t_i, _) = y0 in
           let t_i' = term_close (x :: []) 0 (guarded_val _UU03a6__i t_i) in
           let _UU03d5__i' =
             term_close (x :: []) 0 (residual_guard _UU03a6__i)
           in
           Some ((((PVar, t_i') :: pts), ((PVar,
           _UU03d5__i') :: residual_pts)), _UU03a6_s))
           (mbind (Obj.magic (fun _ _ -> option_bind)) (Obj.magic to_val)
             (Obj.magic encode0
               (insert0
                 (map_insert
                   (gmap_partial_alter String.eq_dec String.countable))
                 x (TADT _UU03b4_) t)
               s e' __ __)))
         pts_res__UU03a6_s_opt
     | PatCtor ctor ->
       let pts_res__UU03a6_s_opt =
         encode_sexp_match_cases t _UU03b4_ t0 l (fun t1 e e' _ _ ->
           encode0 t1 e e' __ __)
       in
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun pts_res__UU03a6_s ->
         let (y, _UU03a6_s) = pts_res__UU03a6_s in
         let (pts, residual_pts) = y in
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun decl ->
           let (_UU03c4_s, _UU03b4_') = decl in
           let xs = fresh_strings_of_set [] (length _UU03c4_s) (lV_sexp s) in
           let e' = sexp_open 0 (map (fun x -> SEFLVar x) xs) s in
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc ->
             let (y0, _UU03a6__i) = enc in
             let (t_i, _) = y0 in
             let t_i' = term_close xs 0 (guarded_val _UU03a6__i t_i) in
             let _UU03d5__i' = term_close xs 0 (residual_guard _UU03a6__i) in
             mbind (Obj.magic (fun _ _ -> option_bind)) (fun _ -> Some
               (((((PApp ((c_constructor_adt ctor), (length _UU03c4_s))),
               t_i') :: pts), (((PApp ((c_constructor_adt ctor),
               (length _UU03c4_s))), _UU03d5__i') :: residual_pts)),
               _UU03a6_s))
               (guard_or () (Obj.magic (fun _ -> option_mfail))
                 (Obj.magic (fun _ -> option_ret))
                 (decide_rel String.eq_dec _UU03b4_ _UU03b4_')))
             (mbind (Obj.magic (fun _ _ -> option_bind)) (Obj.magic to_val)
               (Obj.magic encode0
                 (union0
                   (map_union
                     (Obj.magic (fun _ _ _ ->
                       gmap_merge String.eq_dec String.countable)))
                   (list_to_map
                     (map_insert
                       (gmap_partial_alter String.eq_dec String.countable))
                     (gmap_empty String.eq_dec String.countable)
                     (zip_with (fun x x0 -> (x, x0)) xs _UU03c4_s))
                   t)
                 s e' __ __)))
           (lookup0 (Obj.magic gmap_lookup String.eq_dec String.countable)
             ctor (gmap_empty String.eq_dec String.countable)))
         pts_res__UU03a6_s_opt)

(** val encode_sexp :
    (char list, type0) gmap -> sexp -> ((term * sort) * term gset) option **)

let encode_sexp a b =
  let rec fix_F x =
    let t = let pr1,_ = x in pr1 in
    let encode_sexp0 = fun a0 b0 -> let y = a0,b0 in (fun _ -> fix_F y) in
    (match let _,pr2 = x in pr2 with
     | SEVal v -> encode_preval v
     | SEFLVar var ->
       encode_lvar
         (lookup0 (gmap_lookup String.eq_dec String.countable) var t) var
     | SEBLVar (_, _) -> None
     | SEADT (adt, ctor, es) ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
         let (_UU03c4_s, _UU03b4_') = pat in
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun _ ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun _ ->
             mbind (Obj.magic (fun _ _ -> option_bind)) (fun encs' ->
               mbind (Obj.magic (fun _ _ -> option_bind)) (fun encs ->
                 let ts =
                   map (fun pat0 ->
                     let (y, _) = pat0 in let (t0, _) = y in t0) encs
                 in
                 let _UU03a6_s =
                   map (fun pat0 -> let (_, _UU03a6_) = pat0 in _UU03a6_) encs
                 in
                 Some (((constructor_adt ctor None ts), (_UU03c3__adt adt)),
                 (union_list (gset_empty term_eq_decision term_countable)
                   (gset_union term_eq_decision term_countable) _UU03a6_s)))
                 (Coq0_list.mapM (Obj.magic (fun _ _ -> option_bind))
                   (Obj.magic (fun _ -> option_ret)) (uncurry to_type)
                   (zip_with (fun x0 x1 -> (x0, x1)) _UU03c4_s encs')))
               (Obj.magic encode_sexp_list t es (fun t0 e _ ->
                 encode_sexp0 t0 e __)))
             (guard_or () (Obj.magic (fun _ -> option_mfail))
               (Obj.magic (fun _ -> option_ret))
               (decide_rel Coq_Nat.eq_dec (length es) (length _UU03c4_s))))
           (guard_or () (Obj.magic (fun _ -> option_mfail))
             (Obj.magic (fun _ -> option_ret))
             (decide_rel String.eq_dec _UU03b4_' adt)))
         (lookup0 (Obj.magic gmap_lookup String.eq_dec String.countable) ctor
           (gmap_empty String.eq_dec String.countable))
     | SEList es ->
       let mk_list = fun ts ->
         fold_right (fun t0 l -> seq_concat (seq_unit t0) l)
           (seq_empty _UU03c3__val) ts
       in
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun encs' ->
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun encs ->
           let ts =
             map (fun pat -> let (y, _) = pat in let (t0, _) = y in t0) encs
           in
           let _UU03a6_s =
             map (fun pat -> let (_, _UU03a6_) = pat in _UU03a6_) encs
           in
           Some (((mk_list ts), _UU03c3__list),
           (union_list (gset_empty term_eq_decision term_countable)
             (gset_union term_eq_decision term_countable) _UU03a6_s)))
           (Coq0_list.mapM (Obj.magic (fun _ _ -> option_bind))
             (Obj.magic (fun _ -> option_ret)) to_val encs'))
         (Obj.magic encode_sexp_list t es (fun t0 e _ ->
           encode_sexp0 t0 e __))
     | SEApp (f, es) ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun fdata ->
         let _UU03c4_s = fdata.lfunc_params_ty in
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun _ ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun encs' ->
             mbind (Obj.magic (fun _ _ -> option_bind)) (fun encs ->
               let ts =
                 map (fun pat -> let (y, _) = pat in let (t0, _) = y in t0)
                   encs
               in
               let _UU03a6_s =
                 map (fun pat -> let (_, _UU03a6_) = pat in _UU03a6_) encs
               in
               let pre = lfunc_pre_ f None ts in
               let call = lfunc_ f None ts in
               Some ((call, (encode_type fdata.lfunc_ret_ty)),
               (union0 (gset_union term_eq_decision term_countable)
                 (union0 (gset_union term_eq_decision term_countable)
                   (singleton0
                     (gset_singleton term_eq_decision term_countable) pre)
                   (singleton0
                     (gset_singleton term_eq_decision term_countable)
                     (repr_type_guard fdata.lfunc_ret_ty call)))
                 (union_list (gset_empty term_eq_decision term_countable)
                   (gset_union term_eq_decision term_countable) _UU03a6_s))))
               (Coq0_list.mapM (Obj.magic (fun _ _ -> option_bind))
                 (Obj.magic (fun _ -> option_ret)) (uncurry to_type)
                 (zip_with (fun x0 x1 -> (x0, x1)) _UU03c4_s encs')))
             (Obj.magic encode_sexp_list t es (fun t0 e _ ->
               encode_sexp0 t0 e __)))
           (guard_or () (Obj.magic (fun _ -> option_mfail))
             (Obj.magic (fun _ -> option_ret))
             (decide_rel Coq_Nat.eq_dec (length es) (length _UU03c4_s))))
         (lookup0 (Obj.magic gmap_lookup String.eq_dec String.countable) f
           (gmap_empty String.eq_dec String.countable))
     | SEOp1 (op, e) ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (encode_op1 op)
         (encode_sexp0 t e __)
     | SEOp2 (e1, op, e2) ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc1 ->
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc2 ->
           encode_op2 op enc1 enc2) (encode_sexp0 t e2 __))
         (encode_sexp0 t e1 __)
     | SEIn (e, t0) ->
       (match encode_sexp0 t e __ with
        | Some enc ->
          (match to_val enc with
           | Some enc' ->
             let (p, _UU03a6_) = enc' in
             let (t1, _) = p in
             if eval_type_wf t0
             then Some
                    (((ands_
                        (elements0
                          (gset_elements term_eq_decision term_countable)
                          _UU03a6_)
                        (is_type t0 t1)),
                    _UU03c3__bool),
                    (empty0 (gset_empty term_eq_decision term_countable)))
             else Some ((false_, _UU03c3__bool),
                    (empty0 (gset_empty term_eq_decision term_countable)))
           | None ->
             Some ((false_, _UU03c3__bool),
               (empty0 (gset_empty term_eq_decision term_countable))))
        | None -> None)
     | SEIsCtor (e, ctor) ->
       (match lookup0 (gmap_lookup String.eq_dec String.countable) ctor
                (gmap_empty String.eq_dec String.countable) with
        | Some y ->
          let (_, _UU03b4_) = y in
          (match encode_sexp0 t e __ with
           | Some enc ->
             (match to_adt _UU03b4_ enc with
              | Some enc' ->
                let (p, _UU03a6_) = enc' in
                let (t0, _) = p in
                Some
                (((ands_
                    (elements0
                      (gset_elements term_eq_decision term_countable)
                      _UU03a6_)
                    (is_constructor_adt ctor t0)),
                _UU03c3__bool),
                (empty0 (gset_empty term_eq_decision term_countable)))
              | None ->
                Some ((false_, _UU03c3__bool),
                  (empty0 (gset_empty term_eq_decision term_countable))))
           | None -> None)
        | None ->
          Some ((false_, _UU03c3__bool),
            (empty0 (gset_empty term_eq_decision term_countable))))
     | SEMatch (adt_t, e, cases) ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun _ ->
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun enc ->
           let (y, _UU03a6_) = enc in
           let (t0, _) = y in
           mbind (Obj.magic (fun _ _ -> option_bind))
             (fun pts_res__UU03a6_s ->
             let (y0, _UU03a6_s) = pts_res__UU03a6_s in
             let (pts, residual_pts) = y0 in
             let pts' = app pts ((PVar, null_val) :: []) in
             let residual_pts' = app residual_pts ((PVar, true_) :: []) in
             let p_ctors =
               list_to_set
                 (Obj.magic gset_singleton identifier_eq_decision
                   identifier_countable)
                 (gset_empty identifier_eq_decision identifier_countable)
                 (gset_union identifier_eq_decision identifier_countable)
                 (omap (Obj.magic (fun _ _ -> Coq0_list.list_omap))
                   pattern_constructor
                   (fmap (Obj.magic (fun _ _ -> Coq0_list.list_fmap)) fst
                     pts'))
             in
             mbind (Obj.magic (fun _ _ -> option_bind))
               (fun _UU03b4__ctors ->
               let missed_ctors =
                 filter0 (fun _ ->
                   set_filter
                     (gset_elements identifier_eq_decision
                       identifier_countable)
                     (gset_empty identifier_eq_decision identifier_countable)
                     (gset_singleton identifier_eq_decision
                       identifier_countable)
                     (gset_union identifier_eq_decision identifier_countable))
                   (fun x0 ->
                   not_dec
                     (decide_rel
                       (gset_elem_of_dec identifier_eq_decision
                         identifier_countable)
                       x0 p_ctors))
                   _UU03b4__ctors
               in
               let testers =
                 set_bind
                   (gset_elements identifier_eq_decision identifier_countable)
                   (gset_empty identifier_eq_decision identifier_countable)
                   (gset_union identifier_eq_decision identifier_countable)
                   lookup_tester_for_constructor_adt missed_ctors
               in
               let not_missed_ctors =
                 if sexp_match_cases_has_patvar cases
                 then empty0 (gset_empty term_eq_decision term_countable)
                 else set_map
                        (gset_elements identifier_eq_decision
                          identifier_countable)
                        (gset_singleton term_eq_decision term_countable)
                        (gset_empty term_eq_decision term_countable)
                        (gset_union term_eq_decision term_countable)
                        (fun p -> not_ (TApp (p, None, (t0 :: [])))) testers
               in
               mbind (Obj.magic (fun _ _ -> option_bind)) (fun _ -> Some
                 (((TMatch (t0, pts')), _UU03c3__val),
                 (union0 (gset_union term_eq_decision term_countable)
                   (union0 (gset_union term_eq_decision term_countable)
                     (union0 (gset_union term_eq_decision term_countable)
                       _UU03a6_ _UU03a6_s)
                     not_missed_ctors)
                   (singleton0
                     (gset_singleton term_eq_decision term_countable) (TMatch
                     (t0, residual_pts'))))))
                 (guard_or () (Obj.magic (fun _ -> option_mfail))
                   (Obj.magic (fun _ -> option_ret))
                   (or_dec
                     (decide_rel bool_eq_dec
                       (sexp_match_cases_has_patvar cases) true)
                     (not_dec
                       (decide_rel
                         (gset_eq_dec identifier_eq_decision
                           identifier_countable)
                         p_ctors
                         (empty0
                           (gset_empty identifier_eq_decision
                             identifier_countable)))))))
               (lookup0
                 (Obj.magic gmap_lookup identifier_eq_decision
                   identifier_countable)
                 (s_adt adt_t) constructors_for_adt_sort_map))
             (Obj.magic encode_sexp_match_cases t adt_t t0 cases
               (fun t1 _ e' _ _ -> encode_sexp0 t1 e' __)))
           (mbind (Obj.magic (fun _ _ -> option_bind)) (to_adt adt_t)
             (encode_sexp0 t e __)))
         (guard_or () (Obj.magic (fun _ -> option_mfail))
           (Obj.magic (fun _ -> option_ret))
           (decide_rel (gset_elem_of_dec String.eq_dec String.countable)
             adt_t adts)))
  in fix_F (a,b)
