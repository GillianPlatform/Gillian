
type __ = Obj.t

val negb : bool -> bool

val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

val fst : ('a1 * 'a2) -> 'a1

val snd : ('a1 * 'a2) -> 'a2

val uncurry : ('a1 -> 'a2 -> 'a3) -> ('a1 * 'a2) -> 'a3

val length : 'a1 list -> int

val app : 'a1 list -> 'a1 list -> 'a1 list

type comparison =
| Eq
| Lt
| Gt

val id : __ -> __

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)



val add : int -> int -> int

val compose : ('a2 -> 'a3) -> ('a1 -> 'a2) -> 'a1 -> 'a3

module Nat :
 sig
  val sub : int -> int -> int

  val ltb : int -> int -> bool

  val divmod : int -> int -> int -> int -> int * int

  val div : int -> int -> int

  val modulo : int -> int -> int
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

module Pos :
 sig
  val succ : positive -> positive

  val pred_double : positive -> positive

  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  val succ_double_mask : mask -> mask

  val double_mask : mask -> mask

  val double_pred_mask : positive -> mask

  val sub_mask : positive -> positive -> mask

  val sub_mask_carry : positive -> positive -> mask

  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison

  val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1

  val to_nat : positive -> int

  val of_succ_nat : int -> positive
 end

module Coq_Pos :
 sig
  val succ : positive -> positive

  val add : positive -> positive -> positive

  val add_carry : positive -> positive -> positive

  val pred_double : positive -> positive

  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  val succ_double_mask : mask -> mask

  val double_mask : mask -> mask

  val double_pred_mask : positive -> mask

  val sub_mask : positive -> positive -> mask

  val sub_mask_carry : positive -> positive -> mask

  val sub : positive -> positive -> positive

  val mul : positive -> positive -> positive

  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison

  val pred : positive -> positive

  val size_nat : positive -> int

  val ggcdn : int -> positive -> positive -> positive * (positive * positive)

  val ggcd : positive -> positive -> positive * (positive * positive)

  val eq_dec : positive -> positive -> bool
 end

module N :
 sig
  val succ_double : n -> n

  val double : n -> n

  val sub : n -> n -> n

  val compare : n -> n -> comparison

  val leb : n -> n -> bool

  val pos_div_eucl : positive -> n -> n * n

  val add : n -> n -> n

  val mul : n -> n -> n

  val div_eucl : n -> n -> n * n

  val div : n -> n -> n

  val modulo : n -> n -> n

  val to_nat : n -> int

  val of_nat : int -> n

  val eq_dec : n -> n -> bool
 end

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val nth : int -> 'a1 list -> 'a1 -> 'a1

val firstn : int -> 'a1 list -> 'a1 list

val skipn : int -> 'a1 list -> 'a1 list

val hd_error : 'a1 list -> 'a1 option

val tl : 'a1 list -> 'a1 list

val rev_append : 'a1 list -> 'a1 list -> 'a1 list

val list_eq_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1

module Z :
 sig
  val of_nat : int -> z

  val to_pos : z -> positive

  val sgn : z -> z

  val abs : z -> z

  val ggcd : z -> z -> z * (z * z)
 end

val zero : char

val one : char

val shift : bool -> char -> char

val ascii_of_pos : positive -> char

val ascii_of_N : n -> char

val ascii_of_nat : int -> char

val n_of_digits : bool list -> n

val n_of_ascii : char -> n

val nat_of_ascii : char -> int

val append : char list -> char list -> char list

type decision = bool

val decide : decision -> bool

type ('a, 'b) relDecision = 'a -> 'b -> decision

val decide_rel : ('a1, 'a2) relDecision -> 'a1 -> 'a2 -> decision

val zip_with : ('a1 -> 'a2 -> 'a3) -> 'a1 list -> 'a2 list -> 'a3 list

val prod_map : ('a1 -> 'a2) -> ('a3 -> 'a4) -> ('a1 * 'a3) -> 'a2 * 'a4

type 'a empty = 'a

val empty0 : 'a1 empty -> 'a1

type 'a union = 'a -> 'a -> 'a

val union0 : 'a1 union -> 'a1 -> 'a1 -> 'a1

val union_list : 'a1 empty -> 'a1 union -> 'a1 list -> 'a1

type ('a, 'b) singleton = 'a -> 'b

val singleton0 : ('a1, 'a2) singleton -> 'a1 -> 'a2

val list_to_set :
  ('a1, 'a2) singleton -> 'a2 empty -> 'a2 union -> 'a1 list -> 'a2

type ('a, 'b) filter = __ -> ('a -> decision) -> 'b -> 'b

val filter0 : ('a1, 'a2) filter -> ('a1 -> decision) -> 'a2 -> 'a2

type 'm mRet = __ -> __ -> 'm

val mret : 'a1 mRet -> 'a2 -> 'a1

type 'm mBind = __ -> __ -> (__ -> 'm) -> 'm -> 'm

val mbind : 'a1 mBind -> ('a2 -> 'a1) -> 'a1 -> 'a1

type 'm fMap = __ -> __ -> (__ -> __) -> 'm -> 'm

val fmap : 'a1 fMap -> ('a2 -> 'a3) -> 'a1 -> 'a1

type 'm oMap = __ -> __ -> (__ -> __ option) -> 'm -> 'm

val omap : 'a1 oMap -> ('a2 -> 'a3 option) -> 'a1 -> 'a1

type ('e, 'm) mThrow = __ -> 'e -> 'm

val mthrow : ('a1, 'a2) mThrow -> 'a1 -> 'a2

val guard_or : 'a1 -> ('a1, 'a2) mThrow -> 'a2 mRet -> decision -> 'a2

type ('k, 'a, 'm) lookup = 'k -> 'm -> 'a option

val lookup0 : ('a1, 'a2, 'a3) lookup -> 'a1 -> 'a3 -> 'a2 option

type ('k, 'a, 'm) singletonM = 'k -> 'a -> 'm

val singletonM0 : ('a1, 'a2, 'a3) singletonM -> 'a1 -> 'a2 -> 'a3

type ('k, 'a, 'm) insert = 'k -> 'a -> 'm -> 'm

val insert0 : ('a1, 'a2, 'a3) insert -> 'a1 -> 'a2 -> 'a3 -> 'a3

type ('k, 'a, 'm) partialAlter = ('a option -> 'a option) -> 'k -> 'm -> 'm

val partial_alter :
  ('a1, 'a2, 'a3) partialAlter -> ('a2 option -> 'a2 option) -> 'a1 -> 'a3 ->
  'a3

type 'm merge =
  __ -> __ -> __ -> (__ option -> __ option -> __ option) -> 'm -> 'm -> 'm

val merge0 :
  'a1 merge -> ('a2 option -> 'a3 option -> 'a4 option) -> 'a1 -> 'a1 -> 'a1

type ('a, 'm) unionWith = ('a -> 'a -> 'a option) -> 'm -> 'm -> 'm

val union_with :
  ('a1, 'a2) unionWith -> ('a1 -> 'a1 -> 'a1 option) -> 'a2 -> 'a2 -> 'a2

type ('a, 'c) elements = 'c -> 'a list

val elements0 : ('a1, 'a2) elements -> 'a2 -> 'a1 list

type ('a, 'c) fresh = 'c -> 'a

val fresh0 : ('a1, 'a2) fresh -> 'a2 -> 'a1

type 'a infinite =
  ('a, 'a list) fresh
  (* singleton inductive, whose constructor was Build_Infinite *)

val not_dec : decision -> decision

val or_dec : decision -> decision -> decision

val bool_eq_dec : (bool, bool) relDecision

val unit_eq_dec : (unit, unit) relDecision

val prod_eq_dec :
  ('a1, 'a1) relDecision -> ('a2, 'a2) relDecision -> ('a1 * 'a2, 'a1 * 'a2)
  relDecision

val sum_eq_dec :
  ('a1, 'a1) relDecision -> ('a2, 'a2) relDecision -> (('a1, 'a2) sum, ('a1,
  'a2) sum) relDecision

val bool_decide : decision -> bool

type q = { qnum : z; qden : positive }

val qred : q -> q

type qc = q
  (* singleton inductive, whose constructor was Qcmake *)

val q2Qc : q -> qc

val from_option : ('a1 -> 'a2) -> 'a2 -> 'a1 option -> 'a2

val some_dec : 'a1 option -> 'a1 option

val option_eq_dec :
  ('a1, 'a1) relDecision -> ('a1 option, 'a1 option) relDecision

val option_ret : __ -> __ option

val option_bind : (__ -> __ option) -> __ option -> __ option

val option_fmap : (__ -> __) -> __ option -> __ option

val option_mfail : unit -> __ option

val option_union_with : ('a1, 'a1 option) unionWith

module Coq_Nat :
 sig
  val eq_dec : (int, int) relDecision
 end

module Coq0_Pos :
 sig
  val eq_dec : (positive, positive) relDecision

  val app : positive -> positive -> positive

  val reverse_go : positive -> positive -> positive

  val reverse : positive -> positive

  val dup : positive -> positive
 end

module Coq_N :
 sig
  val eq_dec : (n, n) relDecision

  val lt_dec : (n, n) relDecision
 end

type qp = qc
  (* singleton inductive, whose constructor was mk_Qp *)

module Coq_list :
 sig
  val list_filter : ('a1 -> decision) -> 'a1 list -> 'a1 list

  val reverse : 'a1 list -> 'a1 list

  val list_elem_of_dec : ('a1, 'a1) relDecision -> ('a1, 'a1 list) relDecision

  val list_eq_dec : ('a1, 'a1) relDecision -> ('a1 list, 'a1 list) relDecision
 end

module Coq0_list :
 sig
  val list_fmap : (__ -> __) -> __ list -> __ list

  val list_omap : (__ -> __ option) -> __ list -> __ list

  val list_bind : (__ -> __ list) -> __ list -> __ list

  val mapM : 'a1 mBind -> 'a1 mRet -> ('a2 -> 'a1) -> 'a2 list -> 'a1
 end

module Coq1_list :
 sig
  val list_find : ('a1 -> decision) -> 'a1 list -> (int * 'a1) option

  val positives_flatten_go : positive list -> positive -> positive

  val positives_flatten : positive list -> positive

  val positives_unflatten_go :
    positive -> positive list -> positive -> positive list option

  val positives_unflatten : positive -> positive list option
 end

type 'a countable = { encode : ('a -> positive);
                      decode : (positive -> 'a option) }

val inj_countable :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> ('a2
  -> 'a1) -> ('a1 -> 'a2 option) -> 'a2 countable

val unit_countable : unit countable

val option_countable :
  ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 option countable

val sum_countable :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> 'a2
  countable -> ('a1, 'a2) sum countable

val prod_encode_fst : positive -> positive

val prod_encode_snd : positive -> positive

val prod_encode : positive -> positive -> positive

val prod_decode_fst : positive -> positive option

val prod_decode_snd : positive -> positive option

val prod_countable :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> 'a2
  countable -> ('a1 * 'a2) countable

val list_countable :
  ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 list countable

val n_countable : n countable

val nat_countable : int countable

type 't gen_tree =
| GenLeaf of 't
| GenNode of int * 't gen_tree list

val gen_tree_dec :
  ('a1, 'a1) relDecision -> ('a1 gen_tree, 'a1 gen_tree) relDecision

val gen_tree_to_list : 'a1 gen_tree -> (int * int, 'a1) sum list

val gen_tree_of_list :
  'a1 gen_tree list -> (int * int, 'a1) sum list -> 'a1 gen_tree option

val gen_tree_countable :
  ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 gen_tree countable

val bool_cons_pos : bool -> positive -> positive

val ascii_cons_pos : char -> positive -> positive

val string_to_pos : char list -> positive

val pos_to_string : positive -> char list

module Ascii :
 sig
  val eq_dec : (char, char) relDecision
 end

module String :
 sig
  val eq_dec : (char list, char list) relDecision

  val countable : char list countable
 end

type 'a pretty = 'a -> char list

val pretty0 : 'a1 pretty -> 'a1 -> char list

val pretty_N_char : n -> char

val pretty_N_go_help : n -> char list -> char list

val pretty_N_go : n -> char list -> char list

val pretty_N : n pretty

val pretty_nat : int pretty

val pretty_positive : positive pretty

val pretty_Z : z pretty

val search_infinite_go :
  (int -> 'a1) -> ('a1, 'a1) relDecision -> 'a1 list -> int -> (int -> __ ->
  'a1) -> 'a1

val search_infinite : (int -> 'a1) -> ('a1, 'a1) relDecision -> 'a1 infinite

val string_infinite : char list infinite

val set_filter :
  ('a1, 'a2) elements -> 'a2 empty -> ('a1, 'a2) singleton -> 'a2 union ->
  ('a1 -> decision) -> 'a2 -> 'a2

val set_map :
  ('a1, 'a2) elements -> ('a3, 'a4) singleton -> 'a4 empty -> 'a4 union ->
  ('a1 -> 'a3) -> 'a2 -> 'a4

val set_bind :
  ('a1, 'a2) elements -> 'a3 empty -> 'a3 union -> ('a1 -> 'a3) -> 'a2 -> 'a3

val set_fresh :
  ('a1, 'a2) elements -> ('a1, 'a1 list) fresh -> ('a1, 'a2) fresh

type ('k, 'a, 'm) mapFold = __ -> ('k -> 'a -> __ -> __) -> __ -> 'm -> __

val map_fold :
  ('a1, 'a2, 'a3) mapFold -> ('a1 -> 'a2 -> 'a4 -> 'a4) -> 'a4 -> 'a3 -> 'a4

val map_insert : ('a1, 'a2, 'a3) partialAlter -> ('a1, 'a2, 'a3) insert

val map_singleton :
  ('a1, 'a2, 'a3) partialAlter -> 'a3 empty -> ('a1, 'a2, 'a3) singletonM

val list_to_map :
  ('a1, 'a2, 'a3) insert -> 'a3 empty -> ('a1 * 'a2) list -> 'a3

val map_to_list : ('a1, 'a2, 'a3) mapFold -> 'a3 -> ('a1 * 'a2) list

val map_to_set :
  ('a1, 'a2, 'a3) mapFold -> ('a4, 'a5) singleton -> 'a5 empty -> 'a5 union
  -> ('a1 -> 'a2 -> 'a4) -> 'a3 -> 'a5

val set_to_map :
  ('a1, 'a2) elements -> ('a3, 'a4, 'a5) insert -> 'a5 empty -> ('a1 ->
  'a3 * 'a4) -> 'a2 -> 'a5

val map_union_with : 'a1 merge -> ('a2, 'a1) unionWith

val map_union : 'a1 merge -> 'a1 union

val map_img :
  ('a1, 'a2, 'a3) mapFold -> ('a2, 'a4) singleton -> 'a4 empty -> 'a4 union
  -> 'a3 -> 'a4

type 'munit mapset' =
  'munit
  (* singleton inductive, whose constructor was Mapset *)

val mapset_empty : (__ -> 'a1 empty) -> 'a1 mapset' empty

val mapset_singleton :
  (__ -> 'a2 empty) -> (__ -> ('a1, __, 'a2) partialAlter) -> ('a1, 'a2
  mapset') singleton

val mapset_union : 'a1 merge -> 'a1 mapset' union

val mapset_elements :
  (__ -> ('a1, __, 'a2) mapFold) -> ('a1, 'a2 mapset') elements

val mapset_eq_dec :
  ('a1, 'a1) relDecision -> ('a1 mapset', 'a1 mapset') relDecision

val mapset_elem_of_dec :
  (__ -> ('a1, __, 'a2) lookup) -> ('a1, 'a2 mapset') relDecision

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

val gmap_dep_ne_eq_dec :
  ('a1, 'a1) relDecision -> ('a1 gmap_dep_ne, 'a1 gmap_dep_ne) relDecision

val gmap_dep_eq_dec :
  ('a1, 'a1) relDecision -> ('a1 gmap_dep, 'a1 gmap_dep) relDecision

val gmap_eq_dec :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> (('a1,
  'a2) gmap, ('a1, 'a2) gmap) relDecision

val gNode : 'a1 gmap_dep -> (__ * 'a1) option -> 'a1 gmap_dep -> 'a1 gmap_dep

val gmap_dep_ne_case :
  'a1 gmap_dep_ne -> ('a1 gmap_dep -> (__ * 'a1) option -> 'a1 gmap_dep ->
  'a2) -> 'a2

val gmap_dep_ne_lookup : positive -> 'a1 gmap_dep_ne -> 'a1 option

val gmap_dep_lookup : positive -> 'a1 gmap_dep -> 'a1 option

val gmap_lookup :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2, ('a1, 'a2) gmap)
  lookup

val gmap_empty :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2) gmap empty

val gmap_dep_ne_singleton : positive -> 'a1 -> 'a1 gmap_dep_ne

val gmap_partial_alter_aux :
  (positive -> __ -> 'a1 gmap_dep_ne -> 'a1 gmap_dep) -> ('a1 option -> 'a1
  option) -> positive -> 'a1 gmap_dep -> 'a1 gmap_dep

val gmap_dep_ne_partial_alter :
  ('a1 option -> 'a1 option) -> positive -> 'a1 gmap_dep_ne -> 'a1 gmap_dep

val gmap_dep_partial_alter :
  ('a1 option -> 'a1 option) -> positive -> 'a1 gmap_dep -> 'a1 gmap_dep

val gmap_partial_alter :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2, ('a1, 'a2) gmap)
  partialAlter

val gmap_dep_ne_fmap : ('a1 -> 'a2) -> 'a1 gmap_dep_ne -> 'a2 gmap_dep_ne

val gmap_dep_fmap : ('a1 -> 'a2) -> 'a1 gmap_dep -> 'a2 gmap_dep

val gmap_fmap :
  ('a1, 'a1) relDecision -> 'a1 countable -> (__ -> __) -> ('a1, __) gmap ->
  ('a1, __) gmap

val gmap_dep_omap_aux :
  ('a1 gmap_dep_ne -> 'a2 gmap_dep) -> 'a1 gmap_dep -> 'a2 gmap_dep

val gmap_dep_ne_omap : ('a1 -> 'a2 option) -> 'a1 gmap_dep_ne -> 'a2 gmap_dep

val gmap_merge_aux :
  ('a1 gmap_dep_ne -> 'a2 gmap_dep_ne -> 'a3 gmap_dep) -> ('a1 option -> 'a2
  option -> 'a3 option) -> 'a1 gmap_dep -> 'a2 gmap_dep -> 'a3 gmap_dep

val diag_None' :
  ('a1 option -> 'a2 option -> 'a3 option) -> (__ * 'a1) option -> (__ * 'a2)
  option -> (__ * 'a3) option

val gmap_dep_ne_merge :
  ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 gmap_dep_ne -> 'a2
  gmap_dep_ne -> 'a3 gmap_dep

val gmap_dep_merge :
  ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 gmap_dep -> 'a2 gmap_dep ->
  'a3 gmap_dep

val gmap_merge :
  ('a1, 'a1) relDecision -> 'a1 countable -> (__ option -> __ option -> __
  option) -> ('a1, __) gmap -> ('a1, __) gmap -> ('a1, __) gmap

val gmap_fold_aux :
  (positive -> 'a2 -> 'a1 gmap_dep_ne -> 'a2) -> positive -> 'a2 -> 'a1
  gmap_dep -> 'a2

val gmap_dep_ne_fold :
  (positive -> 'a1 -> 'a2 -> 'a2) -> positive -> 'a2 -> 'a1 gmap_dep_ne -> 'a2

val gmap_dep_fold :
  (positive -> 'a1 -> 'a2 -> 'a2) -> positive -> 'a2 -> 'a1 gmap_dep -> 'a2

val gmap_fold :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a1 -> 'a2 -> __ -> __) -> __
  -> ('a1, 'a2) gmap -> __

type 'k gset = ('k, unit) gmap mapset'

val gset_empty : ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 gset empty

val gset_singleton :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a1 gset) singleton

val gset_union : ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 gset union

val gset_elements :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a1 gset) elements

val gset_eq_dec :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a1 gset, 'a1 gset) relDecision

val gset_elem_of_dec :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a1 gset) relDecision

val fresh_string_go :
  char list -> (char list, 'a1) gmap -> n -> (n -> __ -> char list) ->
  char list

val fresh_string : char list -> (char list, 'a1) gmap -> char list

val fresh_string_of_set : char list -> char list gset -> char list

val fresh_strings_of_set :
  char list -> int -> char list gset -> char list list

val map_snd : ('a2 -> 'a3) -> ('a1 * 'a2) list -> ('a1 * 'a3) list

val gset_to_gmap_with :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> 'a2
  countable -> ('a1 -> 'a2) -> ('a1 -> 'a3) -> 'a1 gset -> ('a2, 'a3) gmap

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

val adts : char list gset

val eval_type_wf : type0 -> bool

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

val lv_lexp : lexp -> char list gset

val lV_lexp : lexp lV

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

val sexp_to_lexp : sexp -> lexp

val lV_sexp : sexp lV

val sexp_open : int -> sexp list -> sexp -> sexp

val sexp_match_cases_has_patvar : (pattern * sexp) list -> bool

val list_eq_dec_elem_of :
  'a1 list -> 'a1 list -> ('a1 -> 'a1 -> __ -> __ -> bool) -> bool

type index =
| IdxNum of int
| IdxSym of char list

val index_eq_decision : (index, index) relDecision

val index_countable : index countable

type identifier =
| IdSimple of char list
| IdIndexed of char list * index list

val identifier_eq_decision : (identifier, identifier) relDecision

val identifier_countable : identifier countable

val identifier_add_prefix : char list -> identifier -> identifier

type sort =
| SParam of identifier
| SApp of identifier * sort list

val sort_eq_decision : (sort, sort) relDecision

val sort_to_gen_tree : sort -> identifier gen_tree

val gen_tree_to_sort : identifier gen_tree -> sort option

val sort_countable : sort countable

val s_bool : identifier

val _UU03c3__bool : sort

type pattern0 =
| PVar
| PApp of identifier * int

val pattern_eq_dec : (pattern0, pattern0) relDecision

val pattern_countable : pattern0 countable

val pattern_constructor : pattern0 -> identifier option

type term =
| TFVar of char list
| TBVar of int * int
| TApp of identifier * sort option * term list
| TLambda of sort * term
| TExists of sort * term
| TForall of sort * term
| TLet of term list * term
| TMatch of term * (pattern0 * term) list

val term_eq_decision : (term, term) relDecision

val term_encode :
  term -> (((char list, int * int) sum, ((int * identifier) * sort option,
  sort) sum) sum, ((sort, sort) sum, (int, pattern0 list) sum) sum) sum list

val term_decode :
  term list -> (((char list, int * int) sum, ((int * identifier) * sort
  option, sort) sum) sum, ((sort, sort) sum, (int, pattern0 list) sum) sum)
  sum list -> term option

val term_countable : term countable

val fv : term -> char list gset

val term_open : int -> term list -> term -> term

val term_close : char list list -> int -> term -> term

val term_subst : (char list, term) gmap -> term -> term

val f_true : identifier

val f_false : identifier

val f_not : identifier

val f_impl : identifier

val f_and : identifier

val f_eq : identifier

val f_ite : identifier

val true_ : term

val false_ : term

val bool_ : bool -> term

val not_ : term -> term

val impl_ : term -> term -> term

val and_ : term -> term -> term

val ands_ : term list -> term -> term

val eq_ : term -> term -> term

val ite_ : term -> term -> term -> term

val s_int : identifier

val _UU03c3__int : sort

val s_real : identifier

val _UU03c3__real : sort

val f_minus : identifier

val f_plus : identifier

val f_times : identifier

val f_idiv : identifier

val f_div : identifier

val f_mod : identifier

val f_leq : identifier

val f_lt : identifier

val f_geq : identifier

val f_to_real : identifier

val f_to_int : identifier

val f_is_int : identifier

val f_int_literal : z -> identifier

val f_decimal_literal : qc -> identifier

val minus_ : term -> term -> term

val plus_ : term -> term -> term

val times_ : term -> term -> term

val idiv_ : term -> term -> term

val div_ : term -> term -> term

val mod_ : term -> term -> term

val leq_ : term -> term -> term

val lt_ : term -> term -> term

val geq_ : term -> term -> term

val to_real : term -> term

val to_int : term -> term

val is_int : term -> term

val int_literal : z -> term

val decimal_literal : qc -> term

val zero_int : term

val zero_real : term

val s_string : identifier

val _UU03c3__string : sort

val quote_char : char

val quote_string : char list

val hex_digit : int -> char

val plain_char : int -> bool

val escape_ascii : char -> char list

val escape_string : char list -> char list

val f_string_literal : char list -> char list

val string_literal : char list -> term

val s_seq : identifier

val _UU03c4__seq : sort -> sort

val f_seq_empty : identifier

val f_seq_unit : identifier

val f_seq_concat : identifier

val f_seq_len : identifier

val f_seq_nth : identifier

val f_seq_contains : identifier

val f_seq_map : identifier

val seq_empty : sort -> term

val seq_unit : term -> term

val seq_concat : term -> term -> term

val seq_len : term -> term

val seq_nth : term -> term -> term

val seq_contains : term -> term -> term

val seq_map : term -> term -> term

val s_null : identifier

val _UU03c3__null : sort

val s_val : identifier

val _UU03c3__val : sort

val s_maybe_val : identifier

val _UU03c3__maybe_val : sort

val s_adt : char list -> identifier

val _UU03c3__adt : char list -> sort

val _UU03c3__list : sort

val encode_type : type0 -> sort

val f_repr_nat : identifier

val f_repr_rat : identifier

val f_repr_val : identifier

val f_repr_adt : char list -> identifier

val repr_nat : term -> term

val repr_rat : term -> term

val repr_val : term -> term

val repr_adt : char list -> term -> term

val f_lfunc : char list -> identifier

val f_lfunc_pre : char list -> identifier

val lfunc_ : char list -> sort option -> term list -> term

val lfunc_pre_ : char list -> sort option -> term list -> term

val c_null : identifier

val null : term

val c_null_val : identifier

val c_gillian_none_val : identifier

val c_gillian_empty_val : identifier

val c_gillian_loc_val : identifier

val c_bool_val : identifier

val c_nat_val : identifier

val c_rat_val : identifier

val c_string_val : identifier

val c_list_val : identifier

val c_adt_val : char list -> identifier

val null_val_of : term -> term

val null_val : term

val gillian_none_val : term

val gillian_empty_val : term

val gillian_loc_val : term -> term

val bool_val : term -> term

val nat_val : term -> term

val rat_val : term -> term

val string_val : term -> term

val list_val : term -> term

val adt_val : char list -> term -> term

val c_some_val : identifier

val c_constructor_adt : char list -> identifier

val constructor_adt : char list -> sort option -> term list -> term

val constructors_for_adt_sort_map : (identifier, identifier gset) gmap

val g_null_val : identifier

val g_gillian_loc_val : identifier

val g_bool_val : identifier

val g_nat_val : identifier

val g_rat_val : identifier

val g_string_val : identifier

val g_list_val : identifier

val g_adt_val : char list -> identifier

val get_null_val : term -> term

val get_bool_val : term -> term

val get_nat_val : term -> term

val get_rat_val : term -> term

val get_string_val : term -> term

val get_list_val : term -> term

val get_adt_val : char list -> term -> term

val g_some_val : identifier

val get_some_val : term -> term

val p_null_val : identifier

val p_gillian_none_val : identifier

val p_gillian_empty_val : identifier

val p_gillian_loc_val : identifier

val p_bool_val : identifier

val p_nat_val : identifier

val p_rat_val : identifier

val p_string_val : identifier

val p_list_val : identifier

val p_adt_val : char list -> identifier

val is_null_val : term -> term

val is_gillian_none_val : term -> term

val is_gillian_empty_val : term -> term

val is_gillian_loc_val : term -> term

val is_bool_val : term -> term

val is_nat_val : term -> term

val is_rat_val : term -> term

val is_string_val : term -> term

val is_list_val : term -> term

val is_adt_val : char list -> term -> term

val p_some_val : identifier

val is_some_val : term -> term

val p_constructor_adt : char list -> identifier

val is_constructor_adt : char list -> term -> term

val tester_for_constructor_adt_map : (identifier, identifier) gmap

val lookup_tester_for_constructor_adt : identifier -> identifier gset

val is_type : type0 -> term -> term

val repr_list_body : (term -> term) -> term -> term

val repr_type_guard : type0 -> term -> term

val to_val_base :
  term -> sort -> term gset -> ((term * sort) * term gset) option

val to_val_curried :
  term -> sort -> term gset -> ((term * sort) * term gset) option

val to_val : ((term * sort) * term gset) -> ((term * sort) * term gset) option

val guarded_val : term gset -> term -> term

val residual_guard : term gset -> term

val list_coercion_residual : term -> char list -> term gset -> term

val to_null :
  ((term * sort) * term gset) -> ((term * sort) * term gset) option

val to_gillian_value :
  (term -> term) ->
  ((term * sort) * term gset ->
  ((term * sort) * term gset) option)

val to_gillian_none :
  ((term * sort) * term gset) -> ((term * sort) * term gset) option

val to_gillian_empty :
  ((term * sort) * term gset) -> ((term * sort) * term gset) option

val to_gillian_loc :
  ((term * sort) * term gset) -> ((term * sort) * term gset) option

val to_bool :
  ((term * sort) * term gset) -> ((term * sort) * term gset) option

val to_nat0 :
  ((term * sort) * term gset) -> ((term * sort) * term gset) option

val to_rat : ((term * sort) * term gset) -> ((term * sort) * term gset) option

val to_string :
  ((term * sort) * term gset) -> ((term * sort) * term gset) option

val to_adt :
  char list -> ((term * sort) * term gset) -> ((term * sort) * term gset)
  option

val to_type_curried_list_binder : term gset -> term -> char list

val to_type_curried :
  type0 -> term -> sort -> term gset -> ((term * sort) * term gset) option

val to_list :
  type0 -> ((term * sort) * term gset) -> ((term * sort) * term gset) option

val to_type :
  type0 -> ((term * sort) * term gset) -> ((term * sort) * term gset) option

val encode_preval : preval -> ((term * sort) * term gset) option

val encode_lvar :
  type0 option -> char list -> ((term * sort) * term gset) option

val encode_op1 :
  op1 -> ((term * sort) * term gset) -> ((term * sort) * term gset) option

val encode_op2 :
  op2 -> ((term * sort) * term gset) -> ((term * sort) * term gset) ->
  ((term * sort) * term gset) option

val encode_sexp_list :
  (char list, type0) gmap -> sexp list -> ((char list, type0) gmap -> sexp ->
  __ -> ((term * sort) * term gset) option) -> ((term * sort) * term gset)
  list option

val encode_sexp_match_cases :
  (char list, type0) gmap -> char list -> term -> (pattern * sexp) list ->
  ((char list, type0) gmap -> sexp -> sexp -> __ -> __ ->
  ((term * sort) * term gset) option) -> (((pattern0 * term)
  list * (pattern0 * term) list) * term gset) option

val encode_sexp :
  (char list, type0) gmap -> sexp -> ((term * sort) * term gset) option
