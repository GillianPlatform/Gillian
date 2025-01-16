(** @canonical Gillian.Gil_syntax.Location *)
module Location : sig
  (** Representation of a location in a source file *)

  type position = { pos_line : int; pos_column : int } [@@deriving yojson]

  type t = { loc_start : position; loc_end : position; loc_source : string }
  [@@deriving yojson, eq]

  val none : t
  val pp : t Fmt.t
  val pp_log_opt : Format.formatter -> t option -> unit
end

(** @canonical Gillian.Gil_syntax.LVar *)
module LVar : sig
  (** Allocator for logical variable names *)

  (** @inline *)
  include Allocators.S with type t = string
end

(** @canonical Gillian.Gil_syntax.ALoc *)
module ALoc : sig
  (** Allocator for (sybolic) memory locations *)

  (** @inline *)
  include Allocators.S with type t = string
end

(** @canonical Gillian.Gil_syntax.Var *)
module Var : sig
  (** GIL Variables *)

  type t = string [@@deriving yojson, show]

  module Set : module type of Containers.SS

  val str : t -> string
end

(** @canonical Gillian.Gil_syntax.Constant *)
module Constant : sig
  (** GIL Constants *)

  type t = TypeDef__.constant =
    | Min_float  (** The smallest float *)
    | Max_float  (** The largest float *)
    | MaxSafeInteger  (** {m 2^{53} - 1} *)
    | Epsilon  (** Smallest positive number *)
    | Random  (** A random number between 0 and 1 *)
    | Pi  (** The number {m \pi} *)
    | UTCTime  (** Current UTC time *)
    | LocalTime  (** Current local time *)
  [@@deriving yojson]

  (** Printer {m e^i} help*)
  val str : t -> string
end

(** @canonical Gillian.Gil_syntax.Type *)
module Type : sig
  (** GIL Types *)

  type t = TypeDef__.typ =
    | UndefinedType  (** Type of Undefined *)
    | NullType  (** Type of Null *)
    | EmptyType  (** Type of Empty *)
    | NoneType  (** Type of None *)
    | BooleanType  (** Type of booleans *)
    | IntType  (** Type of integers *)
    | NumberType  (** Type of floats *)
    | StringType  (** Type of strings *)
    | ObjectType  (** Type of objects *)
    | ListType  (** Type of lists *)
    | TypeType  (** Type of types *)
    | SetType  (** Type of sets *)
    | BvType of int
  [@@deriving yojson, eq, show]

  (** Printer *)
  val str : t -> string

  (** Sets of types *)
  module Set : Set.S with type elt := t
end

(** @canonical Gillian.Gil_syntax.Literal *)
module Literal : sig
  (** GIL Literals *)

  type t = TypeDef__.literal =
    | Undefined  (** The literal [undefined] *)
    | Null  (** The literal [null] *)
    | Empty  (** The literal [empty] *)
    | Constant of Constant.t  (** GIL constants ({!type:Constant.t}) *)
    | Bool of bool  (** GIL booleans: [true] and [false] *)
    | Int of Z.t  (** GIL integers: TODO: understand size *)
    | Num of float  (** GIL floats - double-precision 64-bit IEEE 754 *)
    | String of string  (** GIL strings *)
    | Loc of string  (** GIL locations (uninterpreted symbols) *)
    | Type of Type.t  (** GIL types ({!type:Type.t}) *)
    | LList of t list  (** Lists of GIL literals *)
    | LBitvector of (Z.t * int)
    | Nono  (** Negative information *)
  [@@deriving yojson, eq]

  (** Pretty-printer *)
  val pp : t Fmt.t

  (** Returns the type of a literal *)
  val type_of : t -> Type.t

  (** Evaluates a constant *)
  val evaluate_constant : Constant.t -> t

  (** Builds a GIL list from an OCaml list *)
  val from_list : t list -> t

  (** Expands a list literal to a list of literals, or [None] if not a list *)
  val to_list : t -> t list option

  (** Returns a list of all non-list literals in a literal *)
  val base_elements : t -> t list
end

(** @canonical Gillian.Gil_syntax.UnOp *)
module UnOp : sig
  (** GIL Unary Operators *)

  type t = TypeDef__.unop =
    | IUnaryMinus  (** Integer unary minus *)
    | FUnaryMinus  (** Float unary minus *)
    | Not  (** Negation *)
    | BitwiseNot  (** Bitwise negation *)
    | M_isNaN  (** Test for NaN *)
    | M_abs  (** Absolute value *)
    | M_acos  (** Arccosine *)
    | M_asin  (** Arcsine *)
    | M_atan  (** Arctangent *)
    | M_ceil  (** Ceiling *)
    | M_cos  (** Cosine *)
    | M_exp  (** Exponentiation *)
    | M_floor  (** Flooring *)
    | M_log  (** Natural logarithm *)
    | M_round  (** Rounding *)
    | M_sgn  (** Sign *)
    | M_sin  (** Sine *)
    | M_sqrt  (** Square root *)
    | M_tan  (** Tangent *)
    | ToStringOp  (** Converts a number (integer or float) to a string *)
    | ToIntOp  (** Converts a float to an integer, Num -> Num !!  *)
    | ToUint16Op
        (** Converts an integer to a 16-bit unsigned integer, Num -> Num !! *)
    | ToUint32Op
        (** Converts an integer to a 32-bit unsigned integer, Num -> Num !! *)
    | ToInt32Op
        (** Converts an integer to a 32-bit signed integer, Num -> Num !! *)
    (* | IntToNum Converts a Gil Int to a Gil Num
       | NumToInt Converts a Gil Num to a Gil Int *)
    | ToNumberOp  (** Converts a string to a number *)
    | TypeOf
    | Car  (** Head of a list *)
    | Cdr  (** Tail of a list *)
    | LstLen  (** List length *)
    | LstRev  (** List reverse *)
    | SetToList  (** From set to list *)
    | StrLen  (** String length *)
    (* Integer vs Number *)
    | NumToInt  (** Number to Integer - actual cast *)
    | IntToNum  (** Integer to Number - actual cast *)
    | IsInt  (** IsInt e <=> (e : float) /\ (e % 1. == 0) *)
  [@@deriving yojson, eq]

  (** Printer *)
  val str : t -> string
end

(** @canonical Gillian.Gil_syntax.BVOps *)
module BVOps : sig
  type t = TypeDef__.bvop =
    | BVConcat
    | BVExtract
    | BVNot
    | BVAnd
    | BVOr
    | BVNeg
    | BVPlus
    | BVMul
    | BVUDiv
    | BVUrem
    | BVShl
    | BVLShr
    | BVXor
    | BVSrem
    | BVSub
    | BVSignExtend
    | BVZeroExtend
    | BVSdiv
    | BVSmod
    | BVAshr
  [@@deriving yojson, eq]

  (** Printer *)
  val str : t -> string
end

(** @canonical Gillian.Gil_syntax.BVPred *)
module BVPred : sig
  type t = TypeDef__.bvpred =
    | BVUlt
    | BVUleq
    | BVSlt
    | BVSleq
    | BVUMulO
    | BVSMulO
    | BVNegO
    | BVUAddO
    | BVSAddO
  [@@deriving yojson, eq]

  (** Printer *)
  val str : t -> string
end

(** @canonical Gillian.Gil_syntax.BinOp *)
module BinOp : sig
  (** GIL Binary Operators *)

  type t = TypeDef__.binop =
    | Equal  (** Equality *)
    | ILessThan  (** Less for integers *)
    | ILessThanEqual  (** Less or equal for integers *)
    | IPlus  (** Integer addition *)
    | IMinus  (** Integer subtraction *)
    | ITimes  (** Integer multiplication *)
    | IDiv  (** Integer division *)
    | IMod  (** Integer modulus *)
    | FLessThan  (** Less for floats *)
    | FLessThanEqual  (** Less or equal for floats *)
    | FPlus  (** Float addition *)
    | FMinus  (** Float subtraction *)
    | FTimes  (** Float multiplication *)
    | FDiv  (** Float division *)
    | FMod  (** Float modulus *)
    | And  (** Boolean conjunction *)
    | Or  (** Boolean disjunction *)
    | Impl  (** Boolean implication *)
    | BitwiseAnd  (** Bitwise conjunction *)
    | BitwiseOr  (** Bitwise disjunction *)
    | BitwiseXor  (** Bitwise exclusive disjunction *)
    | LeftShift  (** Left bitshift *)
    | SignedRightShift  (** Signed right bitshift *)
    | UnsignedRightShift  (** Unsigned right bitshift *)
    | BitwiseAndL  (** Bitwise conjunction 64bit *)
    | BitwiseOrL  (** Bitwise disjunction 64bit *)
    | BitwiseXorL  (** Bitwise exclusive disjunction 64bit *)
    | LeftShiftL  (** Left bitshift 64bit *)
    | SignedRightShiftL  (** Signed right bitshift 64bit *)
    | UnsignedRightShiftL  (** Right bitshift 64bit *)
    | BitwiseAndF  (** Bitwise conjunction float *)
    | BitwiseOrF  (** Bitwise disjunction float *)
    | BitwiseXorF  (** Bitwise exclusive disjunction float *)
    | LeftShiftF  (** Left bitshift float *)
    | SignedRightShiftF  (** Signed right bitshift float *)
    | UnsignedRightShiftF  (** Unsigned right bitshift float *)
    | M_atan2  (** Arctangent y/x *)
    | M_pow  (** Power *)
    | LstNth  (** Nth element of a string *)
    | LstRepeat
    (* [[a; b]] is the list that contains [b] times the element [a] *)
    | StrCat  (** String concatenation *)
    | StrNth  (** Nth element of a string *)
    | StrLess  (** Less or equal for strings *)
    | SetDiff  (** Set difference *)
    | SetMem  (** Set membership *)
    | SetSub  (** Subset *)
  [@@deriving yojson, eq]

  (** Printer *)
  val str : t -> string
end

(** @canonical Gillian.Gil_syntax.NOp *)
module NOp : sig
  (** GIL N-ary Operators *)

  type t = TypeDef__.nop =
    | LstCat  (** List concatenation *)
    | SetUnion  (** Set union *)
    | SetInter  (** Set intersection *)
  [@@deriving yojson]

  (** Printer *)
  val str : t -> string
end

(** @canonical Gillian.Gil_syntax.Expr *)
module Expr : sig
  (** GIL Expressions *)

  type t = TypeDef__.expr =
    | Lit of Literal.t  (** GIL literals *)
    | PVar of string  (** GIL program variables *)
    | LVar of string  (** GIL logical variables (interpreted symbols) *)
    | ALoc of string  (** GIL abstract locations (uninterpreted symbols) *)
    | BVExprIntrinsic of BVOps.t * bv_arg list * int
    | UnOp of UnOp.t * t  (** Unary operators ({!type:UnOp.t}) *)
    | BinOp of t * BinOp.t * t  (** Binary operators ({!type:BinOp.t}) *)
    | LstSub of t * t * t  (** Sublist *)
    | NOp of NOp.t * t list  (** n-ary operators ({!type:NOp.t}) *)
    | EList of t list  (** Lists of expressions *)
    | ESet of t list  (** Sets of expressions *)
    | Exists of (string * Type.t option) list * t
        (** Existential quantification. *)
    | ForAll of (string * Type.t option) list * t

  and bv_arg = TypeDef__.bv_arg = Literal of int | BvExpr of (t * int)
  [@@deriving yojson]

  (** {2: Helpers for building expressions}
    Operations will be optimised away if possible, e.g. [type_ (EList x)] will give [Lit (Type ListType)] directly instead of using {!UnOp.TypeOf} *)

  val partition_bvargs : bv_arg list -> (t * int) list * int list
  val exprs_from_bvargs : bv_arg list -> t list
  val map_bv_arg_exprs : (t -> t) -> bv_arg list -> bv_arg list
  val bv_extract_between_sz : int -> int -> t -> t
  val bv_extract : int -> int -> t -> t
  val lit : Literal.t -> t
  val num : float -> t
  val num_int : int -> t
  val int : int -> t
  val int_z : Z.t -> t
  val string : string -> t
  val bool : bool -> t
  val false_ : t
  val true_ : t
  val to_literal : t -> Literal.t option

  (** Lit (Int Z.zero) *)
  val zero_i : t

  (** Lit (Int Z.one) *)
  val one_i : t

  val bv_z : Z.t -> int -> t
  val zero_bv : int -> t
  val int_to_num : t -> t
  val num_to_int : t -> t
  val type_ : Type.t -> t
  val list : t list -> t
  val list_length : t -> t
  val list_nth : t -> int -> t
  val list_nth_e : t -> t -> t
  val list_sub : lst:t -> start:t -> size:t -> t
  val list_repeat : t -> t -> t
  val list_cons : t -> t -> t
  val list_cat : t -> t -> t
  val typeof : t -> t
  val fmod : t -> t -> t
  val imod : t -> t -> t
  val type_eq : t -> Type.t -> t
  val is_concrete_zero_i : t -> bool
  val bv_concat : t list -> t

  module Infix : sig
    (** Floating point math *)

    val ( +. ) : t -> t -> t
    val ( -. ) : t -> t -> t
    val ( *. ) : t -> t -> t
    val ( /. ) : t -> t -> t

    (** Integer math *)

    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val ( << ) : t -> t -> t
    val ( ~- ) : t -> t

    (** {2: } *)

    (** Comparison  *)

    val ( < ) : t -> t -> t
    val ( > ) : t -> t -> t
    val ( <= ) : t -> t -> t
    val ( >= ) : t -> t -> t
    val ( <. ) : t -> t -> t
    val ( >. ) : t -> t -> t
    val ( <=. ) : t -> t -> t
    val ( >=. ) : t -> t -> t

    (** Booleans *)

    val not : t -> t
    val forall : (string * Type.t option) list -> t -> t
    val ( == ) : t -> t -> t
    val ( && ) : t -> t -> t
    val ( || ) : t -> t -> t
    val ( ==> ) : t -> t -> t

    (** List concatenation *)
    val ( @+ ) : t -> t -> t
  end

  val conjunct : t list -> t
  val disjunct : t list -> t

  (** Sets of expressions *)
  module Set : Set.S with type elt := t

  (** Maps with expressions as keys *)
  module Map : Map.S with type key := t

  (** Equality (derived) *)
  val equal : t -> t -> bool

  (** Comparison (derived) *)
  val compare : t -> t -> int

  (** Optional mapper *)
  val map_opt : (t -> t option * bool) -> (t -> t) option -> t -> t option

  (** Pretty-printer *)
  val pp : t Fmt.t

  (** Pretty-printer with constructors (will not parse) *)
  val full_pp : t Fmt.t

  (** If the expression is a list (either an [EList _] of Lit (LList _)), returns the list of expressions. *)
  val to_list : t -> t list option

  (** [from_list] [EList] with the provided elements *)
  val from_list : t list -> t

  (** [lvars e] returns all logical variables in [e] *)
  val lvars : t -> SS.t

  (** [pvars e] returns all program variables in [e] *)
  val pvars : t -> SS.t

  (** [alocs e] returns all abstract locations in [e] *)
  val alocs : t -> SS.t

  (** [clocs e] returns all concrete locations in [e] *)
  val clocs : t -> SS.t

  (** [locs e] returns all concrete and abstract locations in [e] *)
  val locs : t -> SS.t

  (** [vars e] returns all variables in [e] (includes lvars, pvars, alocs and clocs) *)
  val vars : t -> SS.t

  (** [push_in_negations e] pushes all negations in e "downwards", recursively *)
  val push_in_negations : t -> t

  (** [negate e] negates the expression, recursively *)
  val negate : t -> t

  (** Returns if this expression is a boolean expression, recursively. *)
  val is_boolean_expr : t -> bool

  (** [substitutables e] returns all lvars and alocs *)
  val substitutables : t -> SS.t

  (** [is_concrete e] returns [true] iff the expression contains no lvar or aloc *)
  val is_concrete : t -> bool

  (** [all_literals lst] returns [true] iff all elements of the given list [lst] are literals *)
  val all_literals : t list -> bool

  (** [from_lit_list lst] lifts a literal list to an expression list *)
  val from_lit_list : Literal.t -> t

  (** [lists e] all sub-expressions of [e] of the form [Lit (LList lst)] and [EList lst] *)
  val lists : t -> t list

  (** [subst_clocs subst e] substitutes expressions of the form [Lit (Loc l)] with [subst l] in [e] *)
  val subst_clocs : (string -> t) -> t -> t

  (** [from_var_name var] returns either an aloc, an lvar or a pvar if [var] name matches one of these types
    (see {!Utils.Names.is_aloc_name}, {!Utils.Names.is_lvar_name} and {!Utils.Names.is_pvar_name}) *)
  val from_var_name : string -> t

  (** [loc_from_loc_name loc] Has the same behaviour as [from_var_name] except that it returns either an [ALoc loc] or a [Lit (Loc loc)] *)
  val loc_from_loc_name : string -> t

  (** [subst_expr_for_expr ~to_subst ~subst_with expr] substitutes every occurence of the expression [to_subst] with the expression [subst_with] in [expr] *)
  val subst_expr_for_expr : to_subst:t -> subst_with:t -> t -> t

  (** [base_elements e] returns the list containing all logical variables,
      abstract locations, and non-list literals in [e] *)
  val base_elements : t -> t list

  (** [var_to_expr x] returns the expression representing the program/logical variable or abstract location [x] *)
  val var_to_expr : string -> t

  (** [is_matchable x] returns whether or not the expression [e] is matchable *)
  val is_matchable : t -> bool
end

(** @canonical Gillian.Gil_syntax.Formula *)
module Formula : sig
  (** GIL Formulae *)

  type t = TypeDef__.formula =
    | True  (** Logical true *)
    | False  (** Logical false *)
    | Not of t  (** Logical negation *)
    | And of t * t  (** Logical conjunction *)
    | Or of t * t  (** Logical disjunction *)
    | Eq of Expr.t * Expr.t  (** Expression equality *)
    | Impl of t * t  (** Logical implication *)
    | FLess of Expr.t * Expr.t  (** Expression less-than for numbers *)
    | FLessEq of Expr.t * Expr.t
        (** Expression less-than-or-equal for numbers *)
    | ILess of Expr.t * Expr.t  (** Expression less-than for integers *)
    | ILessEq of Expr.t * Expr.t
        (** Expression less-than-or-equal for integeres *)
    | BVFormIntrinsic of BVPred.t * Expr.bv_arg list
    | StrLess of Expr.t * Expr.t  (** Expression less-than for strings *)
    | SetMem of Expr.t * Expr.t  (** Set membership *)
    | SetSub of Expr.t * Expr.t  (** Set subsetness *)
    | ForAll of (string * Type.t option) list * t  (** Forall *)
    | IsInt of Expr.t  (** IsInt e <=> (e : float) /\ (e % 1. == 0) *)
  [@@deriving yojson, eq]

  val of_bool : bool -> t

  (** Sets of formulae *)
  module Set : Set.S with type elt := t

  (** @deprecated Use {!Visitors.endo} instead *)
  val map :
    (t -> t * bool) option ->
    (t -> t) option ->
    (Expr.t -> Expr.t) option ->
    t ->
    t

  val map_opt :
    (t -> t option * bool) option ->
    (t -> t) option ->
    (Expr.t -> Expr.t option) option ->
    t ->
    t option

  (** Get all the logical variables*)
  val lvars : t -> SS.t

  (** Get all the program variables *)
  val pvars : t -> SS.t

  (** Get all the abstract locations *)
  val alocs : t -> SS.t

  (** Get all the concrete locations *)
  val clocs : t -> SS.t

  (** Get all locations *)
  val locs : t -> SS.t

  (** Get print info *)
  val get_print_info : t -> SS.t * SS.t * SS.t

  (** Get all the logical expressions of the formula of the form (Lit (LList lst)) and (EList lst) *)
  val lists : t -> Expr.t list

  (** Get all the list expressions *)
  val list_lexprs : t -> Expr.Set.t

  (** [push_in_negations a] takes negations off the toplevel of [a] and pushes them in the leaves.
    For example [push_in_negations (Not (And (True, False)))] returns [Or (False, False)] *)
  val push_in_negations : t -> t

  (** Turns [f1 /\ f2 /\ f3] into [\[f1; f2; f3\]] *)
  val split_conjunct_formulae : t -> t list

  (** Pretty-printer *)
  val pp : Format.formatter -> t -> unit

  (** Pretty-printer with constructors (will not parse) *)
  val full_pp : Format.formatter -> t -> unit

  (** Lifts an expression to a formula, if possible. It returns
      the lifted expression and its negation *)
  val lift_logic_expr : Expr.t -> (t * t) option

  (** Unlifts the formula to an expression, if possible *)
  val to_expr : t -> Expr.t option

  (** [conjunct \[a1; ...; an\]] returns [a1 /\ ... /\ an] *)
  val conjunct : t list -> t

  (** [disjunct \[a1; ...; an\]] returns [a1 \/ ... \/ an] *)
  val disjunct : t list -> t

  val subst_expr_for_expr : to_subst:Expr.t -> subst_with:Expr.t -> t -> t

  (** [subst_clocs subst e] Substitutes expressions of the form [Lit (Loc l)] with [subst l] in [e] *)
  val subst_clocs : (string -> Expr.t) -> t -> t

  (** [get_disjuncts (a1 \/ ... \/ an)] returns [\[a1; ...; an\]] *)
  val get_disjuncts : t -> t list

  (** Returns a list of strings and a list of numbers that are contained in the formula *)
  val strings_and_numbers : t -> string list * float list

  module Infix : sig
    (** Same as Not *)
    val fnot : t -> t

    (** Same as Forall *)
    val forall : (string * Type.t option) list -> t -> t

    (** Same as Or *)
    val ( #|| ) : t -> t -> t

    (** Same as And *)
    val ( #&& ) : t -> t -> t

    (** Same as Eq *)
    val ( #== ) : Expr.t -> Expr.t -> t

    (** Same as ILess *)
    val ( #< ) : Expr.t -> Expr.t -> t

    (** [a #> b] if [Not ILess (b, a)]*)
    val ( #> ) : Expr.t -> Expr.t -> t

    (** Same as ILessEq *)
    val ( #<= ) : Expr.t -> Expr.t -> t

    (** [a #>= b] is [Not ILess (b, a)] *)
    val ( #>= ) : Expr.t -> Expr.t -> t

    (** Same as FLess *)
    val ( #<. ) : Expr.t -> Expr.t -> t

    (** [a #>. b] if [Not FLess (b, a)]*)
    val ( #>. ) : Expr.t -> Expr.t -> t

    (** Same as FLessEq *)
    val ( #<=. ) : Expr.t -> Expr.t -> t

    (** [a #>=. b] is [Not FLess (b, a)] *)
    val ( #>=. ) : Expr.t -> Expr.t -> t

    val ( #=> ) : t -> t -> t
  end
end

(** @canonical Gillian.Gil_syntax.Asrt *)
module Asrt : sig
  (** GIL Assertions *)

  type atom = TypeDef__.assertion =
    | Emp  (** Empty heap *)
    | Pred of string * Expr.t list  (** Predicates *)
    | Pure of Expr.t  (** Pure formula *)
    | Types of (Expr.t * Type.t) list  (** Typing assertion *)
    | CorePred of string * Expr.t list * Expr.t list  (** Core assertion *)
    | Wand of { lhs : string * Expr.t list; rhs : string * Expr.t list }
        (** Magic wand of the form [P(...) -* Q(...)] *)
  [@@deriving yojson, eq]

  type t = atom list [@@deriving yojson, eq]

  (** Comparison of assertions *)
  val compare : atom -> atom -> int

  (** Sorting of assertions *)
  val prioritise : atom -> atom -> int

  (** Sets of assertions *)
  module Set : Set.S with type elt := t

  (** @deprecated Use {!Visitors.endo} instead *)
  val map : (Expr.t -> Expr.t) -> t -> t

  (** Get all the logical variables in [a] *)
  val lvars : t -> SS.t

  (** Get all the program variables in [a] *)
  val pvars : t -> SS.t

  (** Get all the abstract locations in [a] *)
  val alocs : t -> SS.t

  (** Get all the concrete locations in [a] *)
  val clocs : t -> SS.t

  (** Get all locations in [a] *)
  val locs : t -> SS.t

  (** Returns a list with the names of the predicates that occur in [a] *)
  val pred_names : t -> string list

  (** Returns a list with the pure assertions that occur in [a] *)
  val pure_asrts : t -> Expr.t list

  (** Check if [a] is a pure assertion *)
  val is_pure_asrt : atom -> bool

  (** Eliminate Emp assertions.
      Pure assertions are converted to a single formula.
      This function expects its argument to be a PURE assertion. *)
  val make_pure : t -> Expr.t

  (** Pretty-printer *)
  val pp : Format.formatter -> t -> unit

  val pp_atom : Format.formatter -> atom -> unit

  (** Full pretty-printer *)
  val full_pp : Format.formatter -> t -> unit

  val pp_atom_full : Format.formatter -> atom -> unit

  (** [subst_clocs subst a] Substitutes expressions of the form [Lit (Loc l)] with [subst l] in [a] *)
  val subst_clocs : (string -> Expr.t) -> t -> t

  (** [subst_expr_for_expr ~to_subst ~subst_with a] substitutes every occurence of the expression [to_subst] with the expression [subst_with] in [a] *)
  val subst_expr_for_expr : to_subst:Expr.t -> subst_with:Expr.t -> t -> t

  (** Move pvars to lvars *)
  val pvars_to_lvars : t -> t
end

(** @canonical Gillian.Gil_syntax.SLCmd *)
module SLCmd : sig
  (** GIL Separation-Logic Commands *)

  type t = TypeDef__.slcmd =
    | Fold of string * Expr.t list * (string * (string * Expr.t) list) option
        (** Fold predicate *)
    | Unfold of string * Expr.t list * (string * string) list option * bool
        (** Unfold predicate *)
    | Package of { lhs : string * Expr.t list; rhs : string * Expr.t list }
        (** Magic wand packaging *)
    | GUnfold of string  (** Global Unfold *)
    | ApplyLem of string * Expr.t list * string list  (** Apply lemma *)
    | SepAssert of Asrt.t * string list  (** Assert *)
    | Invariant of Asrt.t * string list  (** Invariant *)
    | Consume of Asrt.t * string list
    | Produce of Asrt.t
    | SymbExec

  (** @deprecated Use {!Visitors.endo} instead *)
  val map : (Asrt.t -> Asrt.t) -> (Expr.t -> Expr.t) -> t -> t

  (** Pretty-printer of folding info *)
  val pp_folding_info : (string * (string * Expr.t) list) option Fmt.t

  val pp_unfold_info : (string * string) list option Fmt.t

  (** Pretty-printer *)
  val pp : Format.formatter -> t -> unit
end

(** @canonical Gillian.Gil_syntax.LCmd *)
module LCmd : sig
  (** GIL Logical Commands *)

  type t = TypeDef__.lcmd =
    | If of Expr.t * t list * t list  (** If-then-else *)
    | Branch of Expr.t  (** Branching on a FO formual *)
    | Macro of string * Expr.t list  (** Macros *)
    | Assert of Expr.t  (** Assert *)
    | Assume of Expr.t  (** Assume *)
    | AssumeType of Expr.t * Type.t  (** Assume Type *)
    | FreshSVar of string  (** x := fresh_svar() *)
    | SL of SLCmd.t  (** Separation-logic command *)

  (** @deprecated Use {!Visitors.endo} instead *)
  val map : (Expr.t -> Expr.t) -> (SLCmd.t -> SLCmd.t) -> t -> t

  (** Pretty-printer *)
  val pp : t Fmt.t
end

(** @canonical Gillian.Gil_syntax.Cmd *)
module Cmd : sig
  (** GIL Commands *)

  (** Optional bindings for procedure calls *)
  type logic_bindings_t = string * (string * Expr.t) list

  type 'label t = 'label TypeDef__.cmd =
    | Skip  (** Skip *)
    | Assignment of string * Expr.t  (** Variable Assignment *)
    | LAction of string * string * Expr.t list  (** Action *)
    | Logic of LCmd.t  (** Logic commands *)
    | Goto of 'label  (** Unconditional goto *)
    | GuardedGoto of Expr.t * 'label * 'label  (** Conditional goto *)
    | Call of
        string * Expr.t * Expr.t list * 'label option * logic_bindings_t option
        (** Procedure call *)
    | ECall of string * Expr.t * Expr.t list * 'label option
        (** External Procedure call *)
    | Apply of string * Expr.t * 'label option
        (** Application-style procedure call *)
    | Arguments of string  (** Arguments of the currently executing function *)
    | PhiAssignment of (string * Expr.t list) list  (** PHI-assignment *)
    | ReturnNormal  (** Normal return *)
    | ReturnError  (** Error return *)
    | Fail of string * Expr.t list  (** Failure *)
  [@@deriving yojson, eq]

  (** Pretty-printer *)
  val pp : pp_label:'a Fmt.t -> Format.formatter -> 'a t -> unit

  (** Pretty-printer for labelled programs *)
  val pp_labeled : Format.formatter -> string t -> unit

  (** Pretty-printer for integer-indexed programs *)
  val pp_indexed : Format.formatter -> int t -> unit

  (** Possible successors of an command (in integer indexing) *)
  val successors : int t -> int -> int list

  (** Program variable collector *)
  val pvars : 'a t -> Containers.SS.t

  (** Logical variable collector *)
  val lvars : 'a t -> Containers.SS.t

  (** Location collector *)
  val locs : 'a t -> Containers.SS.t
end

(** @canonical Gillian.Gil_syntax.Pred *)
module Pred : sig
  (** GIL Predicates *)

  type t = TypeDef__.pred = {
    pred_name : string;  (** Name of the predicate *)
    pred_source_path : string option;
    pred_internal : bool;
    pred_num_params : int;  (** Number of parameters *)
    pred_params : (string * Type.t option) list;
        (** Parameter names and (optional) types *)
    pred_ins : int list;  (** Ins *)
    pred_definitions : ((string * string list) option * Asrt.t) list;
        (** Predicate definitions *)
    pred_facts : Expr.t list;  (** Facts that hold for every definition *)
    pred_guard : Asrt.t option;  (** Cost for unfolding the predicate *)
    pred_pure : bool;  (** Is the predicate pure? *)
    pred_abstract : bool;  (**  Is the predicate abstract? *)
    pred_nounfold : bool;  (** Should the predicate be unfolded? *)
    pred_normalised : bool;  (** Has the predicate been previously normalised? *)
  }

  (** Populates a Hashtbl from the given predicate list *)
  val init : t list -> (string, t) Hashtbl.t

  (** Returns the sets of in- and out-parameters of a predicate *)
  val ins_and_outs : t -> Utils.Containers.SI.t * Utils.Containers.SI.t

  (** Returns the names of in-parameters *)
  val in_params : t -> string list

  (** Returns the in-parameters given all parameters *)
  val in_args : t -> 'a list -> 'a list

  (** Returns the names of in-parameters *)
  val out_params : t -> string list

  (** Returns the out-parameters given all parameters *)
  val out_args : t -> 'a list -> 'a list

  (** Pretty-printer *)
  val pp : Format.formatter -> t -> unit

  (** Sanity check on program variables inside normalised predicates *)
  val check_pvars : (string, t) Hashtbl.t -> unit

  (** Infers parameter types and makes them explicit in the assertions *)
  val explicit_param_types : (string, t) Hashtbl.t -> t -> t

  (** Combines a list of ins and a list of outs putting them in the right order
    according to a given predicate. *)
  val combine_ins_outs : t -> 'a list -> 'a list -> 'a list

  (** [iter_ins_outs p f_ins f_outs (ins, outs)] will iterate, applying [f_ins] on the [ins] and
    [f_outs] on the [outs], in the order specified *)
  val iter_ins_outs :
    t -> ('a -> unit) -> ('b -> unit) -> 'a list * 'b list -> unit

  (** Prints the ins and outs in the right order *)
  val pp_ins_outs :
    t ->
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    Format.formatter ->
    'a list * 'b list ->
    unit

  (** Retrieves a predicate definition by name *)
  val get : (string, t) Hashtbl.t -> string -> t

  (** Given a guarded predicate, return the name of its close token.
      Fails if the predicate isn't guarded. *)
  val close_token_name : t -> string

  (** Given a guarded predicate, return a "call" to its close token.
      The arguments given are PVars with the same name as the ins of the predicate. *)
  val close_token_call : t -> Asrt.atom

  (** Given a name, if it's a close_token name, returns the name of the corresponding predicate,
   otherwise return None. *)
  val pred_name_from_close_token_name : string -> string option
end

(** @canonical Gillian.Gil_syntax.Lemma *)
module Lemma : sig
  (** GIL Lemmas *)

  type spec = TypeDef__.lemma_spec = {
    lemma_hyp : Asrt.t;  (** Hypothesis *)
    lemma_concs : Asrt.t list;  (** Conclusion *)
    lemma_spec_variant : Expr.t option;  (** Variant *)
  }

  type t = TypeDef__.lemma = {
    lemma_name : string;  (** Name *)
    lemma_source_path : string option;
    lemma_internal : bool;
    lemma_params : string list;  (** Parameters *)
    lemma_specs : spec list;  (** Specs of the Lemma *)
    lemma_proof : LCmd.t list option;  (** (Optional) Proof *)
    lemma_variant : Expr.t option;  (** Variant *)
    lemma_existentials : string list; (* Existentials *)
  }

  (** Pretty-printer *)
  val pp : Format.formatter -> t -> unit

  (** Infers types of parameters and adds them to the contained assertions *)
  val parameter_types : (string, Pred.t) Hashtbl.t -> t -> t

  (** Adds bindings from parameters to logical variables *)
  val add_param_bindings : t -> t
end

(** @canonical Gillian.Gil_syntax.Macro *)
module Macro : sig
  (** GIL Macros *)

  type t = {
    macro_name : string;  (** Name of the macro *)
    macro_params : string list;  (** Actual parameters *)
    macro_definition : LCmd.t list;  (** Macro definition *)
  }

  (** Pretty-printer *)
  val pp : Format.formatter -> t -> unit

  (** Table pretty-printer *)
  val pp_tbl : (string, t) Hashtbl.t Fmt.t

  (** Retrieves a macro definition by name *)
  val get : (string, t) Hashtbl.t -> string -> t option
end

(** @canonical Gillian.Gil_syntax.Flag *)
module Flag : sig
  (** Return-flags for GIL specifications *)

  type t = TypeDef__.flag =
    | Normal  (** Normal return *)
    | Error  (** Error return *)
    | Bug  (** Instant crash - for biabduction *)
  [@@deriving yojson, eq]

  val str : t -> string
  val pp : t Fmt.t

  module Set : Set.S with type elt := t
end

(** @canonical Gillian.Gil_syntax.Spec *)
module Spec : sig
  (** GIL specifications *)

  (** Single specification *)
  type st = TypeDef__.single_spec = {
    ss_pre : Asrt.t;  (** Precondition *)
    ss_posts : Asrt.t list;  (** Postcondition *)
    ss_variant : Expr.t option;  (** Variant *)
    ss_flag : Flag.t;  (** Return flag *)
    ss_to_verify : bool;  (** Should the spec be verified? *)
    ss_label : (string * string list) option;
  }

  (** Full specification *)
  type t = TypeDef__.spec = {
    spec_name : string;  (** Procedure/spec name *)
    spec_params : string list;  (** Procedure/spec parameters *)
    spec_sspecs : st list;  (** List of single specifications *)
    spec_normalised : bool;  (** If the spec is already normalised *)
    spec_incomplete : bool;  (**  If the spec is incomplete *)
    spec_to_verify : bool;  (** Should the spec be verified? *)
  }

  (** [s_init ~ss_label ss_pre ss_posts ss_flag ss_to_verify] creates a single specification with the given values *)
  val s_init :
    ?ss_label:string * string list ->
    Asrt.t ->
    Asrt.t list ->
    Expr.t option ->
    Flag.t ->
    bool ->
    st

  (** [init spec_name spec_params spec_sspecs spec_normalised spec_to_verify] creates a full specification with the given values *)
  val init : string -> string list -> st list -> bool -> bool -> bool -> t

  (** Extends a full specfiication with a single specification *)
  val extend : t -> st list -> t

  (** Return the list of parameters of a Spec *)
  val get_params : t -> string list

  val pp_sspec : Format.formatter -> st -> unit
  val pp : Format.formatter -> t -> unit

  (** Makes the types of parameters explicit in the assertions *)
  val parameter_types : (string, Pred.t) Hashtbl.t -> t -> t

  (** @deprecated For legacy purposes, some functions use string sets instead of string list existentials.
    This function allows for a smooth translation *)
  val label_vars_to_set :
    ('a * Utils.Containers.SS.elt list) option ->
    ('a * Utils.Containers.SS.t) option

  (** {3 Serialization} *)

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
  val hash_of_t : t -> string
end

(** @canonical Gillian.Gil_syntax.BiSpec *)
module BiSpec : sig
  (** Bi-abductive specifications *)

  type t = {
    bispec_name : string;  (** Procedure/spec name *)
    bispec_params : string list;  (** Procedure/spec parameters *)
    bispec_pres : Asrt.t list;  (** Possible preconditions *)
    bispec_normalised : bool;  (** If the spec is already normalised *)
  }

  type t_tbl = (string, t) Hashtbl.t

  val init : string -> string list -> Asrt.t list -> bool -> t
  val init_tbl : unit -> t_tbl

  (** Pretty-printer *)
  val pp : Format.formatter -> t -> unit
end

(** @canonical Gillian.Gil_syntax.Branch_case *)
module Branch_case : sig
  (** Reasons for a branch in execution.

    These are used to reason about execution when using the debugger.

    {i Note: most of these haven't yet been properly reasoned about, so they won't be very informative. } *)

  type t =
    | GuardedGoto of bool  (** Effectively if/else; either true or false case *)
    | LCmd of int  (** Logical command *)
    | SpecExec of Flag.t * int  (** Spec execution *)
    | LAction of int  (** Logical action *)
    | LActionFail of int  (** {i Failed} logical action*)
  [@@deriving yojson, show]

  (** A list of branch cases describes the path of execution.

    Every termination of a symbolic execution is uniquely identified by its branch path. *)
  type path = t list [@@deriving yojson, show]

  val pp_short : Format.formatter -> t -> unit
end

(** @canonical Gillian.Gil_syntax.Annot *)
module Annot : sig
  (** Annotations for GIL commands

    This is parametric on the target language. *)

  module type S = sig
    type t [@@deriving yojson]

    val make_basic :
      ?origin_loc:Location.t -> ?loop_info:string list -> unit -> t

    val get_origin_loc : t -> Location.t option
    val get_loop_info : t -> string list
    val set_loop_info : string list -> t -> t
    val is_hidden : t -> bool
  end

  module Basic : sig
    include S

    val equal : t -> t -> bool
    val make : ?origin_loc:Location.t -> ?loop_info:string list -> unit -> t
  end
end

(** @canonical Gillian.Gil_syntax.Proc *)
module Proc : sig
  (** Labeled GIL procedures

    Every command is annotated with a label, and the gotos indicate to which label one should jump.
    Labels can be of any type. However, we say "labeled" when the labels are strings, and "indexed" when the labels are integers.
    Most functions in Gillian that work with indexed procedures assume for efficiency that the label of the i-th command is always Some i
    (starting from 0). *)

  type ('annot, 'label) t = ('annot, 'label) TypeDef__.proc = {
    proc_name : string;
    proc_source_path : string option;
    proc_internal : bool;
    proc_body : ('annot * 'label option * 'label Cmd.t) array;
    proc_params : string list;
    proc_spec : Spec.t option;
    proc_aliases : string list;
    proc_calls : string list;
  }
  [@@deriving yojson]

  (** Gets the parameters of the procedure *)
  val get_params : ('a, 'b) t -> string list

  (** If the [show_labels] flag is true, the labels will be written before the command they correspond to *)
  val pp :
    show_labels:bool ->
    pp_label:'a Fmt.t ->
    ?pp_annot:'b Fmt.t ->
    Format.formatter ->
    ('b, 'a) t ->
    unit

  (** Print labelled *)
  val pp_labeled :
    Format.formatter -> ?pp_annot:'a Fmt.t -> ('a, string) t -> unit

  (** Print indexed *)
  val pp_indexed : Format.formatter -> ?pp_annot:'a Fmt.t -> ('a, int) t -> unit

  (** Returns the indexed procedure for a labeled procedures where the labels can be of any type.
    Equality of labels is decided by structural equality *)
  val indexed_of_labeled : ('annot, string) t -> ('annot, int) t

  val check_proc_spec_correspondence :
    (string, ('annot, 'a) t) Hashtbl.t -> unit
end

(** @canonical Gillian.Gil_syntax.Prog *)
module Prog : sig
  (** A full GIL program *)

  type ('annot, 'label) t = {
    imports : (string * bool) list;
        (** List of imported GIL files, and whether each has to be verified *)
    lemmas : (string, Lemma.t) Hashtbl.t;  (** Lemmas *)
    preds : (string, Pred.t) Hashtbl.t;  (** Predicates *)
    only_specs : (string, Spec.t) Hashtbl.t;
        (** Specs without function definitions *)
    procs : (string, ('annot, 'label) Proc.t) Hashtbl.t;  (** Proceudes *)
    macros : (string, Macro.t) Hashtbl.t;  (** Macros *)
    bi_specs : (string, BiSpec.t) Hashtbl.t;  (** Bi-abductive specs *)
    proc_names : string list;  (** Names of the procedures *)
    predecessors : (string * int * int, int) Hashtbl.t;
        (** Table used for Phi-assignment *)
  }

  (** Makes a full program *)
  val make :
    imports:(string * bool) list ->
    lemmas:(string, Lemma.t) Hashtbl.t ->
    preds:(string, Pred.t) Hashtbl.t ->
    only_specs:(string, Spec.t) Hashtbl.t ->
    procs:(string, ('annot, 'label) Proc.t) Hashtbl.t ->
    macros:(string, Macro.t) Hashtbl.t ->
    bi_specs:(string, BiSpec.t) Hashtbl.t ->
    proc_names:string list ->
    predecessors:(string * int * int, int) Hashtbl.t ->
    unit ->
    ('annot, 'label) t

  (** Initialises a labeled program (with empty predecessors, to be computed later) *)
  val make_labeled :
    procs:(string, ('annot, string) Proc.t) Hashtbl.t ->
    imports:(string * bool) list ->
    lemmas:(string, Lemma.t) Hashtbl.t ->
    preds:(string, Pred.t) Hashtbl.t ->
    only_specs:(string, Spec.t) Hashtbl.t ->
    macros:(string, Macro.t) Hashtbl.t ->
    bi_specs:(string, BiSpec.t) Hashtbl.t ->
    proc_names:string list ->
    unit ->
    ('annot, string) t

  (** Initialises an indexed program (with empty proc_names and imports, useless for the rest) *)
  val make_indexed :
    procs:('annot, int) Proc.t list ->
    predecessors:(string * int * int * int) list ->
    lemmas:(string, Lemma.t) Hashtbl.t ->
    preds:(string, Pred.t) Hashtbl.t ->
    only_specs:(string, Spec.t) Hashtbl.t ->
    macros:(string, Macro.t) Hashtbl.t ->
    bi_specs:(string, BiSpec.t) Hashtbl.t ->
    unit ->
    ('annot, int) t

  (** Creates an empty program *)
  val create : unit -> ('a, string) t

  (** {3 Getters} *)
  val get_lemmas : ('a, 'b) t -> Lemma.t list
  (** Get all lemmas *)

  (** Get all predicates *)
  val get_preds : ('a, 'b) t -> Pred.t list

  (** Get all only-specs *)
  val get_ospecs : ('a, 'b) t -> Spec.t list

  (** Get all specs *)
  val get_specs : ('a, 'b) t -> Spec.t list

  (** Get all procedures *)
  val get_procs : ?proc_names:string list -> ('a, 'b) t -> ('a, 'b) Proc.t list

  (** Get all bi-abductive specs *)
  val get_bispecs : ('a, 'b) t -> BiSpec.t list

  (** Get names of all procedures *)
  val get_proc_names : ('a, 'b) t -> string list

  (** Get names of all procedures not marked as internal *)
  val get_noninternal_proc_names : ('a, 'b) t -> string list

  (** Get names of all predicates not marked as internal *)
  val get_noninternal_pred_names : ('a, 'b) t -> string list

  (** Get names of all lemmas not marked as internal *)
  val get_noninternal_lemma_names : ('a, 'b) t -> string list

  (** Get a specific procedure *)
  val get_proc : ('a, 'b) t -> string -> ('a, 'b) Proc.t option

  (** Get a specific procedure. Raises [Failure] if it does not exist *)
  val get_proc_exn : ('a, 'b) t -> string -> ('a, 'b) Proc.t

  (* FIXME: should raise Not_found instead *)

  (** Get a specific predicate *)
  val get_pred : ('a, 'b) t -> string -> Pred.t option

  (** Get a specific predicate. Raises [Failure] if it does not exist *)
  val get_pred_exn : ('a, 'b) t -> string -> Pred.t

  (** Get a specific bi-abductive spec *)
  val get_bispec : ('a, 'b) t -> string -> BiSpec.t option

  (** Get a specific bi-abductive spec. Raises [Failure] if it does not exist *)
  val get_bispec_exn : ('a, 'b) t -> string -> BiSpec.t

  (** Get a specific lemma *)
  val get_lemma : ('a, 'b) t -> string -> Lemma.t option

  (** Get a specific lemma. Raises [Failure] if it does not exist *)
  val get_lemma_exn : ('a, 'b) t -> string -> Lemma.t

  (** {3 Setters} *)

  (** Add specs *)
  val update_specs : ('a, 'b) t -> ('c, 'd) t -> unit

  (** Add imports *)
  val update_imports : ('a, 'b) t -> (string * bool) list -> ('a, 'b) t

  (** Add a lemma *)
  val add_lemma : ('a, 'b) t -> Lemma.t -> ('a, 'b) t

  (** Add a predicate *)
  val add_pred : ('a, 'b) t -> Pred.t -> ('a, 'b) t

  (** Add an only-spec *)
  val add_ospec : ('a, 'b) t -> Spec.t -> ('a, 'b) t

  (** Add a proc *)
  val add_proc : ('a, 'b) t -> ('a, 'b) Proc.t -> ('a, 'b) t

  (** Add a macro *)
  val add_macro : ('a, 'b) t -> Macro.t -> ('a, 'b) t

  (** Add a bi-abductive spec *)
  val add_bispec : ('a, 'b) t -> BiSpec.t -> ('a, 'b) t

  (** {3 Printers} *)
  val pp :
    show_labels:bool ->
    pp_label:'b Fmt.t ->
    ?pp_annot:'a Fmt.t ->
    Format.formatter ->
    ('a, 'b) t ->
    unit

  (** Print labelled *)
  val pp_labeled :
    Format.formatter -> ?pp_annot:'a Fmt.t -> ('a, string) t -> unit

  (** Print indexed *)
  val pp_indexed : Format.formatter -> ?pp_annot:'a Fmt.t -> ('a, int) t -> unit
end

module Visitors = Visitors
