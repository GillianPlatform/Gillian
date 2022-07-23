module Location : sig
  type position = { pos_line : int; pos_column : int }
  type t = { loc_start : position; loc_end : position; loc_source : string }

  val none : t
  val pp : t Fmt.t
end

module LVar : Allocators.S with type t = string
module ALoc : Allocators.S with type t = string

module Var : sig
  (** GIL Variables *)

  type t = string [@@deriving yojson]

  module Set : module type of Containers.SS

  val str : t -> string
end

module Constant : sig
  (** {b GIL Constants } *)
  type t =
    | Min_float  (** The smallest float *)
    | Max_float  (** The largest float *)
    | MaxSafeInteger  (** 2^53 - 1 **)
    | Epsilon  (** Smallest positive number *)
    | Random  (** A random number between 0 and 1 *)
    | Pi  (** The number pi *)
    | UTCTime  (** Current UTC time *)
    | LocalTime  (** Current local time *)
  [@@deriving yojson]

  (** Printer *)
  val str : t -> string
end

module Type : sig
  (** {b GIL Types } *)
  type t =
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
  [@@deriving yojson, eq]

  (** Printer *)
  val str : t -> string

  (** Sets of types *)
  module Set : Set.S with type elt := t
end

module Literal : sig
  (** {b GIL Literals } *)
  type t =
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
    | Nono  (** Negative information *)
  [@@deriving yojson, eq]

  (** Pretty-printer *)
  val pp : t Fmt.t

  (** [typeof lit] returns the type of the literal [lit] *)
  val type_of : t -> Type.t

  (** [evaluate_constant c] evaluates the constant [c] *)
  val evaluate_constant : Constant.t -> t

  (** [from_list lst] builds a GIL list [LList lst] from the OCaml list [lst] *)
  val from_list : t list -> t

  (** [to_list lit] returns [Some l] if [lit = LList l], and otherwise [None] *)
  val to_list : t -> t list option

  (** [base_elements lit] Returns a list of all non-list literals occurring in [lit] *)
  val base_elements : t -> t list
end

module UnOp : sig
  (** {b GIL Unary Operators } *)
  type t =
    | IUnaryMinus  (** Integer unary minus *)
    | FUnaryMinus  (** Float unary minus *)
    | UNot  (** Negation *)
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
  [@@deriving yojson, eq]

  (** Printer *)
  val str : t -> string
end

module BinOp : sig
  (** {b GIL Binary Operators } *)
  type t =
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
    | SLessThan  (** Less or equal for strings *)
    | BAnd  (** Boolean conjunction *)
    | BOr  (** Boolean disjunction *)
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
    | UnsignedRightShiftL  (** Unsigned right bitshift 64bit *)
    | M_atan2  (** Arctangent y/x *)
    | M_pow  (** Power *)
    | LstNth  (** Nth element of a string *)
    | StrCat  (** String concatenation *)
    | StrNth  (** Nth element of a string *)
    | SetDiff  (** Set difference *)
    | BSetMem  (** Set membership *)
    | BSetSub  (** Subset *)
  [@@deriving yojson, eq]

  (** Printer *)
  val str : t -> string
end

module NOp : sig
  (** {b GIL N-ary Operators } *)
  type t =
    | LstCat  (** List concatenation *)
    | SetUnion  (** Set union *)
    | SetInter  (** Set intersection *)
  [@@deriving yojson]

  (** Printer *)
  val str : t -> string
end

module Expr : sig
  (** {b GIL Expressions } *)
  type t =
    | Lit of Literal.t  (** GIL literals *)
    | PVar of string  (** GIL program variables *)
    | LVar of string  (** GIL logical variables (interpreted symbols) *)
    | ALoc of string  (** GIL abstract locations (uninterpreted symbols) *)
    | UnOp of UnOp.t * t  (** Unary operators ({!type:UnOp.t}) *)
    | BinOp of t * BinOp.t * t  (** Binary operators ({!type:BinOp.t}) *)
    | LstSub of t * t * t  (** Sublist *)
    | NOp of NOp.t * t list  (** n-ary operators ({!type:NOp.t}) *)
    | EList of t list  (** Lists of expressions *)
    | ESet of t list  (** Sets of expressions *)
  [@@deriving yojson]

  val lit : Literal.t -> t
  val num : float -> t
  val num_int : int -> t
  val int : int -> t
  val int_z : Z.t -> t
  val string : string -> t
  val bool : bool -> t

  (** Lit (Int Z.zero) *)
  val zero_i : t

  (** Lit (Int Z.one) *)
  val one_i : t

  val type_ : Type.t -> t
  val list : t list -> t
  val list_length : t -> t
  val list_nth : t -> int -> t
  val list_nth_e : t -> t -> t
  val list_sub : lst:t -> start:t -> size:t -> t
  val list_cons : t -> t -> t
  val list_cat : t -> t -> t
  val typeof : t -> t
  val fmod : t -> t -> t
  val imod : t -> t -> t

  module Infix : sig
    val ( +. ) : t -> t -> t
    val ( -. ) : t -> t -> t
    val ( *. ) : t -> t -> t
    val ( /. ) : t -> t -> t
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val not : t -> t

    (** [a @+ b] is [list_cat a b] *)
    val ( @+ ) : t -> t -> t
  end

  (** Sets of expressions *)
  module Set : Set.S with type elt := t

  (** Maps with expressions as keys *)
  module Map : Map.S with type key := t

  (** Equality *)
  val equal : t -> t -> bool

  (** Mapper *)

  (* val map : (t -> t * bool) -> (t -> t) option -> t -> t *)

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

  (** [is_unifiable x] returns whether or not the expression [e] is unifiable *)
  val is_unifiable : t -> bool

  (** Sub-expression check *)
  val sub_expr : t -> t -> bool
end

module Formula : sig
  (** {b GIL Formulae } *)
  type t =
    | True  (** Logical true *)
    | False  (** Logical false *)
    | Not of t  (** Logical negation *)
    | And of t * t  (** Logical conjunction *)
    | Or of t * t  (** Logical disjunction *)
    | Eq of Expr.t * Expr.t  (** Expression equality *)
    | FLess of Expr.t * Expr.t  (** Expression less-than for numbers *)
    | FLessEq of Expr.t * Expr.t
        (** Expression less-than-or-equal for numbers *)
    | ILess of Expr.t * Expr.t  (** Expression less-than for integers *)
    | ILessEq of Expr.t * Expr.t
        (** Expression less-than-or-equal for integeres *)
    | StrLess of Expr.t * Expr.t  (** Expression less-than for strings *)
    | SetMem of Expr.t * Expr.t  (** Set membership *)
    | SetSub of Expr.t * Expr.t  (** Set subsetness *)
    | ForAll of (string * Type.t option) list * t  (** Forall *)
  [@@deriving yojson, eq]

  val of_bool : bool -> t

  (** Sets of formulae *)
  module Set : Set.S with type elt := t

  (** Deprecated. Use {!Visitors.endo} instead *)
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

    (** [fa #=> fb] is [(fnot fa) #|| fb] *)
    val ( #=> ) : t -> t -> t
  end
end

module Asrt : sig
  (** {b GIL Assertions } *)
  type t =
    | Emp  (** Empty heap *)
    | Star of t * t  (** Separating conjunction *)
    | Pred of string * Expr.t list  (** Predicates *)
    | Pure of Formula.t  (** Pure formula *)
    | Types of (Expr.t * Type.t) list  (** Typing assertion *)
    | GA of string * Expr.t list * Expr.t list  (** Core assertion *)
  [@@deriving yojson]

  (** Comparison of assertions *)
  val compare : t -> t -> int

  (** Sorting of assertions *)
  val prioritise : t -> t -> int

  (** Sets of assertions *)
  module Set : Set.S with type elt := t

  (** Deprecated, use {!Visitors.endo} instead. *)
  val map :
    (t -> t * bool) option ->
    (t -> t) option ->
    (Expr.t -> Expr.t) option ->
    (Formula.t -> Formula.t) option ->
    t ->
    t

  (** Get all the logical expressions of [a] that denote a list
   and are not logical variables *)
  val list_lexprs : t -> Expr.Set.t

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

  (** Get all the variables in [a] (includes lvars, pvars, alocs and clocs) *)
  val vars : t -> SS.t

  (** Returns a list with the names of the predicates that occur in [a] *)
  val pred_names : t -> string list

  (** Returns a list with the pure assertions that occur in [a] *)
  val pure_asrts : t -> Formula.t list

  (** Returns a list with the pure assertions that occur in [a] *)
  val simple_asrts : t -> t list

  (** Check if [a] is a pure assertion *)
  val is_pure_asrt : t -> bool

  (** Check if [a] is a pure assertion & non-recursive assertion.
   It assumes that only pure assertions are universally quantified *)
  val is_pure_non_rec_asrt : t -> bool

  (** Eliminate LStar and LTypes assertions.
   LTypes disappears. LStar is replaced by LAnd.
   This function expects its argument to be a PURE assertion. *)
  val make_pure : t -> Formula.t

  (** Pretty-printer *)
  val pp : Format.formatter -> t -> unit

  (** Full pretty-printer *)
  val full_pp : Format.formatter -> t -> unit

  (** [star \[a1; a2; ...; an\] will return \[a1 * a2 * ... * an\]] *)
  val star : t list -> t

  (** [subst_clocs subst a] Substitutes expressions of the form [Lit (Loc l)] with [subst l] in [a] *)
  val subst_clocs : (string -> Expr.t) -> t -> t

  (** [subst_expr_for_expr ~to_subst ~subst_with a] substitutes every occurence of the expression [to_subst] with the expression [subst_with] in [a] *)
  val subst_expr_for_expr : to_subst:Expr.t -> subst_with:Expr.t -> t -> t

  (** Move pvars to lvars *)
  val pvars_to_lvars : t -> t

  module Infix : sig
    (** Star constructor *)
    val ( ** ) : t -> t -> t
  end
end

module SLCmd : sig
  (** {b GIL Separation-Logic Commands} *)
  type t =
    | Fold of string * Expr.t list * (string * (string * Expr.t) list) option
        (** Fold predicate *)
    | Unfold of string * Expr.t list * (string * string) list option * bool
        (** Unfold predicate *)
    | GUnfold of string  (** Global Unfold *)
    | ApplyLem of string * Expr.t list * string list  (** Apply lemma *)
    | SepAssert of Asrt.t * string list  (** Assert *)
    | Invariant of Asrt.t * string list  (** Invariant *)
    | SymbExec

  (** Deprecated. Use {!Visitors.endo} instead *)
  val map :
    (t -> t) option ->
    (Asrt.t -> Asrt.t) option ->
    (Expr.t -> Expr.t) option ->
    t ->
    t

  (** Pretty-printer of folding info *)
  val pp_folding_info : (string * (string * Expr.t) list) option Fmt.t

  val pp_unfold_info : (string * string) list option Fmt.t

  (** Pretty-printer *)
  val pp : Format.formatter -> t -> unit
end

module LCmd : sig
  (** {b GIL Logical Commands} *)
  type t =
    | If of Expr.t * t list * t list  (** If-then-else *)
    | Branch of Formula.t  (** Branching on a FO formual *)
    | Macro of string * Expr.t list  (** Macros *)
    | Assert of Formula.t  (** Assert *)
    | Assume of Formula.t  (** Assume *)
    | AssumeType of Expr.t * Type.t  (** Assume Type *)
    | FreshSVar of string  (** x := fresh_svar() *)
    | SL of SLCmd.t  (** Separation-logic-related commands ({!type:SLCmd.t}) *)

  (** Deprecated. Use {!Visitors} instead *)
  val map :
    (t -> t) option ->
    (Expr.t -> Expr.t) option ->
    (Formula.t -> Formula.t) option ->
    (SLCmd.t -> SLCmd.t) option ->
    t ->
    t

  (** Pretty-printer *)
  val pp : t Fmt.t
end

module Cmd : sig
  (** {b GIL Commands} *)

  (** Optional bindings for procedure calls *)
  type logic_bindings_t = string * (string * Expr.t) list

  type 'label t =
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

module Pred : sig
  (** {b GIL Predicates} *)
  type t = {
    pred_name : string;  (** Name of the predicate *)
    pred_source_path : string option;
    pred_internal : bool;
    pred_num_params : int;  (** Number of parameters *)
    pred_params : (string * Type.t option) list;
        (** Parameter names and (optional) types *)
    pred_ins : int list;  (** Ins *)
    pred_definitions :
      ((string * string list) option * Asrt.t * string list) list;
        (** Predicate definitions *)
    pred_facts : Formula.t list;  (** Facts that hold for every definition *)
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
end

module Lemma : sig
  (** {b GIL Lemmas} *)

  type spec = {
    lemma_hyp : Asrt.t;  (** Hypothesis *)
    lemma_concs : Asrt.t list;  (** Conclusion *)
    lemma_spec_variant : Expr.t option;  (** Variant *)
    lemma_spec_hides : string list option;  (** Over-approximating logicals *)
  }

  type t = {
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

module Macro : sig
  (** {b GIL Macros } *)
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

module Flag : sig
  (** {b Return flags for GIL specifications}. *)

  type t = Normal  (** Normal return *) | Error  (** Error return *)
  [@@deriving yojson]

  val str : t -> string
  val pp : t Fmt.t

  module Set : Set.S with type elt := t
end

module Spec : sig
  (** {b GIL specifications}. *)
  type st = {
    ss_pre : Asrt.t;  (** Precondition *)
    ss_posts : Asrt.t list;  (** Postcondition *)
    ss_variant : Expr.t option;  (** Variant *)
    ss_flag : Flag.t;  (** Return flag *)
    ss_to_verify : bool;  (** Should the spec be verified? *)
    ss_label : (string * string list) option;
  }
  (** Single GIL specifications. *)

  (** {b Full GIL specifications}. *)
  type t = {
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

  (** For legacy purpose, some functions use string sets instead of string lists existentials.
    This function allows for a smooth translation *)
  val label_vars_to_set :
    ('a * Utils.Containers.SS.elt list) option ->
    ('a * Utils.Containers.SS.t) option

  (** {3 Serialization} *)

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
  val hash_of_t : t -> string
end

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

module Annot : sig
  (** {b GIL annot}. *)
  type t [@@deriving yojson]

  (** make an annotation *)
  val make :
    ?origin_loc:Location.t ->
    ?origin_id:int ->
    ?loop_info:string list ->
    unit ->
    t

  (** Get the loop info *)
  val get_loop_info : t -> string list

  (** Set the loop info *)
  val set_loop_info : t -> string list -> t

  (** Get the origin location *)
  val get_origin_loc : t -> Location.t option

  (* Get the origin id *)
  val get_origin_id : t -> int option
end

module Proc : sig
  (** Labeled procedures. Every command is annotated with a label, and the gotos indicate to which label one should jump.
    Labels can be of any type. However, we say "labeled" when the labels are strings, and "indexed" when the labels are integers.
    Most functions in Gillian that work with indexed procedures assume for efficiency that the label of the i-th command is always Some i
    (starting from 0).
 *)
  type ('annot, 'label) t = {
    proc_name : string;
    proc_source_path : string option;
    proc_internal : bool;
    proc_body : ('annot * 'label option * 'label Cmd.t) array;
    proc_params : string list;
    proc_spec : Spec.t option;
  }

  (** Gets the parameters of the procedure *)
  val get_params : ('a, 'b) t -> string list

  (** If the [show_labels] flag is true, the labels will be written before the command they correspond to *)
  val pp :
    show_labels:bool ->
    pp_label:'a Fmt.t ->
    Format.formatter ->
    ('b, 'a) t ->
    unit

  (** Print labelled *)
  val pp_labeled : Format.formatter -> ('a, string) t -> unit

  (** Print indexed *)
  val pp_indexed : Format.formatter -> ('a, int) t -> unit

  (** Returns the indexed procedure for a labeled procedures where the labels can be of any type.
    Equality of labels is decided by structural equality *)
  val indexed_of_labeled : (Annot.t, string) t -> (Annot.t, int) t

  val check_proc_spec_correspondence :
    (string, (Annot.t, 'a) t) Hashtbl.t -> unit
end

module Prog : sig
  type ('annot, 'label) t = {
    imports : (string * bool) list;
    lemmas : (string, Lemma.t) Hashtbl.t;
    preds : (string, Pred.t) Hashtbl.t;
    only_specs : (string, Spec.t) Hashtbl.t;
    procs : (string, ('annot, 'label) Proc.t) Hashtbl.t;
    macros : (string, Macro.t) Hashtbl.t;
    bi_specs : (string, BiSpec.t) Hashtbl.t;
    proc_names : string list;
    predecessors : (string * int * int, int) Hashtbl.t;
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
    Format.formatter ->
    ('a, 'b) t ->
    unit

  (** Print labelled *)
  val pp_labeled : Format.formatter -> ('a, string) t -> unit

  (** Print indexed *)
  val pp_indexed : Format.formatter -> ('a, int) t -> unit
end

module Visitors : sig
  class ['b] endo :
    object ('b)
      constraint
      'b = < visit_'annot : 'c -> 'd -> 'd
           ; visit_'label : 'c -> 'f -> 'f
           ; visit_ALoc : 'c -> Expr.t -> string -> Expr.t
           ; visit_And : 'c -> Formula.t -> Formula.t -> Formula.t -> Formula.t
           ; visit_Apply :
               'c -> 'f Cmd.t -> string -> Expr.t -> 'f option -> 'f Cmd.t
           ; visit_ApplyLem :
               'c -> SLCmd.t -> string -> Expr.t list -> string list -> SLCmd.t
           ; visit_Arguments : 'c -> 'f Cmd.t -> string -> 'f Cmd.t
           ; visit_Assert : 'c -> LCmd.t -> Formula.t -> LCmd.t
           ; visit_Assignment : 'c -> 'f Cmd.t -> string -> Expr.t -> 'f Cmd.t
           ; visit_Assume : 'c -> LCmd.t -> Formula.t -> LCmd.t
           ; visit_AssumeType : 'c -> LCmd.t -> Expr.t -> Type.t -> LCmd.t
           ; visit_BAnd : 'c -> BinOp.t -> BinOp.t
           ; visit_BOr : 'c -> BinOp.t -> BinOp.t
           ; visit_BSetMem : 'c -> BinOp.t -> BinOp.t
           ; visit_BSetSub : 'c -> BinOp.t -> BinOp.t
           ; visit_BinOp : 'c -> Expr.t -> Expr.t -> BinOp.t -> Expr.t -> Expr.t
           ; visit_BitwiseAnd : 'c -> BinOp.t -> BinOp.t
           ; visit_BitwiseAndL : 'c -> BinOp.t -> BinOp.t
           ; visit_BitwiseNot : 'c -> UnOp.t -> UnOp.t
           ; visit_BitwiseOr : 'c -> BinOp.t -> BinOp.t
           ; visit_BitwiseOrL : 'c -> BinOp.t -> BinOp.t
           ; visit_BitwiseXor : 'c -> BinOp.t -> BinOp.t
           ; visit_BitwiseXorL : 'c -> BinOp.t -> BinOp.t
           ; visit_Bool : 'c -> Literal.t -> bool -> Literal.t
           ; visit_BooleanType : 'c -> Type.t -> Type.t
           ; visit_Branch : 'c -> LCmd.t -> Formula.t -> LCmd.t
           ; visit_Call :
               'c ->
               'f Cmd.t ->
               string ->
               Expr.t ->
               Expr.t list ->
               'f option ->
               (string * (string * Expr.t) list) option ->
               'f Cmd.t
           ; visit_Car : 'c -> UnOp.t -> UnOp.t
           ; visit_Cdr : 'c -> UnOp.t -> UnOp.t
           ; visit_Constant : 'c -> Literal.t -> Constant.t -> Literal.t
           ; visit_ECall :
               'c ->
               'f Cmd.t ->
               string ->
               Expr.t ->
               Expr.t list ->
               'f option ->
               'f Cmd.t
           ; visit_EList : 'c -> Expr.t -> Expr.t list -> Expr.t
           ; visit_ESet : 'c -> Expr.t -> Expr.t list -> Expr.t
           ; visit_Emp : 'c -> Asrt.t -> Asrt.t
           ; visit_Empty : 'c -> Literal.t -> Literal.t
           ; visit_EmptyType : 'c -> Type.t -> Type.t
           ; visit_Epsilon : 'c -> Constant.t -> Constant.t
           ; visit_Eq : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
           ; visit_Equal : 'c -> BinOp.t -> BinOp.t
           ; visit_Error : 'c -> Flag.t -> Flag.t
           ; visit_FDiv : 'c -> BinOp.t -> BinOp.t
           ; visit_FLessThan : 'c -> BinOp.t -> BinOp.t
           ; visit_FLessThanEqual : 'c -> BinOp.t -> BinOp.t
           ; visit_FMinus : 'c -> BinOp.t -> BinOp.t
           ; visit_FMod : 'c -> BinOp.t -> BinOp.t
           ; visit_FPlus : 'c -> BinOp.t -> BinOp.t
           ; visit_FTimes : 'c -> BinOp.t -> BinOp.t
           ; visit_FUnaryMinus : 'c -> UnOp.t -> UnOp.t
           ; visit_Fail : 'c -> 'f Cmd.t -> string -> Expr.t list -> 'f Cmd.t
           ; visit_False : 'c -> Formula.t -> Formula.t
           ; visit_Fold :
               'c ->
               SLCmd.t ->
               string ->
               Expr.t list ->
               (string * (string * Expr.t) list) option ->
               SLCmd.t
           ; visit_ForAll :
               'c ->
               Formula.t ->
               (string * Type.t option) list ->
               Formula.t ->
               Formula.t
           ; visit_GA :
               'c -> Asrt.t -> string -> Expr.t list -> Expr.t list -> Asrt.t
           ; visit_GUnfold : 'c -> SLCmd.t -> string -> SLCmd.t
           ; visit_Goto : 'c -> 'f Cmd.t -> 'f -> 'f Cmd.t
           ; visit_GuardedGoto :
               'c -> 'f Cmd.t -> Expr.t -> 'f -> 'f -> 'f Cmd.t
           ; visit_IDiv : 'c -> BinOp.t -> BinOp.t
           ; visit_ILessThan : 'c -> BinOp.t -> BinOp.t
           ; visit_ILessThanEqual : 'c -> BinOp.t -> BinOp.t
           ; visit_IMinus : 'c -> BinOp.t -> BinOp.t
           ; visit_IMod : 'c -> BinOp.t -> BinOp.t
           ; visit_IPlus : 'c -> BinOp.t -> BinOp.t
           ; visit_ITimes : 'c -> BinOp.t -> BinOp.t
           ; visit_IUnaryMinus : 'c -> UnOp.t -> UnOp.t
           ; visit_If :
               'c -> LCmd.t -> Expr.t -> LCmd.t list -> LCmd.t list -> LCmd.t
           ; visit_Int : 'c -> Literal.t -> Z.t -> Literal.t
           ; visit_IntType : 'c -> Type.t -> Type.t
           ; visit_Invariant : 'c -> SLCmd.t -> Asrt.t -> string list -> SLCmd.t
           ; visit_LAction :
               'c -> 'f Cmd.t -> string -> string -> Expr.t list -> 'f Cmd.t
           ; visit_LList : 'c -> Literal.t -> Literal.t list -> Literal.t
           ; visit_LVar : 'c -> Expr.t -> string -> Expr.t
           ; visit_LeftShift : 'c -> BinOp.t -> BinOp.t
           ; visit_LeftShiftL : 'c -> BinOp.t -> BinOp.t
           ; visit_FLess : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
           ; visit_FLessEq : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
           ; visit_ILess : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
           ; visit_ILessEq : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
           ; visit_ListType : 'c -> Type.t -> Type.t
           ; visit_Lit : 'c -> Expr.t -> Literal.t -> Expr.t
           ; visit_Loc : 'c -> Literal.t -> string -> Literal.t
           ; visit_LocalTime : 'c -> Constant.t -> Constant.t
           ; visit_Logic : 'c -> 'f Cmd.t -> LCmd.t -> 'f Cmd.t
           ; visit_LstCat : 'c -> NOp.t -> NOp.t
           ; visit_LstLen : 'c -> UnOp.t -> UnOp.t
           ; visit_LstNth : 'c -> BinOp.t -> BinOp.t
           ; visit_LstRev : 'c -> UnOp.t -> UnOp.t
           ; visit_LstSub : 'c -> Expr.t -> Expr.t -> Expr.t -> Expr.t -> Expr.t
           ; visit_M_abs : 'c -> UnOp.t -> UnOp.t
           ; visit_M_acos : 'c -> UnOp.t -> UnOp.t
           ; visit_M_asin : 'c -> UnOp.t -> UnOp.t
           ; visit_M_atan : 'c -> UnOp.t -> UnOp.t
           ; visit_M_atan2 : 'c -> BinOp.t -> BinOp.t
           ; visit_M_ceil : 'c -> UnOp.t -> UnOp.t
           ; visit_M_cos : 'c -> UnOp.t -> UnOp.t
           ; visit_M_exp : 'c -> UnOp.t -> UnOp.t
           ; visit_M_floor : 'c -> UnOp.t -> UnOp.t
           ; visit_M_isNaN : 'c -> UnOp.t -> UnOp.t
           ; visit_M_log : 'c -> UnOp.t -> UnOp.t
           ; visit_M_pow : 'c -> BinOp.t -> BinOp.t
           ; visit_M_round : 'c -> UnOp.t -> UnOp.t
           ; visit_M_sgn : 'c -> UnOp.t -> UnOp.t
           ; visit_M_sin : 'c -> UnOp.t -> UnOp.t
           ; visit_M_sqrt : 'c -> UnOp.t -> UnOp.t
           ; visit_M_tan : 'c -> UnOp.t -> UnOp.t
           ; visit_Macro : 'c -> LCmd.t -> string -> Expr.t list -> LCmd.t
           ; visit_MaxSafeInteger : 'c -> Constant.t -> Constant.t
           ; visit_Max_float : 'c -> Constant.t -> Constant.t
           ; visit_Min_float : 'c -> Constant.t -> Constant.t
           ; visit_NOp : 'c -> Expr.t -> NOp.t -> Expr.t list -> Expr.t
           ; visit_NoneType : 'c -> Type.t -> Type.t
           ; visit_Nono : 'c -> Literal.t -> Literal.t
           ; visit_Normal : 'c -> Flag.t -> Flag.t
           ; visit_Not : 'c -> Formula.t -> Formula.t -> Formula.t
           ; visit_Null : 'c -> Literal.t -> Literal.t
           ; visit_NullType : 'c -> Type.t -> Type.t
           ; visit_Num : 'c -> Literal.t -> float -> Literal.t
           ; visit_NumberType : 'c -> Type.t -> Type.t
           ; visit_ObjectType : 'c -> Type.t -> Type.t
           ; visit_Or : 'c -> Formula.t -> Formula.t -> Formula.t -> Formula.t
           ; visit_PVar : 'c -> Expr.t -> string -> Expr.t
           ; visit_PhiAssignment :
               'c -> 'f Cmd.t -> (string * Expr.t list) list -> 'f Cmd.t
           ; visit_Pi : 'c -> Constant.t -> Constant.t
           ; visit_Pred : 'c -> Asrt.t -> string -> Expr.t list -> Asrt.t
           ; visit_Pure : 'c -> Asrt.t -> Formula.t -> Asrt.t
           ; visit_Random : 'c -> Constant.t -> Constant.t
           ; visit_ReturnError : 'c -> 'f Cmd.t -> 'f Cmd.t
           ; visit_ReturnNormal : 'c -> 'f Cmd.t -> 'f Cmd.t
           ; visit_SL : 'c -> LCmd.t -> SLCmd.t -> LCmd.t
           ; visit_SLessThan : 'c -> BinOp.t -> BinOp.t
           ; visit_SepAssert : 'c -> SLCmd.t -> Asrt.t -> string list -> SLCmd.t
           ; visit_SetDiff : 'c -> BinOp.t -> BinOp.t
           ; visit_SetInter : 'c -> NOp.t -> NOp.t
           ; visit_SetMem : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
           ; visit_SetSub : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
           ; visit_SetToList : 'c -> UnOp.t -> UnOp.t
           ; visit_SetType : 'c -> Type.t -> Type.t
           ; visit_SetUnion : 'c -> NOp.t -> NOp.t
           ; visit_SignedRightShift : 'c -> BinOp.t -> BinOp.t
           ; visit_SignedRightShiftL : 'c -> BinOp.t -> BinOp.t
           ; visit_Skip : 'c -> 'f Cmd.t -> 'f Cmd.t
           ; visit_FreshSVar : 'c -> LCmd.t -> string -> LCmd.t
           ; visit_Star : 'c -> Asrt.t -> Asrt.t -> Asrt.t -> Asrt.t
           ; visit_StrCat : 'c -> BinOp.t -> BinOp.t
           ; visit_StrLen : 'c -> UnOp.t -> UnOp.t
           ; visit_NumToInt : 'c -> UnOp.t -> UnOp.t
           ; visit_IntToNum : 'c -> UnOp.t -> UnOp.t
           ; visit_StrLess : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
           ; visit_StrNth : 'c -> BinOp.t -> BinOp.t
           ; visit_String : 'c -> Literal.t -> string -> Literal.t
           ; visit_StringType : 'c -> Type.t -> Type.t
           ; visit_SymbExec : 'c -> SLCmd.t -> SLCmd.t
           ; visit_ToInt32Op : 'c -> UnOp.t -> UnOp.t
           ; visit_ToIntOp : 'c -> UnOp.t -> UnOp.t
           ; visit_ToNumberOp : 'c -> UnOp.t -> UnOp.t
           ; visit_ToStringOp : 'c -> UnOp.t -> UnOp.t
           ; visit_ToUint16Op : 'c -> UnOp.t -> UnOp.t
           ; visit_ToUint32Op : 'c -> UnOp.t -> UnOp.t
           ; visit_True : 'c -> Formula.t -> Formula.t
           ; visit_Type : 'c -> Literal.t -> Type.t -> Literal.t
           ; visit_TypeOf : 'c -> UnOp.t -> UnOp.t
           ; visit_TypeType : 'c -> Type.t -> Type.t
           ; visit_Types : 'c -> Asrt.t -> (Expr.t * Type.t) list -> Asrt.t
           ; visit_UNot : 'c -> UnOp.t -> UnOp.t
           ; visit_UTCTime : 'c -> Constant.t -> Constant.t
           ; visit_UnOp : 'c -> Expr.t -> UnOp.t -> Expr.t -> Expr.t
           ; visit_Undefined : 'c -> Literal.t -> Literal.t
           ; visit_UndefinedType : 'c -> Type.t -> Type.t
           ; visit_Unfold :
               'c ->
               SLCmd.t ->
               string ->
               Expr.t list ->
               (string * string) list option ->
               bool ->
               SLCmd.t
           ; visit_UnsignedRightShift : 'c -> BinOp.t -> BinOp.t
           ; visit_UnsignedRightShiftL : 'c -> BinOp.t -> BinOp.t
           ; visit_assertion : 'c -> Asrt.t -> Asrt.t
           ; visit_bindings :
               'c ->
               string * (string * Expr.t) list ->
               string * (string * Expr.t) list
           ; visit_binop : 'c -> BinOp.t -> BinOp.t
           ; visit_bispec : 'c -> BiSpec.t -> BiSpec.t
           ; visit_cmd : 'c -> 'f Cmd.t -> 'f Cmd.t
           ; visit_constant : 'c -> Constant.t -> Constant.t
           ; visit_expr : 'c -> Expr.t -> Expr.t
           ; visit_flag : 'c -> Flag.t -> Flag.t
           ; visit_formula : 'c -> Formula.t -> Formula.t
           ; visit_lcmd : 'c -> LCmd.t -> LCmd.t
           ; visit_lemma : 'c -> Lemma.t -> Lemma.t
           ; visit_lemma_spec : 'c -> Lemma.spec -> Lemma.spec
           ; visit_literal : 'c -> Literal.t -> Literal.t
           ; visit_macro : 'c -> Macro.t -> Macro.t
           ; visit_nop : 'c -> NOp.t -> NOp.t
           ; visit_pred : 'c -> Pred.t -> Pred.t
           ; visit_proc : 'c -> ('d, 'f) Proc.t -> ('d, 'f) Proc.t
           ; visit_single_spec : 'c -> Spec.st -> Spec.st
           ; visit_slcmd : 'c -> SLCmd.t -> SLCmd.t
           ; visit_spec : 'c -> Spec.t -> Spec.t
           ; visit_typ : 'c -> Type.t -> Type.t
           ; visit_unop : 'c -> UnOp.t -> UnOp.t
           ; .. >

      method visit_'annot : 'c -> 'd -> 'd
      method visit_'label : 'c -> 'f -> 'f
      method visit_ALoc : 'c -> Expr.t -> string -> Expr.t
      method visit_And : 'c -> Formula.t -> Formula.t -> Formula.t -> Formula.t

      method visit_Apply :
        'c -> 'f Cmd.t -> string -> Expr.t -> 'f option -> 'f Cmd.t

      method visit_ApplyLem :
        'c -> SLCmd.t -> string -> Expr.t list -> string list -> SLCmd.t

      method visit_Arguments : 'c -> 'f Cmd.t -> string -> 'f Cmd.t
      method visit_Assert : 'c -> LCmd.t -> Formula.t -> LCmd.t
      method visit_Assignment : 'c -> 'f Cmd.t -> string -> Expr.t -> 'f Cmd.t
      method visit_Assume : 'c -> LCmd.t -> Formula.t -> LCmd.t
      method visit_AssumeType : 'c -> LCmd.t -> Expr.t -> Type.t -> LCmd.t
      method visit_BAnd : 'c -> BinOp.t -> BinOp.t
      method visit_BOr : 'c -> BinOp.t -> BinOp.t
      method visit_BSetMem : 'c -> BinOp.t -> BinOp.t
      method visit_BSetSub : 'c -> BinOp.t -> BinOp.t
      method visit_BinOp : 'c -> Expr.t -> Expr.t -> BinOp.t -> Expr.t -> Expr.t
      method visit_BitwiseAnd : 'c -> BinOp.t -> BinOp.t
      method visit_BitwiseAndL : 'c -> BinOp.t -> BinOp.t
      method visit_BitwiseNot : 'c -> UnOp.t -> UnOp.t
      method visit_BitwiseOr : 'c -> BinOp.t -> BinOp.t
      method visit_BitwiseOrL : 'c -> BinOp.t -> BinOp.t
      method visit_BitwiseXor : 'c -> BinOp.t -> BinOp.t
      method visit_BitwiseXorL : 'c -> BinOp.t -> BinOp.t
      method visit_Bool : 'c -> Literal.t -> bool -> Literal.t
      method visit_BooleanType : 'c -> Type.t -> Type.t
      method visit_Branch : 'c -> LCmd.t -> Formula.t -> LCmd.t

      method visit_Call :
        'c ->
        'f Cmd.t ->
        string ->
        Expr.t ->
        Expr.t list ->
        'f option ->
        (string * (string * Expr.t) list) option ->
        'f Cmd.t

      method visit_Car : 'c -> UnOp.t -> UnOp.t
      method visit_Cdr : 'c -> UnOp.t -> UnOp.t
      method visit_Constant : 'c -> Literal.t -> Constant.t -> Literal.t

      method visit_ECall :
        'c ->
        'f Cmd.t ->
        string ->
        Expr.t ->
        Expr.t list ->
        'f option ->
        'f Cmd.t

      method visit_EList : 'c -> Expr.t -> Expr.t list -> Expr.t
      method visit_ESet : 'c -> Expr.t -> Expr.t list -> Expr.t
      method visit_Emp : 'c -> Asrt.t -> Asrt.t
      method visit_Empty : 'c -> Literal.t -> Literal.t
      method visit_EmptyType : 'c -> Type.t -> Type.t
      method visit_Epsilon : 'c -> Constant.t -> Constant.t
      method visit_Eq : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
      method visit_Equal : 'c -> BinOp.t -> BinOp.t
      method visit_Error : 'c -> Flag.t -> Flag.t
      method visit_FDiv : 'c -> BinOp.t -> BinOp.t
      method visit_FLessThan : 'c -> BinOp.t -> BinOp.t
      method visit_FLessThanEqual : 'c -> BinOp.t -> BinOp.t
      method visit_FMinus : 'c -> BinOp.t -> BinOp.t
      method visit_FMod : 'c -> BinOp.t -> BinOp.t
      method visit_FPlus : 'c -> BinOp.t -> BinOp.t
      method visit_FTimes : 'c -> BinOp.t -> BinOp.t
      method visit_FUnaryMinus : 'c -> UnOp.t -> UnOp.t
      method visit_Fail : 'c -> 'f Cmd.t -> string -> Expr.t list -> 'f Cmd.t
      method visit_False : 'c -> Formula.t -> Formula.t

      method visit_Fold :
        'c ->
        SLCmd.t ->
        string ->
        Expr.t list ->
        (string * (string * Expr.t) list) option ->
        SLCmd.t

      method visit_ForAll :
        'c ->
        Formula.t ->
        (string * Type.t option) list ->
        Formula.t ->
        Formula.t

      method visit_GA :
        'c -> Asrt.t -> string -> Expr.t list -> Expr.t list -> Asrt.t

      method visit_GUnfold : 'c -> SLCmd.t -> string -> SLCmd.t
      method visit_Goto : 'c -> 'f Cmd.t -> 'f -> 'f Cmd.t

      method visit_GuardedGoto :
        'c -> 'f Cmd.t -> Expr.t -> 'f -> 'f -> 'f Cmd.t

      method visit_IDiv : 'c -> BinOp.t -> BinOp.t
      method visit_ILessThan : 'c -> BinOp.t -> BinOp.t
      method visit_ILessThanEqual : 'c -> BinOp.t -> BinOp.t
      method visit_IMinus : 'c -> BinOp.t -> BinOp.t
      method visit_IMod : 'c -> BinOp.t -> BinOp.t
      method visit_IPlus : 'c -> BinOp.t -> BinOp.t
      method visit_ITimes : 'c -> BinOp.t -> BinOp.t
      method visit_IUnaryMinus : 'c -> UnOp.t -> UnOp.t

      method visit_If :
        'c -> LCmd.t -> Expr.t -> LCmd.t list -> LCmd.t list -> LCmd.t

      method visit_Int : 'c -> Literal.t -> Z.t -> Literal.t
      method visit_IntType : 'c -> Type.t -> Type.t
      method visit_Invariant : 'c -> SLCmd.t -> Asrt.t -> string list -> SLCmd.t

      method visit_LAction :
        'c -> 'f Cmd.t -> string -> string -> Expr.t list -> 'f Cmd.t

      method visit_LList : 'c -> Literal.t -> Literal.t list -> Literal.t
      method visit_LVar : 'c -> Expr.t -> string -> Expr.t
      method visit_LeftShift : 'c -> BinOp.t -> BinOp.t
      method visit_LeftShiftL : 'c -> BinOp.t -> BinOp.t
      method visit_FLess : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
      method visit_FLessEq : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
      method visit_ILess : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
      method visit_ILessEq : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
      method visit_ListType : 'c -> Type.t -> Type.t
      method visit_Lit : 'c -> Expr.t -> Literal.t -> Expr.t
      method visit_Loc : 'c -> Literal.t -> string -> Literal.t
      method visit_LocalTime : 'c -> Constant.t -> Constant.t
      method visit_Logic : 'c -> 'f Cmd.t -> LCmd.t -> 'f Cmd.t
      method visit_LstCat : 'c -> NOp.t -> NOp.t
      method visit_LstLen : 'c -> UnOp.t -> UnOp.t
      method visit_LstNth : 'c -> BinOp.t -> BinOp.t
      method visit_LstRev : 'c -> UnOp.t -> UnOp.t
      method visit_LstSub : 'c -> Expr.t -> Expr.t -> Expr.t -> Expr.t -> Expr.t
      method visit_M_abs : 'c -> UnOp.t -> UnOp.t
      method visit_M_acos : 'c -> UnOp.t -> UnOp.t
      method visit_M_asin : 'c -> UnOp.t -> UnOp.t
      method visit_M_atan : 'c -> UnOp.t -> UnOp.t
      method visit_M_atan2 : 'c -> BinOp.t -> BinOp.t
      method visit_M_ceil : 'c -> UnOp.t -> UnOp.t
      method visit_M_cos : 'c -> UnOp.t -> UnOp.t
      method visit_M_exp : 'c -> UnOp.t -> UnOp.t
      method visit_M_floor : 'c -> UnOp.t -> UnOp.t
      method visit_M_isNaN : 'c -> UnOp.t -> UnOp.t
      method visit_M_log : 'c -> UnOp.t -> UnOp.t
      method visit_M_pow : 'c -> BinOp.t -> BinOp.t
      method visit_M_round : 'c -> UnOp.t -> UnOp.t
      method visit_M_sgn : 'c -> UnOp.t -> UnOp.t
      method visit_M_sin : 'c -> UnOp.t -> UnOp.t
      method visit_M_sqrt : 'c -> UnOp.t -> UnOp.t
      method visit_M_tan : 'c -> UnOp.t -> UnOp.t
      method visit_Macro : 'c -> LCmd.t -> string -> Expr.t list -> LCmd.t
      method visit_MaxSafeInteger : 'c -> Constant.t -> Constant.t
      method visit_Max_float : 'c -> Constant.t -> Constant.t
      method visit_Min_float : 'c -> Constant.t -> Constant.t
      method visit_NOp : 'c -> Expr.t -> NOp.t -> Expr.t list -> Expr.t
      method visit_NoneType : 'c -> Type.t -> Type.t
      method visit_Nono : 'c -> Literal.t -> Literal.t
      method visit_Normal : 'c -> Flag.t -> Flag.t
      method visit_Not : 'c -> Formula.t -> Formula.t -> Formula.t
      method visit_Null : 'c -> Literal.t -> Literal.t
      method visit_NullType : 'c -> Type.t -> Type.t
      method visit_Num : 'c -> Literal.t -> float -> Literal.t
      method visit_NumberType : 'c -> Type.t -> Type.t
      method visit_ObjectType : 'c -> Type.t -> Type.t
      method visit_Or : 'c -> Formula.t -> Formula.t -> Formula.t -> Formula.t
      method visit_PVar : 'c -> Expr.t -> string -> Expr.t

      method visit_PhiAssignment :
        'c -> 'f Cmd.t -> (string * Expr.t list) list -> 'f Cmd.t

      method visit_Pi : 'c -> Constant.t -> Constant.t
      method visit_Pred : 'c -> Asrt.t -> string -> Expr.t list -> Asrt.t
      method visit_Pure : 'c -> Asrt.t -> Formula.t -> Asrt.t
      method visit_Random : 'c -> Constant.t -> Constant.t
      method visit_ReturnError : 'c -> 'f Cmd.t -> 'f Cmd.t
      method visit_ReturnNormal : 'c -> 'f Cmd.t -> 'f Cmd.t
      method visit_SL : 'c -> LCmd.t -> SLCmd.t -> LCmd.t
      method visit_SLessThan : 'c -> BinOp.t -> BinOp.t
      method visit_SepAssert : 'c -> SLCmd.t -> Asrt.t -> string list -> SLCmd.t
      method visit_SetDiff : 'c -> BinOp.t -> BinOp.t
      method visit_SetInter : 'c -> NOp.t -> NOp.t
      method visit_SetMem : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
      method visit_SetSub : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
      method visit_SetToList : 'c -> UnOp.t -> UnOp.t
      method visit_SetType : 'c -> Type.t -> Type.t
      method visit_SetUnion : 'c -> NOp.t -> NOp.t
      method visit_SignedRightShift : 'c -> BinOp.t -> BinOp.t
      method visit_SignedRightShiftL : 'c -> BinOp.t -> BinOp.t
      method visit_Skip : 'c -> 'f Cmd.t -> 'f Cmd.t
      method visit_FreshSVar : 'c -> LCmd.t -> string -> LCmd.t
      method visit_Star : 'c -> Asrt.t -> Asrt.t -> Asrt.t -> Asrt.t
      method visit_StrCat : 'c -> BinOp.t -> BinOp.t
      method visit_StrLen : 'c -> UnOp.t -> UnOp.t
      method visit_IntToNum : 'c -> UnOp.t -> UnOp.t
      method visit_NumToInt : 'c -> UnOp.t -> UnOp.t
      method visit_StrLess : 'c -> Formula.t -> Expr.t -> Expr.t -> Formula.t
      method visit_StrNth : 'c -> BinOp.t -> BinOp.t
      method visit_String : 'c -> Literal.t -> string -> Literal.t
      method visit_StringType : 'c -> Type.t -> Type.t
      method visit_SymbExec : 'c -> SLCmd.t -> SLCmd.t
      method visit_ToInt32Op : 'c -> UnOp.t -> UnOp.t
      method visit_ToIntOp : 'c -> UnOp.t -> UnOp.t
      method visit_ToNumberOp : 'c -> UnOp.t -> UnOp.t
      method visit_ToStringOp : 'c -> UnOp.t -> UnOp.t
      method visit_ToUint16Op : 'c -> UnOp.t -> UnOp.t
      method visit_ToUint32Op : 'c -> UnOp.t -> UnOp.t
      method visit_True : 'c -> Formula.t -> Formula.t
      method visit_Type : 'c -> Literal.t -> Type.t -> Literal.t
      method visit_TypeOf : 'c -> UnOp.t -> UnOp.t
      method visit_TypeType : 'c -> Type.t -> Type.t
      method visit_Types : 'c -> Asrt.t -> (Expr.t * Type.t) list -> Asrt.t
      method visit_UNot : 'c -> UnOp.t -> UnOp.t
      method visit_UTCTime : 'c -> Constant.t -> Constant.t
      method visit_UnOp : 'c -> Expr.t -> UnOp.t -> Expr.t -> Expr.t
      method visit_Undefined : 'c -> Literal.t -> Literal.t
      method visit_UndefinedType : 'c -> Type.t -> Type.t

      method visit_Unfold :
        'c ->
        SLCmd.t ->
        string ->
        Expr.t list ->
        (string * string) list option ->
        bool ->
        SLCmd.t

      method visit_UnsignedRightShift : 'c -> BinOp.t -> BinOp.t
      method visit_UnsignedRightShiftL : 'c -> BinOp.t -> BinOp.t

      method private visit_array :
        'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a array -> 'a array

      method visit_assertion : 'c -> Asrt.t -> Asrt.t

      method visit_bindings :
        'c -> string * (string * Expr.t) list -> string * (string * Expr.t) list

      method visit_binop : 'c -> BinOp.t -> BinOp.t
      method visit_bispec : 'c -> BiSpec.t -> BiSpec.t
      method private visit_bool : 'env. 'env -> bool -> bool
      method private visit_bytes : 'env. 'env -> bytes -> bytes
      method private visit_char : 'env. 'env -> char -> char
      method visit_cmd : 'c -> 'f Cmd.t -> 'f Cmd.t
      method visit_constant : 'c -> Constant.t -> Constant.t
      method visit_expr : 'c -> Expr.t -> Expr.t
      method visit_flag : 'c -> Flag.t -> Flag.t
      method private visit_float : 'env. 'env -> float -> float
      method visit_formula : 'c -> Formula.t -> Formula.t
      method private visit_int : 'env. 'env -> int -> int
      method private visit_int32 : 'env. 'env -> int32 -> int32
      method private visit_int64 : 'env. 'env -> int64 -> int64

      method private visit_lazy_t :
        'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a Lazy.t -> 'a Lazy.t

      method visit_lcmd : 'c -> LCmd.t -> LCmd.t
      method visit_lemma : 'c -> Lemma.t -> Lemma.t
      method visit_lemma_spec : 'c -> Lemma.spec -> Lemma.spec

      method private visit_list :
        'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a list -> 'a list

      method visit_literal : 'c -> Literal.t -> Literal.t
      method visit_macro : 'c -> Macro.t -> Macro.t
      method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
      method visit_nop : 'c -> NOp.t -> NOp.t

      method private visit_option :
        'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a option -> 'a option

      method visit_pred : 'c -> Pred.t -> Pred.t
      method visit_proc : 'c -> ('d, 'f) Proc.t -> ('d, 'f) Proc.t

      method private visit_ref :
        'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a ref -> 'a ref

      method private visit_result :
        'env 'a 'e.
        ('env -> 'a -> 'a) ->
        ('env -> 'e -> 'e) ->
        'env ->
        ('a, 'e) Result.result ->
        ('a, 'e) Result.result

      method visit_single_spec : 'c -> Spec.st -> Spec.st
      method visit_slcmd : 'c -> SLCmd.t -> SLCmd.t
      method visit_spec : 'c -> Spec.t -> Spec.t
      method private visit_string : 'env. 'env -> string -> string
      method visit_typ : 'c -> Type.t -> Type.t
      method private visit_unit : 'env. 'env -> unit -> unit
      method visit_unop : 'c -> UnOp.t -> UnOp.t
    end

  class virtual ['b] reduce :
    object ('b)
      constraint
      'b = < visit_'annot : 'c -> 'd -> 'f
           ; visit_'label : 'c -> 'g -> 'f
           ; visit_ALoc : 'c -> ALoc.t -> 'f
           ; visit_And : 'c -> Formula.t -> Formula.t -> 'f
           ; visit_Apply : 'c -> string -> Expr.t -> 'g option -> 'f
           ; visit_ApplyLem : 'c -> string -> Expr.t list -> string list -> 'f
           ; visit_Arguments : 'c -> string -> 'f
           ; visit_Assert : 'c -> Formula.t -> 'f
           ; visit_Assignment : 'c -> string -> Expr.t -> 'f
           ; visit_Assume : 'c -> Formula.t -> 'f
           ; visit_AssumeType : 'c -> Expr.t -> Type.t -> 'f
           ; visit_BAnd : 'c -> 'f
           ; visit_BOr : 'c -> 'f
           ; visit_BSetMem : 'c -> 'f
           ; visit_BSetSub : 'c -> 'f
           ; visit_BinOp : 'c -> Expr.t -> BinOp.t -> Expr.t -> 'f
           ; visit_BitwiseAnd : 'c -> 'f
           ; visit_BitwiseAndL : 'c -> 'f
           ; visit_BitwiseNot : 'c -> 'f
           ; visit_BitwiseOr : 'c -> 'f
           ; visit_BitwiseOrL : 'c -> 'f
           ; visit_BitwiseXor : 'c -> 'f
           ; visit_BitwiseXorL : 'c -> 'f
           ; visit_Bool : 'c -> bool -> 'f
           ; visit_BooleanType : 'c -> 'f
           ; visit_Branch : 'c -> Formula.t -> 'f
           ; visit_Call :
               'c ->
               string ->
               Expr.t ->
               Expr.t list ->
               'g option ->
               (string * (string * Expr.t) list) option ->
               'f
           ; visit_Car : 'c -> 'f
           ; visit_Cdr : 'c -> 'f
           ; visit_Constant : 'c -> Constant.t -> 'f
           ; visit_IDiv : 'c -> 'f
           ; visit_FDiv : 'c -> 'f
           ; visit_ECall :
               'c -> string -> Expr.t -> Expr.t list -> 'g option -> 'f
           ; visit_EList : 'c -> Expr.t list -> 'f
           ; visit_ESet : 'c -> Expr.t list -> 'f
           ; visit_Emp : 'c -> 'f
           ; visit_Empty : 'c -> 'f
           ; visit_EmptyType : 'c -> 'f
           ; visit_Epsilon : 'c -> 'f
           ; visit_Eq : 'c -> Expr.t -> Expr.t -> 'f
           ; visit_Equal : 'c -> 'f
           ; visit_Error : 'c -> 'f
           ; visit_Fail : 'c -> string -> Expr.t list -> 'f
           ; visit_False : 'c -> 'f
           ; visit_Fold :
               'c ->
               string ->
               Expr.t list ->
               (string * (string * Expr.t) list) option ->
               'f
           ; visit_ForAll :
               'c -> (string * Type.t option) list -> Formula.t -> 'f
           ; visit_GA : 'c -> string -> Expr.t list -> Expr.t list -> 'f
           ; visit_GUnfold : 'c -> string -> 'f
           ; visit_Goto : 'c -> 'g -> 'f
           ; visit_GuardedGoto : 'c -> Expr.t -> 'g -> 'g -> 'f
           ; visit_If : 'c -> Expr.t -> LCmd.t list -> LCmd.t list -> 'f
           ; visit_Invariant : 'c -> Asrt.t -> string list -> 'f
           ; visit_LAction : 'c -> string -> string -> Expr.t list -> 'f
           ; visit_LList : 'c -> Literal.t list -> 'f
           ; visit_LVar : 'c -> LVar.t -> 'f
           ; visit_LeftShift : 'c -> 'f
           ; visit_LeftShiftL : 'c -> 'f
           ; visit_FLess : 'c -> Expr.t -> Expr.t -> 'f
           ; visit_FLessEq : 'c -> Expr.t -> Expr.t -> 'f
           ; visit_ILess : 'c -> Expr.t -> Expr.t -> 'f
           ; visit_ILessEq : 'c -> Expr.t -> Expr.t -> 'f
           ; visit_ILessThan : 'c -> 'f
           ; visit_ILessThanEqual : 'c -> 'f
           ; visit_FLessThan : 'c -> 'f
           ; visit_FLessThanEqual : 'c -> 'f
           ; visit_SLessThan : 'c -> 'f
           ; visit_ListType : 'c -> 'f
           ; visit_Lit : 'c -> Literal.t -> 'f
           ; visit_Loc : 'c -> string -> 'f
           ; visit_LocalTime : 'c -> 'f
           ; visit_Logic : 'c -> LCmd.t -> 'f
           ; visit_LstCat : 'c -> 'f
           ; visit_LstLen : 'c -> 'f
           ; visit_LstNth : 'c -> 'f
           ; visit_LstRev : 'c -> 'f
           ; visit_LstSub : 'c -> Expr.t -> Expr.t -> Expr.t -> 'f
           ; visit_M_abs : 'c -> 'f
           ; visit_M_acos : 'c -> 'f
           ; visit_M_asin : 'c -> 'f
           ; visit_M_atan : 'c -> 'f
           ; visit_M_atan2 : 'c -> 'f
           ; visit_M_ceil : 'c -> 'f
           ; visit_M_cos : 'c -> 'f
           ; visit_M_exp : 'c -> 'f
           ; visit_M_floor : 'c -> 'f
           ; visit_M_isNaN : 'c -> 'f
           ; visit_M_log : 'c -> 'f
           ; visit_M_pow : 'c -> 'f
           ; visit_M_round : 'c -> 'f
           ; visit_M_sgn : 'c -> 'f
           ; visit_M_sin : 'c -> 'f
           ; visit_M_sqrt : 'c -> 'f
           ; visit_M_tan : 'c -> 'f
           ; visit_Macro : 'c -> string -> Expr.t list -> 'f
           ; visit_Max_float : 'c -> 'f
           ; visit_MaxSafeInteger : 'c -> 'f
           ; visit_Min_float : 'c -> 'f
           ; visit_IMinus : 'c -> 'f
           ; visit_FMinus : 'c -> 'f
           ; visit_IMod : 'c -> 'f
           ; visit_FMod : 'c -> 'f
           ; visit_NOp : 'c -> NOp.t -> Expr.t list -> 'f
           ; visit_NoneType : 'c -> 'f
           ; visit_Nono : 'c -> 'f
           ; visit_Normal : 'c -> 'f
           ; visit_Not : 'c -> Formula.t -> 'f
           ; visit_Null : 'c -> 'f
           ; visit_NullType : 'c -> 'f
           ; visit_Int : 'c -> Z.t -> 'f
           ; visit_Num : 'c -> float -> 'f
           ; visit_IntType : 'c -> 'f
           ; visit_NumberType : 'c -> 'f
           ; visit_ObjectType : 'c -> 'f
           ; visit_Or : 'c -> Formula.t -> Formula.t -> 'f
           ; visit_PVar : 'c -> string -> 'f
           ; visit_PhiAssignment : 'c -> (string * Expr.t list) list -> 'f
           ; visit_Pi : 'c -> 'f
           ; visit_IPlus : 'c -> 'f
           ; visit_FPlus : 'c -> 'f
           ; visit_Pred : 'c -> string -> Expr.t list -> 'f
           ; visit_Pure : 'c -> Formula.t -> 'f
           ; visit_Random : 'c -> 'f
           ; visit_ReturnError : 'c -> 'f
           ; visit_ReturnNormal : 'c -> 'f
           ; visit_SL : 'c -> SLCmd.t -> 'f
           ; visit_SepAssert : 'c -> Asrt.t -> string list -> 'f
           ; visit_SetDiff : 'c -> 'f
           ; visit_SetInter : 'c -> 'f
           ; visit_SetMem : 'c -> Expr.t -> Expr.t -> 'f
           ; visit_SetSub : 'c -> Expr.t -> Expr.t -> 'f
           ; visit_SetToList : 'c -> 'f
           ; visit_SetType : 'c -> 'f
           ; visit_SetUnion : 'c -> 'f
           ; visit_SignedRightShift : 'c -> 'f
           ; visit_SignedRightShiftL : 'c -> 'f
           ; visit_Skip : 'c -> 'f
           ; visit_FreshSVar : 'c -> string -> 'f
           ; visit_Star : 'c -> Asrt.t -> Asrt.t -> 'f
           ; visit_StrCat : 'c -> 'f
           ; visit_StrLen : 'c -> 'f
           ; visit_IntToNum : 'c -> 'f
           ; visit_NumToInt : 'c -> 'f
           ; visit_StrLess : 'c -> Expr.t -> Expr.t -> 'f
           ; visit_StrNth : 'c -> 'f
           ; visit_String : 'c -> string -> 'f
           ; visit_StringType : 'c -> 'f
           ; visit_SymbExec : 'c -> 'f
           ; visit_ITimes : 'c -> 'f
           ; visit_FTimes : 'c -> 'f
           ; visit_ToInt32Op : 'c -> 'f
           ; visit_ToIntOp : 'c -> 'f
           ; visit_ToNumberOp : 'c -> 'f
           ; visit_ToStringOp : 'c -> 'f
           ; visit_ToUint16Op : 'c -> 'f
           ; visit_ToUint32Op : 'c -> 'f
           ; visit_True : 'c -> 'f
           ; visit_Type : 'c -> Type.t -> 'f
           ; visit_TypeOf : 'c -> 'f
           ; visit_TypeType : 'c -> 'f
           ; visit_Types : 'c -> (Expr.t * Type.t) list -> 'f
           ; visit_UNot : 'c -> 'f
           ; visit_UTCTime : 'c -> 'f
           ; visit_UnOp : 'c -> UnOp.t -> Expr.t -> 'f
           ; visit_IUnaryMinus : 'c -> 'f
           ; visit_FUnaryMinus : 'c -> 'f
           ; visit_Undefined : 'c -> 'f
           ; visit_UndefinedType : 'c -> 'f
           ; visit_Unfold :
               'c ->
               string ->
               Expr.t list ->
               (string * string) list option ->
               bool ->
               'f
           ; visit_UnsignedRightShift : 'c -> 'f
           ; visit_UnsignedRightShiftL : 'c -> 'f
           ; visit_assertion : 'c -> Asrt.t -> 'f
           ; visit_bindings : 'c -> string * (string * Expr.t) list -> 'f
           ; visit_binop : 'c -> BinOp.t -> 'f
           ; visit_bispec : 'c -> BiSpec.t -> 'f
           ; visit_cmd : 'c -> 'g Cmd.t -> 'f
           ; visit_constant : 'c -> Constant.t -> 'f
           ; visit_expr : 'c -> Expr.t -> 'f
           ; visit_flag : 'c -> Flag.t -> 'f
           ; visit_formula : 'c -> Formula.t -> 'f
           ; visit_lcmd : 'c -> LCmd.t -> 'f
           ; visit_lemma : 'c -> Lemma.t -> 'f
           ; visit_lemma_spec : 'c -> Lemma.spec -> 'f
           ; visit_literal : 'c -> Literal.t -> 'f
           ; visit_macro : 'c -> Macro.t -> 'f
           ; visit_nop : 'c -> NOp.t -> 'f
           ; visit_pred : 'c -> Pred.t -> 'f
           ; visit_proc : 'c -> ('d, 'g) Proc.t -> 'f
           ; visit_single_spec : 'c -> Spec.st -> 'f
           ; visit_slcmd : 'c -> SLCmd.t -> 'f
           ; visit_spec : 'c -> Spec.t -> 'f
           ; visit_typ : 'c -> Type.t -> 'f
           ; visit_unop : 'c -> UnOp.t -> 'f
           ; .. >

      method virtual private plus : 'f -> 'f -> 'f
      method visit_'annot : 'c -> 'd -> 'f
      method visit_'label : 'c -> 'g -> 'f
      method visit_ALoc : 'c -> ALoc.t -> 'f
      method visit_And : 'c -> Formula.t -> Formula.t -> 'f
      method visit_Apply : 'c -> string -> Expr.t -> 'g option -> 'f
      method visit_ApplyLem : 'c -> string -> Expr.t list -> string list -> 'f
      method visit_Arguments : 'c -> string -> 'f
      method visit_Assert : 'c -> Formula.t -> 'f
      method visit_Assignment : 'c -> string -> Expr.t -> 'f
      method visit_Assume : 'c -> Formula.t -> 'f
      method visit_AssumeType : 'c -> Expr.t -> Type.t -> 'f
      method visit_BAnd : 'c -> 'f
      method visit_BOr : 'c -> 'f
      method visit_BSetMem : 'c -> 'f
      method visit_BSetSub : 'c -> 'f
      method visit_BinOp : 'c -> Expr.t -> BinOp.t -> Expr.t -> 'f
      method visit_BitwiseAnd : 'c -> 'f
      method visit_BitwiseAndL : 'c -> 'f
      method visit_BitwiseNot : 'c -> 'f
      method visit_BitwiseOr : 'c -> 'f
      method visit_BitwiseOrL : 'c -> 'f
      method visit_BitwiseXor : 'c -> 'f
      method visit_BitwiseXorL : 'c -> 'f
      method visit_Bool : 'c -> bool -> 'f
      method visit_BooleanType : 'c -> 'f
      method visit_Branch : 'c -> Formula.t -> 'f

      method visit_Call :
        'c ->
        string ->
        Expr.t ->
        Expr.t list ->
        'g option ->
        (string * (string * Expr.t) list) option ->
        'f

      method visit_Car : 'c -> 'f
      method visit_Cdr : 'c -> 'f
      method visit_Constant : 'c -> Constant.t -> 'f
      method visit_IDiv : 'c -> 'f
      method visit_FDiv : 'c -> 'f

      method visit_ECall :
        'c -> string -> Expr.t -> Expr.t list -> 'g option -> 'f

      method visit_EList : 'c -> Expr.t list -> 'f
      method visit_ESet : 'c -> Expr.t list -> 'f
      method visit_Emp : 'c -> 'f
      method visit_Empty : 'c -> 'f
      method visit_EmptyType : 'c -> 'f
      method visit_Epsilon : 'c -> 'f
      method visit_Eq : 'c -> Expr.t -> Expr.t -> 'f
      method visit_Equal : 'c -> 'f
      method visit_Error : 'c -> 'f
      method visit_Fail : 'c -> string -> Expr.t list -> 'f
      method visit_False : 'c -> 'f

      method visit_Fold :
        'c ->
        string ->
        Expr.t list ->
        (string * (string * Expr.t) list) option ->
        'f

      method visit_ForAll :
        'c -> (string * Type.t option) list -> Formula.t -> 'f

      method visit_GA : 'c -> string -> Expr.t list -> Expr.t list -> 'f
      method visit_GUnfold : 'c -> string -> 'f
      method visit_Goto : 'c -> 'g -> 'f
      method visit_GuardedGoto : 'c -> Expr.t -> 'g -> 'g -> 'f
      method visit_If : 'c -> Expr.t -> LCmd.t list -> LCmd.t list -> 'f
      method visit_Invariant : 'c -> Asrt.t -> string list -> 'f
      method visit_LAction : 'c -> string -> string -> Expr.t list -> 'f
      method visit_LList : 'c -> Literal.t list -> 'f
      method visit_LVar : 'c -> LVar.t -> 'f
      method visit_LeftShift : 'c -> 'f
      method visit_LeftShiftL : 'c -> 'f
      method visit_FLess : 'c -> Expr.t -> Expr.t -> 'f
      method visit_FLessEq : 'c -> Expr.t -> Expr.t -> 'f
      method visit_ILess : 'c -> Expr.t -> Expr.t -> 'f
      method visit_ILessEq : 'c -> Expr.t -> Expr.t -> 'f
      method visit_ILessThan : 'c -> 'f
      method visit_ILessThanEqual : 'c -> 'f
      method visit_FLessThan : 'c -> 'f
      method visit_FLessThanEqual : 'c -> 'f
      method visit_SLessThan : 'c -> 'f
      method visit_ListType : 'c -> 'f
      method visit_Lit : 'c -> Literal.t -> 'f
      method visit_Loc : 'c -> string -> 'f
      method visit_LocalTime : 'c -> 'f
      method visit_Logic : 'c -> LCmd.t -> 'f
      method visit_LstCat : 'c -> 'f
      method visit_LstLen : 'c -> 'f
      method visit_LstNth : 'c -> 'f
      method visit_LstRev : 'c -> 'f
      method visit_LstSub : 'c -> Expr.t -> Expr.t -> Expr.t -> 'f
      method visit_M_abs : 'c -> 'f
      method visit_M_acos : 'c -> 'f
      method visit_M_asin : 'c -> 'f
      method visit_M_atan : 'c -> 'f
      method visit_M_atan2 : 'c -> 'f
      method visit_M_ceil : 'c -> 'f
      method visit_M_cos : 'c -> 'f
      method visit_M_exp : 'c -> 'f
      method visit_M_floor : 'c -> 'f
      method visit_M_isNaN : 'c -> 'f
      method visit_M_log : 'c -> 'f
      method visit_M_pow : 'c -> 'f
      method visit_M_round : 'c -> 'f
      method visit_M_sgn : 'c -> 'f
      method visit_M_sin : 'c -> 'f
      method visit_M_sqrt : 'c -> 'f
      method visit_M_tan : 'c -> 'f
      method visit_Macro : 'c -> string -> Expr.t list -> 'f
      method visit_Max_float : 'c -> 'f
      method visit_MaxSafeInteger : 'c -> 'f
      method visit_Min_float : 'c -> 'f
      method visit_IMinus : 'c -> 'f
      method visit_FMinus : 'c -> 'f
      method visit_IMod : 'c -> 'f
      method visit_FMod : 'c -> 'f
      method visit_NOp : 'c -> NOp.t -> Expr.t list -> 'f
      method visit_NoneType : 'c -> 'f
      method visit_Nono : 'c -> 'f
      method visit_Normal : 'c -> 'f
      method visit_Not : 'c -> Formula.t -> 'f
      method visit_Null : 'c -> 'f
      method visit_NullType : 'c -> 'f
      method visit_Int : 'c -> Z.t -> 'f
      method visit_Num : 'c -> float -> 'f
      method visit_IntType : 'c -> 'f
      method visit_NumberType : 'c -> 'f
      method visit_ObjectType : 'c -> 'f
      method visit_Or : 'c -> Formula.t -> Formula.t -> 'f
      method visit_PVar : 'c -> string -> 'f
      method visit_PhiAssignment : 'c -> (string * Expr.t list) list -> 'f
      method visit_Pi : 'c -> 'f
      method visit_IPlus : 'c -> 'f
      method visit_FPlus : 'c -> 'f
      method visit_Pred : 'c -> string -> Expr.t list -> 'f
      method visit_Pure : 'c -> Formula.t -> 'f
      method visit_Random : 'c -> 'f
      method visit_ReturnError : 'c -> 'f
      method visit_ReturnNormal : 'c -> 'f
      method visit_SL : 'c -> SLCmd.t -> 'f
      method visit_SepAssert : 'c -> Asrt.t -> string list -> 'f
      method visit_SetDiff : 'c -> 'f
      method visit_SetInter : 'c -> 'f
      method visit_SetMem : 'c -> Expr.t -> Expr.t -> 'f
      method visit_SetSub : 'c -> Expr.t -> Expr.t -> 'f
      method visit_SetToList : 'c -> 'f
      method visit_SetType : 'c -> 'f
      method visit_SetUnion : 'c -> 'f
      method visit_SignedRightShift : 'c -> 'f
      method visit_SignedRightShiftL : 'c -> 'f
      method visit_Skip : 'c -> 'f
      method visit_FreshSVar : 'c -> string -> 'f
      method visit_Star : 'c -> Asrt.t -> Asrt.t -> 'f
      method visit_StrCat : 'c -> 'f
      method visit_StrLen : 'c -> 'f
      method visit_IntToNum : 'c -> 'f
      method visit_NumToInt : 'c -> 'f
      method visit_StrLess : 'c -> Expr.t -> Expr.t -> 'f
      method visit_StrNth : 'c -> 'f
      method visit_String : 'c -> string -> 'f
      method visit_StringType : 'c -> 'f
      method visit_SymbExec : 'c -> 'f
      method visit_ITimes : 'c -> 'f
      method visit_FTimes : 'c -> 'f
      method visit_ToInt32Op : 'c -> 'f
      method visit_ToIntOp : 'c -> 'f
      method visit_ToNumberOp : 'c -> 'f
      method visit_ToStringOp : 'c -> 'f
      method visit_ToUint16Op : 'c -> 'f
      method visit_ToUint32Op : 'c -> 'f
      method visit_True : 'c -> 'f
      method visit_Type : 'c -> Type.t -> 'f
      method visit_TypeOf : 'c -> 'f
      method visit_TypeType : 'c -> 'f
      method visit_Types : 'c -> (Expr.t * Type.t) list -> 'f
      method visit_UNot : 'c -> 'f
      method visit_UTCTime : 'c -> 'f
      method visit_UnOp : 'c -> UnOp.t -> Expr.t -> 'f
      method visit_IUnaryMinus : 'c -> 'f
      method visit_FUnaryMinus : 'c -> 'f
      method visit_Undefined : 'c -> 'f
      method visit_UndefinedType : 'c -> 'f

      method visit_Unfold :
        'c ->
        string ->
        Expr.t list ->
        (string * string) list option ->
        bool ->
        'f

      method visit_UnsignedRightShift : 'c -> 'f
      method visit_UnsignedRightShiftL : 'c -> 'f
      method visit_assertion : 'c -> Asrt.t -> 'f
      method visit_bindings : 'c -> string * (string * Expr.t) list -> 'f
      method visit_binop : 'c -> BinOp.t -> 'f
      method visit_bispec : 'c -> BiSpec.t -> 'f
      method visit_cmd : 'c -> 'g Cmd.t -> 'f
      method visit_constant : 'c -> Constant.t -> 'f
      method visit_expr : 'c -> Expr.t -> 'f
      method visit_flag : 'c -> Flag.t -> 'f
      method visit_formula : 'c -> Formula.t -> 'f
      method visit_lcmd : 'c -> LCmd.t -> 'f
      method visit_lemma : 'c -> Lemma.t -> 'f
      method visit_lemma_spec : 'c -> Lemma.spec -> 'f
      method visit_literal : 'c -> Literal.t -> 'f
      method visit_macro : 'c -> Macro.t -> 'f
      method visit_nop : 'c -> NOp.t -> 'f
      method visit_pred : 'c -> Pred.t -> 'f
      method visit_proc : 'c -> ('d, 'g) Proc.t -> 'f
      method visit_single_spec : 'c -> Spec.st -> 'f
      method visit_slcmd : 'c -> SLCmd.t -> 'f
      method visit_spec : 'c -> Spec.t -> 'f
      method visit_typ : 'c -> Type.t -> 'f
      method visit_unop : 'c -> UnOp.t -> 'f
      method virtual private zero : 'f
    end

  class ['b] iter :
    object ('b)
      constraint
      'b = < visit_'annot : 'c -> 'd -> unit
           ; visit_'label : 'c -> 'f -> unit
           ; visit_ALoc : 'c -> string -> unit
           ; visit_And : 'c -> Formula.t -> Formula.t -> unit
           ; visit_Apply : 'c -> string -> Expr.t -> 'f option -> unit
           ; visit_ApplyLem : 'c -> string -> Expr.t list -> string list -> unit
           ; visit_Arguments : 'c -> string -> unit
           ; visit_Assert : 'c -> Formula.t -> unit
           ; visit_Assignment : 'c -> string -> Expr.t -> unit
           ; visit_Assume : 'c -> Formula.t -> unit
           ; visit_AssumeType : 'c -> Expr.t -> Type.t -> unit
           ; visit_BAnd : 'c -> unit
           ; visit_BOr : 'c -> unit
           ; visit_BSetMem : 'c -> unit
           ; visit_BSetSub : 'c -> unit
           ; visit_BinOp : 'c -> Expr.t -> BinOp.t -> Expr.t -> unit
           ; visit_BitwiseAnd : 'c -> unit
           ; visit_BitwiseAndL : 'c -> unit
           ; visit_BitwiseNot : 'c -> unit
           ; visit_BitwiseOr : 'c -> unit
           ; visit_BitwiseOrL : 'c -> unit
           ; visit_BitwiseXor : 'c -> unit
           ; visit_BitwiseXorL : 'c -> unit
           ; visit_Bool : 'c -> bool -> unit
           ; visit_BooleanType : 'c -> unit
           ; visit_Branch : 'c -> Formula.t -> unit
           ; visit_Call :
               'c ->
               string ->
               Expr.t ->
               Expr.t list ->
               'f option ->
               Cmd.logic_bindings_t option ->
               unit
           ; visit_Car : 'c -> unit
           ; visit_Cdr : 'c -> unit
           ; visit_Constant : 'c -> Constant.t -> unit
           ; visit_ECall :
               'c -> string -> Expr.t -> Expr.t list -> 'f option -> unit
           ; visit_EList : 'c -> Expr.t list -> unit
           ; visit_ESet : 'c -> Expr.t list -> unit
           ; visit_Emp : 'c -> unit
           ; visit_Empty : 'c -> unit
           ; visit_EmptyType : 'c -> unit
           ; visit_Epsilon : 'c -> unit
           ; visit_Eq : 'c -> Expr.t -> Expr.t -> unit
           ; visit_Equal : 'c -> unit
           ; visit_Error : 'c -> unit
           ; visit_FDiv : 'c -> unit
           ; visit_FLessThan : 'c -> unit
           ; visit_FLessThanEqual : 'c -> unit
           ; visit_FMinus : 'c -> unit
           ; visit_FMod : 'c -> unit
           ; visit_FPlus : 'c -> unit
           ; visit_FTimes : 'c -> unit
           ; visit_FUnaryMinus : 'c -> unit
           ; visit_Fail : 'c -> string -> Expr.t list -> unit
           ; visit_False : 'c -> unit
           ; visit_Fold :
               'c ->
               string ->
               Expr.t list ->
               (string * (string * Expr.t) list) option ->
               unit
           ; visit_ForAll :
               'c -> (string * Type.t option) list -> Formula.t -> unit
           ; visit_GA : 'c -> string -> Expr.t list -> Expr.t list -> unit
           ; visit_GUnfold : 'c -> string -> unit
           ; visit_Goto : 'c -> 'f -> unit
           ; visit_GuardedGoto : 'c -> Expr.t -> 'f -> 'f -> unit
           ; visit_IDiv : 'c -> unit
           ; visit_ILessThan : 'c -> unit
           ; visit_ILessThanEqual : 'c -> unit
           ; visit_IMinus : 'c -> unit
           ; visit_IMod : 'c -> unit
           ; visit_IPlus : 'c -> unit
           ; visit_ITimes : 'c -> unit
           ; visit_IUnaryMinus : 'c -> unit
           ; visit_If : 'c -> Expr.t -> LCmd.t list -> LCmd.t list -> unit
           ; visit_Int : 'c -> Z.t -> unit
           ; visit_IntType : 'c -> unit
           ; visit_Invariant : 'c -> Asrt.t -> string list -> unit
           ; visit_LAction : 'c -> string -> string -> Expr.t list -> unit
           ; visit_LList : 'c -> Literal.t list -> unit
           ; visit_LVar : 'c -> string -> unit
           ; visit_LeftShift : 'c -> unit
           ; visit_LeftShiftL : 'c -> unit
           ; visit_FLess : 'c -> Expr.t -> Expr.t -> unit
           ; visit_FLessEq : 'c -> Expr.t -> Expr.t -> unit
           ; visit_ILess : 'c -> Expr.t -> Expr.t -> unit
           ; visit_ILessEq : 'c -> Expr.t -> Expr.t -> unit
           ; visit_ListType : 'c -> unit
           ; visit_Lit : 'c -> Literal.t -> unit
           ; visit_Loc : 'c -> string -> unit
           ; visit_LocalTime : 'c -> unit
           ; visit_Logic : 'c -> LCmd.t -> unit
           ; visit_LstCat : 'c -> unit
           ; visit_LstLen : 'c -> unit
           ; visit_LstNth : 'c -> unit
           ; visit_LstRev : 'c -> unit
           ; visit_LstSub : 'c -> Expr.t -> Expr.t -> Expr.t -> unit
           ; visit_M_abs : 'c -> unit
           ; visit_M_acos : 'c -> unit
           ; visit_M_asin : 'c -> unit
           ; visit_M_atan : 'c -> unit
           ; visit_M_atan2 : 'c -> unit
           ; visit_M_ceil : 'c -> unit
           ; visit_M_cos : 'c -> unit
           ; visit_M_exp : 'c -> unit
           ; visit_M_floor : 'c -> unit
           ; visit_M_isNaN : 'c -> unit
           ; visit_M_log : 'c -> unit
           ; visit_M_pow : 'c -> unit
           ; visit_M_round : 'c -> unit
           ; visit_M_sgn : 'c -> unit
           ; visit_M_sin : 'c -> unit
           ; visit_M_sqrt : 'c -> unit
           ; visit_M_tan : 'c -> unit
           ; visit_Macro : 'c -> string -> Expr.t list -> unit
           ; visit_MaxSafeInteger : 'c -> unit
           ; visit_Max_float : 'c -> unit
           ; visit_Min_float : 'c -> unit
           ; visit_NOp : 'c -> NOp.t -> Expr.t list -> unit
           ; visit_NoneType : 'c -> unit
           ; visit_Nono : 'c -> unit
           ; visit_Normal : 'c -> unit
           ; visit_Not : 'c -> Formula.t -> unit
           ; visit_Null : 'c -> unit
           ; visit_NullType : 'c -> unit
           ; visit_Num : 'c -> float -> unit
           ; visit_NumberType : 'c -> unit
           ; visit_ObjectType : 'c -> unit
           ; visit_Or : 'c -> Formula.t -> Formula.t -> unit
           ; visit_PVar : 'c -> string -> unit
           ; visit_PhiAssignment : 'c -> (string * Expr.t list) list -> unit
           ; visit_Pi : 'c -> unit
           ; visit_Pred : 'c -> string -> Expr.t list -> unit
           ; visit_Pure : 'c -> Formula.t -> unit
           ; visit_Random : 'c -> unit
           ; visit_ReturnError : 'c -> unit
           ; visit_ReturnNormal : 'c -> unit
           ; visit_SL : 'c -> SLCmd.t -> unit
           ; visit_SLessThan : 'c -> unit
           ; visit_SepAssert : 'c -> Asrt.t -> string list -> unit
           ; visit_SetDiff : 'c -> unit
           ; visit_SetInter : 'c -> unit
           ; visit_SetMem : 'c -> Expr.t -> Expr.t -> unit
           ; visit_SetSub : 'c -> Expr.t -> Expr.t -> unit
           ; visit_SetToList : 'c -> unit
           ; visit_SetType : 'c -> unit
           ; visit_SetUnion : 'c -> unit
           ; visit_SignedRightShift : 'c -> unit
           ; visit_SignedRightShiftL : 'c -> unit
           ; visit_Skip : 'c -> unit
           ; visit_FreshSVar : 'c -> string -> unit
           ; visit_Star : 'c -> Asrt.t -> Asrt.t -> unit
           ; visit_StrCat : 'c -> unit
           ; visit_StrLen : 'c -> unit
           ; visit_IntToNum : 'c -> unit
           ; visit_NumToInt : 'c -> unit
           ; visit_StrLess : 'c -> Expr.t -> Expr.t -> unit
           ; visit_StrNth : 'c -> unit
           ; visit_String : 'c -> string -> unit
           ; visit_StringType : 'c -> unit
           ; visit_SymbExec : 'c -> unit
           ; visit_ToInt32Op : 'c -> unit
           ; visit_ToIntOp : 'c -> unit
           ; visit_ToNumberOp : 'c -> unit
           ; visit_ToStringOp : 'c -> unit
           ; visit_ToUint16Op : 'c -> unit
           ; visit_ToUint32Op : 'c -> unit
           ; visit_True : 'c -> unit
           ; visit_Type : 'c -> Type.t -> unit
           ; visit_TypeOf : 'c -> unit
           ; visit_TypeType : 'c -> unit
           ; visit_Types : 'c -> (Expr.t * Type.t) list -> unit
           ; visit_UNot : 'c -> unit
           ; visit_UTCTime : 'c -> unit
           ; visit_UnOp : 'c -> UnOp.t -> Expr.t -> unit
           ; visit_Undefined : 'c -> unit
           ; visit_UndefinedType : 'c -> unit
           ; visit_Unfold :
               'c ->
               string ->
               Expr.t list ->
               (string * string) list option ->
               bool ->
               unit
           ; visit_UnsignedRightShift : 'c -> unit
           ; visit_UnsignedRightShiftL : 'c -> unit
           ; visit_assertion : 'c -> Asrt.t -> unit
           ; visit_bindings : 'c -> string * (string * Expr.t) list -> unit
           ; visit_binop : 'c -> BinOp.t -> unit
           ; visit_bispec : 'c -> BiSpec.t -> unit
           ; visit_cmd : 'c -> 'f Cmd.t -> unit
           ; visit_constant : 'c -> Constant.t -> unit
           ; visit_expr : 'c -> Expr.t -> unit
           ; visit_flag : 'c -> Flag.t -> unit
           ; visit_formula : 'c -> Formula.t -> unit
           ; visit_lcmd : 'c -> LCmd.t -> unit
           ; visit_lemma : 'c -> Lemma.t -> unit
           ; visit_lemma_spec : 'c -> Lemma.spec -> unit
           ; visit_literal : 'c -> Literal.t -> unit
           ; visit_macro : 'c -> Macro.t -> unit
           ; visit_nop : 'c -> NOp.t -> unit
           ; visit_pred : 'c -> Pred.t -> unit
           ; visit_proc : 'c -> ('d, 'f) Proc.t -> unit
           ; visit_single_spec : 'c -> Spec.st -> unit
           ; visit_slcmd : 'c -> SLCmd.t -> unit
           ; visit_spec : 'c -> Spec.t -> unit
           ; visit_typ : 'c -> Type.t -> unit
           ; visit_unop : 'c -> UnOp.t -> unit
           ; .. >

      method visit_'annot : 'c -> 'd -> unit
      method visit_'label : 'c -> 'f -> unit
      method visit_ALoc : 'c -> string -> unit
      method visit_And : 'c -> Formula.t -> Formula.t -> unit
      method visit_Apply : 'c -> string -> Expr.t -> 'f option -> unit
      method visit_ApplyLem : 'c -> string -> Expr.t list -> string list -> unit
      method visit_Arguments : 'c -> string -> unit
      method visit_Assert : 'c -> Formula.t -> unit
      method visit_Assignment : 'c -> string -> Expr.t -> unit
      method visit_Assume : 'c -> Formula.t -> unit
      method visit_AssumeType : 'c -> Expr.t -> Type.t -> unit
      method visit_BAnd : 'c -> unit
      method visit_BOr : 'c -> unit
      method visit_BSetMem : 'c -> unit
      method visit_BSetSub : 'c -> unit
      method visit_BinOp : 'c -> Expr.t -> BinOp.t -> Expr.t -> unit
      method visit_BitwiseAnd : 'c -> unit
      method visit_BitwiseAndL : 'c -> unit
      method visit_BitwiseNot : 'c -> unit
      method visit_BitwiseOr : 'c -> unit
      method visit_BitwiseOrL : 'c -> unit
      method visit_BitwiseXor : 'c -> unit
      method visit_BitwiseXorL : 'c -> unit
      method visit_Bool : 'c -> bool -> unit
      method visit_BooleanType : 'c -> unit
      method visit_Branch : 'c -> Formula.t -> unit

      method visit_Call :
        'c ->
        string ->
        Expr.t ->
        Expr.t list ->
        'f option ->
        (string * (string * Expr.t) list) option ->
        unit

      method visit_Car : 'c -> unit
      method visit_Cdr : 'c -> unit
      method visit_Constant : 'c -> Constant.t -> unit

      method visit_ECall :
        'c -> string -> Expr.t -> Expr.t list -> 'f option -> unit

      method visit_EList : 'c -> Expr.t list -> unit
      method visit_ESet : 'c -> Expr.t list -> unit
      method visit_Emp : 'c -> unit
      method visit_Empty : 'c -> unit
      method visit_EmptyType : 'c -> unit
      method visit_Epsilon : 'c -> unit
      method visit_Eq : 'c -> Expr.t -> Expr.t -> unit
      method visit_Equal : 'c -> unit
      method visit_Error : 'c -> unit
      method visit_FDiv : 'c -> unit
      method visit_FLessThan : 'c -> unit
      method visit_FLessThanEqual : 'c -> unit
      method visit_FMinus : 'c -> unit
      method visit_FMod : 'c -> unit
      method visit_FPlus : 'c -> unit
      method visit_FTimes : 'c -> unit
      method visit_FUnaryMinus : 'c -> unit
      method visit_Fail : 'c -> string -> Expr.t list -> unit
      method visit_False : 'c -> unit

      method visit_Fold :
        'c ->
        string ->
        Expr.t list ->
        (string * (string * Expr.t) list) option ->
        unit

      method visit_ForAll :
        'c -> (string * Type.t option) list -> Formula.t -> unit

      method visit_GA : 'c -> string -> Expr.t list -> Expr.t list -> unit
      method visit_GUnfold : 'c -> string -> unit
      method visit_Goto : 'c -> 'f -> unit
      method visit_GuardedGoto : 'c -> Expr.t -> 'f -> 'f -> unit
      method visit_IDiv : 'c -> unit
      method visit_ILessThan : 'c -> unit
      method visit_ILessThanEqual : 'c -> unit
      method visit_IMinus : 'c -> unit
      method visit_IMod : 'c -> unit
      method visit_IPlus : 'c -> unit
      method visit_ITimes : 'c -> unit
      method visit_IUnaryMinus : 'c -> unit
      method visit_If : 'c -> Expr.t -> LCmd.t list -> LCmd.t list -> unit
      method visit_Int : 'c -> Z.t -> unit
      method visit_IntType : 'c -> unit
      method visit_Invariant : 'c -> Asrt.t -> string list -> unit
      method visit_LAction : 'c -> string -> string -> Expr.t list -> unit
      method visit_LList : 'c -> Literal.t list -> unit
      method visit_LVar : 'c -> string -> unit
      method visit_LeftShift : 'c -> unit
      method visit_LeftShiftL : 'c -> unit
      method visit_FLess : 'c -> Expr.t -> Expr.t -> unit
      method visit_FLessEq : 'c -> Expr.t -> Expr.t -> unit
      method visit_ILess : 'c -> Expr.t -> Expr.t -> unit
      method visit_ILessEq : 'c -> Expr.t -> Expr.t -> unit
      method visit_ListType : 'c -> unit
      method visit_Lit : 'c -> Literal.t -> unit
      method visit_Loc : 'c -> string -> unit
      method visit_LocalTime : 'c -> unit
      method visit_Logic : 'c -> LCmd.t -> unit
      method visit_LstCat : 'c -> unit
      method visit_LstLen : 'c -> unit
      method visit_LstNth : 'c -> unit
      method visit_LstRev : 'c -> unit
      method visit_LstSub : 'c -> Expr.t -> Expr.t -> Expr.t -> unit
      method visit_M_abs : 'c -> unit
      method visit_M_acos : 'c -> unit
      method visit_M_asin : 'c -> unit
      method visit_M_atan : 'c -> unit
      method visit_M_atan2 : 'c -> unit
      method visit_M_ceil : 'c -> unit
      method visit_M_cos : 'c -> unit
      method visit_M_exp : 'c -> unit
      method visit_M_floor : 'c -> unit
      method visit_M_isNaN : 'c -> unit
      method visit_M_log : 'c -> unit
      method visit_M_pow : 'c -> unit
      method visit_M_round : 'c -> unit
      method visit_M_sgn : 'c -> unit
      method visit_M_sin : 'c -> unit
      method visit_M_sqrt : 'c -> unit
      method visit_M_tan : 'c -> unit
      method visit_Macro : 'c -> string -> Expr.t list -> unit
      method visit_MaxSafeInteger : 'c -> unit
      method visit_Max_float : 'c -> unit
      method visit_Min_float : 'c -> unit
      method visit_NOp : 'c -> NOp.t -> Expr.t list -> unit
      method visit_NoneType : 'c -> unit
      method visit_Nono : 'c -> unit
      method visit_Normal : 'c -> unit
      method visit_Not : 'c -> Formula.t -> unit
      method visit_Null : 'c -> unit
      method visit_NullType : 'c -> unit
      method visit_Num : 'c -> float -> unit
      method visit_NumberType : 'c -> unit
      method visit_ObjectType : 'c -> unit
      method visit_Or : 'c -> Formula.t -> Formula.t -> unit
      method visit_PVar : 'c -> string -> unit
      method visit_PhiAssignment : 'c -> (string * Expr.t list) list -> unit
      method visit_Pi : 'c -> unit
      method visit_Pred : 'c -> string -> Expr.t list -> unit
      method visit_Pure : 'c -> Formula.t -> unit
      method visit_Random : 'c -> unit
      method visit_ReturnError : 'c -> unit
      method visit_ReturnNormal : 'c -> unit
      method visit_SL : 'c -> SLCmd.t -> unit
      method visit_SLessThan : 'c -> unit
      method visit_SepAssert : 'c -> Asrt.t -> string list -> unit
      method visit_SetDiff : 'c -> unit
      method visit_SetInter : 'c -> unit
      method visit_SetMem : 'c -> Expr.t -> Expr.t -> unit
      method visit_SetSub : 'c -> Expr.t -> Expr.t -> unit
      method visit_SetToList : 'c -> unit
      method visit_SetType : 'c -> unit
      method visit_SetUnion : 'c -> unit
      method visit_SignedRightShift : 'c -> unit
      method visit_SignedRightShiftL : 'c -> unit
      method visit_Skip : 'c -> unit
      method visit_FreshSVar : 'c -> string -> unit
      method visit_Star : 'c -> Asrt.t -> Asrt.t -> unit
      method visit_StrCat : 'c -> unit
      method visit_StrLen : 'c -> unit
      method visit_IntToNum : 'c -> unit
      method visit_NumToInt : 'c -> unit
      method visit_StrLess : 'c -> Expr.t -> Expr.t -> unit
      method visit_StrNth : 'c -> unit
      method visit_String : 'c -> string -> unit
      method visit_StringType : 'c -> unit
      method visit_SymbExec : 'c -> unit
      method visit_ToInt32Op : 'c -> unit
      method visit_ToIntOp : 'c -> unit
      method visit_ToNumberOp : 'c -> unit
      method visit_ToStringOp : 'c -> unit
      method visit_ToUint16Op : 'c -> unit
      method visit_ToUint32Op : 'c -> unit
      method visit_True : 'c -> unit
      method visit_Type : 'c -> Type.t -> unit
      method visit_TypeOf : 'c -> unit
      method visit_TypeType : 'c -> unit
      method visit_Types : 'c -> (Expr.t * Type.t) list -> unit
      method visit_UNot : 'c -> unit
      method visit_UTCTime : 'c -> unit
      method visit_UnOp : 'c -> UnOp.t -> Expr.t -> unit
      method visit_Undefined : 'c -> unit
      method visit_UndefinedType : 'c -> unit

      method visit_Unfold :
        'c ->
        string ->
        Expr.t list ->
        (string * string) list option ->
        bool ->
        unit

      method visit_UnsignedRightShift : 'c -> unit
      method visit_UnsignedRightShiftL : 'c -> unit

      method private visit_array :
        'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit

      method visit_assertion : 'c -> Asrt.t -> unit
      method visit_bindings : 'c -> string * (string * Expr.t) list -> unit
      method visit_binop : 'c -> BinOp.t -> unit
      method visit_bispec : 'c -> BiSpec.t -> unit
      method private visit_bool : 'env. 'env -> bool -> unit
      method private visit_bytes : 'env. 'env -> bytes -> unit
      method private visit_char : 'env. 'env -> char -> unit
      method visit_cmd : 'c -> 'f Cmd.t -> unit
      method visit_constant : 'c -> Constant.t -> unit
      method visit_expr : 'c -> Expr.t -> unit
      method visit_flag : 'c -> Flag.t -> unit
      method private visit_float : 'env. 'env -> float -> unit
      method visit_formula : 'c -> Formula.t -> unit
      method private visit_int : 'env. 'env -> int -> unit
      method private visit_int32 : 'env. 'env -> int32 -> unit
      method private visit_int64 : 'env. 'env -> int64 -> unit

      method private visit_lazy_t :
        'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit

      method visit_lcmd : 'c -> LCmd.t -> unit
      method visit_lemma : 'c -> Lemma.t -> unit
      method visit_lemma_spec : 'c -> Lemma.spec -> unit

      method private visit_list :
        'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit

      method visit_literal : 'c -> Literal.t -> unit
      method visit_macro : 'c -> Macro.t -> unit
      method private visit_nativeint : 'env. 'env -> nativeint -> unit
      method visit_nop : 'c -> NOp.t -> unit

      method private visit_option :
        'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit

      method visit_pred : 'c -> Pred.t -> unit
      method visit_proc : 'c -> ('d, 'f) Proc.t -> unit

      method private visit_ref :
        'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit

      method private visit_result :
        'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) ->
        'env ->
        ('a, 'e) Result.result ->
        unit

      method visit_single_spec : 'c -> Spec.st -> unit
      method visit_slcmd : 'c -> SLCmd.t -> unit
      method visit_spec : 'c -> Spec.t -> unit
      method private visit_string : 'env. 'env -> string -> unit
      method visit_typ : 'c -> Type.t -> unit
      method private visit_unit : 'env. 'env -> unit -> unit
      method visit_unop : 'c -> UnOp.t -> unit
    end

  module Utils : sig
    module SS = Containers.SS

    class list_monoid :
      object
        method private zero : 'b list
        method private plus : 'a list -> 'a list -> 'a list
      end

    (** Same as list_monoid but uses [rev_append] as [plus]. Will break any order-conservation *)
    class non_ordered_list_monoid :
      object
        method private zero : 'b list
        method private plus : 'a list -> 'a list -> 'a list
      end

    class ss_monoid :
      object
        method private zero : SS.t
        method private plus : SS.t -> SS.t -> SS.t
      end

    class two_list_monoid :
      object
        method private zero : 'c list * 'd list

        method private plus :
          'a list * 'b list -> 'a list * 'b list -> 'a list * 'b list
      end
  end
end
