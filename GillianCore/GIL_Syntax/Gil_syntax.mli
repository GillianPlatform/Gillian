open Containers

module LVar : Allocators.S with type t = string

module ALoc : Allocators.S with type t = string

module Var : sig
  (** GIL Variables *)

  type t = string

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

  val str : t -> string
  (** Printer *)
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

  val str : t -> string
  (** Printer *)

  (** Sets of types *)
  module Set : Set.S with type elt := t
end

module Literal : sig
  (** {b GIL Literals } *)
  type t =
    | Undefined  (** The literal [undefined] *)
    | Null  (** The literal [null] *)
    | Empty  (** The literal [empty] *)
    | Constant  of Constant.t  (** GIL constants ({!type:Constant.t}) *)
    | Bool      of bool  (** GIL booleans: [true] and [false] *)
    | Int       of int  (** GIL integers: TODO: understand size *)
    | Num       of float  (** GIL floats - double-precision 64-bit IEEE 754 *)
    | String    of string  (** GIL strings *)
    | Loc       of string  (** GIL locations (uninterpreted symbols) *)
    | Type      of Type.t  (** GIL types ({!type:Type.t}) *)
    | LList     of t list  (** Lists of GIL literals *)
    | Nono  (** Negative information *)

  val pp : t Fmt.t
  (** Pretty-printer *)

  val type_of : t -> Type.t
  (** [typeof lit] returns the type of the literal [lit] *)

  val evaluate_constant : Constant.t -> t
  (** [evaluate_constant c] evaluates the constant [c] *)

  val from_list : t list -> t
  (** [from_list lst] builds a GIL list [LList lst] from the OCaml list [lst] *)

  val to_list : t -> t list option
  (** [to_list lit] returns [Some l] if [lit = LList l], and otherwise [None] *)

  val base_elements : t -> t list
  (** [base_elements lit] Returns a list of all non-list literals occurring in [lit] *)
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
    | ToIntOp  (** Converts a float to an integer *)
    | ToUint16Op  (** Converts an integer to a 16-bit unsigned integer *)
    | ToUint32Op  (** Converts an integer to a 32-bit unsigned integer *)
    | ToInt32Op  (** Converts an integer to a 32-bit signed integer *)
    | ToNumberOp  (** Converts a string to a number *)
    | TypeOf
    | Car  (** Head of a list *)
    | Cdr  (** Tail of a list *)
    | LstLen  (** List length *)
    | LstRev  (** List reverse *)
    | SetToList  (** From set to list *)
    | StrLen  (** String length *)

  val str : t -> string
  (** Printer *)
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

  val str : t -> string
  (** Printer *)
end

module NOp : sig
  (** {b GIL N-ary Operators } *)
  type t =
    | LstCat  (** List concatenation *)
    | SetUnion  (** Set union *)
    | SetInter  (** Set intersection *)

  val str : t -> string
  (** Printer *)
end

module Expr : sig
  (** {b GIL Expressions } *)
  type t =
    | Lit    of Literal.t  (** GIL literals *)
    | PVar   of string  (** GIL program variables *)
    | LVar   of string  (** GIL logical variables (interpreted symbols) *)
    | ALoc   of string  (** GIL abstract locations (uninterpreted symbols) *)
    | UnOp   of UnOp.t * t  (** Unary operators ({!type:UnOp.t}) *)
    | BinOp  of t * BinOp.t * t  (** Binary operators ({!type:BinOp.t}) *)
    | LstSub of t * t * t  (** Sublist *)
    | NOp    of NOp.t * t list  (** n-ary operators ({!type:NOp.t}) *)
    | EList  of t list  (** Lists of expressions *)
    | ESet   of t list  (** Sets of expressions *)

  (** Sets of expressions *)
  module Set : Set.S with type elt := t

  (** Maps with expressions as keys *)
  module Map : Map.S with type key := t

  val equal : t -> t -> bool
  (** Equality *)

  val map : (t -> t * bool) -> (t -> t) option -> t -> t
  (** Mapper *)

  val map_opt : (t -> t option * bool) -> (t -> t) option -> t -> t option
  (** Optional mapper *)

  val pp : t Fmt.t
  (** Pretty-printer *)

  val full_pp : t Fmt.t
  (** Pretty-printer with constructors (will not parse) *)

  val to_list : t -> t list option
  (** If the expression is a list (either an [EList _] of Lit (LList _)), returns the list of expressions. *)

  val from_list : t list -> t
  (** [from_list] [EList] with the provided elements *)

  val fold :
    (t -> 'b -> 'b -> 'a list -> 'a) -> (t -> 'b -> 'b) option -> 'b -> t -> 'a
  (** Folder *)

  val lvars : t -> SS.t
  (** [lvars e] returns all logical variables in [e] *)

  val pvars : t -> SS.t
  (** [pvars e] returns all program variables in [e] *)

  val alocs : t -> SS.t
  (** [alocs e] returns all abstract locations in [e] *)

  val clocs : t -> SS.t
  (** [clocs e] returns all concrete locations in [e] *)

  val vars : t -> SS.t
  (** [vars e] returns all variables in [e] (includes lvars, pvars, alocs and clocs) *)

  val substitutables : t -> SS.t
  (** [substitutables e] returns all lvars and alocs *)

  val is_concrete : t -> bool
  (** [is_concrete e] returns [true] iff the expression contains no lvar or aloc *)

  val all_literals : t list -> bool
  (** [all_literals lst] returns [true] iff all elements of the given list [lst] are literals *)

  val from_lit_list : Literal.t -> t
  (** [from_lit_list lst] lifts a literal list to an expression list *)

  val lists : t -> t list
  (** [lists e] all sub-expressions of [e] of the form [Lit (LList lst)] and [EList lst] *)

  val subst_clocs : (string -> t) -> t -> t
  (** [subst_clocs subst e] substitutes expressions of the form [Lit (Loc l)] with [subst l] in [e] *)

  val from_var_name : string -> t
  (** [from_var_name var] returns either an aloc, an lvar or a pvar if [var] name matches one of these types
    (see {!Utils.Names.is_aloc_name}, {!Utils.Names.is_lvar_name} and {!Utils.Names.is_pvar_name}) *)

  val loc_from_loc_name : string -> t
  (** [loc_from_loc_name loc] Has the same behaviour as [from_var_name] except that it returns either an [ALoc loc] or a [Lit (Loc loc)] *)

  val subst_expr_for_expr : to_subst:t -> subst_with:t -> t -> t
  (** [subst_expr_for_expr ~to_subst ~subst_with expr] substitutes every occurence of the expression [to_subst] with the expression [subst_with] in [expr] *)

  val base_elements : t -> t list
  (** [base_elements e] returns the list containing all logical variables,
      abstract locations, and non-list literals in [e] *)
end

module Formula : sig
  (** {b GIL Formulae } *)
  type t =
    | True  (** Logical true *)
    | False  (** Logical false *)
    | Not     of t  (** Logical negation *)
    | And     of t * t  (** Logical conjunction *)
    | Or      of t * t  (** Logical disjunction *)
    | Eq      of Expr.t * Expr.t  (** Expression equality *)
    | Less    of Expr.t * Expr.t  (** Expression less-than for numbers *)
    | LessEq  of Expr.t * Expr.t
        (** Expression less-than-or-equal for numbers *)
    | StrLess of Expr.t * Expr.t  (** Expression less-than for strings *)
    | SetMem  of Expr.t * Expr.t  (** Set membership *)
    | SetSub  of Expr.t * Expr.t  (** Set subsetness *)
    | ForAll  of (string * Type.t option) list * t  (** Forall *)

  (** Sets of formulae *)
  module Set : Set.S with type elt := t

  val map :
    (t -> t * bool) option ->
    (t -> t) option ->
    (Expr.t -> Expr.t) option ->
    t ->
    t
  (** Deprecated. Use {!Visitors.map} instead *)

  val fold :
    (Expr.t -> 'a) option ->
    (t -> 'b -> 'b -> 'a list -> 'a) ->
    (t -> 'b -> 'b) option ->
    'b ->
    t ->
    'a
  (** Deprecated. Use {!Visitors.reduce} instead *)

  val lvars : t -> SS.t
  (** Get all the logical variables*)

  val pvars : t -> SS.t
  (** Get all the program variables *)

  val alocs : t -> SS.t
  (** Get all the abstract locations *)

  val clocs : t -> SS.t
  (** Get all the concrete locations *)

  val lists : t -> Expr.t list
  (** Get all the logical expressions of the formula of the form (Lit (LList lst)) and (EList lst) *)

  val list_lexprs : t -> Expr.t list
  (** Get all the list expressions *)

  val push_in_negations : t -> t
  (** [push_in_negations a] takes negations off the toplevel of [a] and pushes them in the leaves.
    For example [push_in_negations (Not (And (True, False)))] returns [Or (False, False)] *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printer *)

  val full_pp : Format.formatter -> t -> unit
  (** Pretty-printer with constructors (will not parse) *)

  val lift_logic_expr : Expr.t -> (t * t) option
  (** Lifts an expression to a formula, if possible. It returns
      the lifted expression and its negation *)

  val to_expr : t -> Expr.t option
  (** Unlifts the formula to an expression, if possible *)

  val conjunct : t list -> t
  (** [conjunct \[a1; ...; an\]] returns [a1 /\ ... /\ an] *)

  val disjunct : t list -> t
  (** [disjunct \[a1; ...; an\]] returns [a1 \/ ... \/ an] *)

  val subst_expr_for_expr : to_subst:Expr.t -> subst_with:Expr.t -> t -> t

  val subst_clocs : (string -> Expr.t) -> t -> t
  (** [subst_clocs subst e] Substitutes expressions of the form [Lit (Loc l)] with [subst l] in [e] *)

  val get_disjuncts : t -> t list
  (** [get_disjuncts (a1 \/ ... \/ an)] returns [\[a1; ...; an\]] *)

  val strings_and_numbers : t -> string list * float list
  (** Returns a list of strings and a list of numbers that are contained in the formula *)
end

module Asrt : sig
  (** {b GIL Assertions } *)
  type t =
    | Emp  (** Empty heap *)
    | Star  of t * t  (** Separating conjunction *)
    | Pred  of string * Expr.t list  (** Predicates *)
    | Pure  of Formula.t  (** Pure formula *)
    | Types of (Expr.t * Type.t) list  (** Typing assertion *)
    | GA    of string * Expr.t list * Expr.t list  (** Core assertion *)

  val compare : t -> t -> int
  (** Comparison of assertions *)

  val prioritise : t -> t -> int
  (** Sorting of assertions *)

  (** Sets of assertions *)
  module Set : Set.S with type elt := t

  val map :
    (t -> t * bool) option ->
    (t -> t) option ->
    (Expr.t -> Expr.t) option ->
    (Formula.t -> Formula.t) option ->
    t ->
    t
  (** Deprecated, use {!Visitors.map} instead. *)

  val fold :
    (Expr.t -> 'a) option ->
    (Formula.t -> 'a) option ->
    (t -> 'b -> 'b -> 'a list -> 'a) ->
    (t -> 'b -> 'b) option ->
    'b ->
    t ->
    'a

  val lists : t -> Expr.t list
  (** Get all the logical expressions of [a] of the form (Lit (LList lst)) and (EList lst) *)

  val list_lexprs : t -> Expr.t list
  (** Get all the logical expressions of [a] that denote a list
   and are not logical variables *)

  val lvars : t -> SS.t
  (** Get all the logical variables in [a] *)

  val pvars : t -> SS.t
  (** Get all the program variables in [a] *)

  val alocs : t -> SS.t
  (** Get all the abstract locations in [a] *)

  val clocs : t -> SS.t
  (** Get all the concrete locations in [a] *)

  val vars : t -> SS.t
  (** Get all the variables in [a] (includes lvars, pvars, alocs and clocs) *)

  val pred_names : t -> string list
  (** Returns a list with the names of the predicates that occur in [a] *)

  val pure_asrts : t -> Formula.t list
  (** Returns a list with the pure assertions that occur in [a] *)

  val simple_asrts : t -> t list
  (** Returns a list with the pure assertions that occur in [a] *)

  val is_pure_asrt : t -> bool
  (** Check if [a] is a pure assertion *)

  val is_pure_non_rec_asrt : t -> bool
  (** Check if [a] is a pure assertion & non-recursive assertion.
   It assumes that only pure assertions are universally quantified *)

  val make_pure : t -> Formula.t
  (** Eliminate LStar and LTypes assertions.
   LTypes disappears. LStar is replaced by LAnd.
   This function expects its argument to be a PURE assertion. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printer *)

  val star : t list -> t
  (** [star \[a1; a2; ...; an\] will return \[a1 * a2 * ... * an\]] *)

  val subst_clocs : (string -> Expr.t) -> t -> t
  (** [subst_clocs subst a] Substitutes expressions of the form [Lit (Loc l)] with [subst l] in [a] *)
end

module SLCmd : sig
  (** {b GIL Separation-Logic Commands} *)
  type t =
    | Fold      of string
                   * Expr.t list
                   * (string * (string * Expr.t) list) option
        (** Fold predicate *)
    | Unfold    of
        string * Expr.t list * (string * (string * Expr.t) list) option * bool
        (** Unfold predicate *)
    | GUnfold   of string  (** Global Unfold *)
    | ApplyLem  of string * Expr.t list * string list  (** Apply lemma *)
    | SepAssert of Asrt.t * string list  (** Assert *)
    | Invariant of Asrt.t * string list  (** Invariant *)

  val map :
    (t -> t) option ->
    (Asrt.t -> Asrt.t) option ->
    (Expr.t -> Expr.t) option ->
    t ->
    t
  (** Deprecated. Use {!Visitors.map} instead *)

  val pp_folding_info : (string * (string * Expr.t) list) option Fmt.t
  (** Pretty-printer of folding info *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printer *)
end

module LCmd : sig
  (** {b GIL Logical Commands} *)
  type t =
    | If         of Expr.t * t list * t list  (** If-then-else *)
    | Branch     of Formula.t  (** Branching on a FO formual *)
    | Macro      of string * Expr.t list  (** Macros *)
    | Assert     of Formula.t  (** Assert *)
    | Assume     of Formula.t  (** Assume *)
    | AssumeType of string * Type.t  (** Assume Type *)
    | SpecVar    of string list  (** Specification variables (spec vars) *)
    | SL         of SLCmd.t  (** Separation-logic-related commands ({!type:SLCmd.t}) *)

  val map :
    (t -> t) option ->
    (Expr.t -> Expr.t) option ->
    (Formula.t -> Formula.t) option ->
    (SLCmd.t -> SLCmd.t) option ->
    t ->
    t
  (** Deprecated. Use {!Visitors} instead *)

  val pp : t Fmt.t
  (** Pretty-printer *)
end

module Cmd : sig
  (** {b GIL Commands} *)

  (** Optional bindings for procedure calls *)
  type logic_bindings_t = string * (string * Expr.t) list

  type 'label t =
    | Skip  (** Skip *)
    | Assignment    of string * Expr.t  (** Variable Assignment *)
    | LAction       of string * string * Expr.t list  (** Action *)
    | Logic         of LCmd.t  (** Logic commands *)
    | Goto          of 'label  (** Unconditional goto *)
    | GuardedGoto   of Expr.t * 'label * 'label  (** Conditional goto *)
    | Call          of
        string * Expr.t * Expr.t list * 'label option * logic_bindings_t option
        (** Procedure call *)
    | ECall         of string * Expr.t * Expr.t list * 'label option
        (** External Procedure call *)
    | Apply         of string * Expr.t * 'label option
        (** Application-style procedure call *)
    | Arguments     of string  (** Arguments of the currently executing function *)
    | PhiAssignment of (string * Expr.t list) list  (** PHI-assignment *)
    | ReturnNormal  (** Normal return *)
    | ReturnError  (** Error return *)
    | Fail          of string * Expr.t list  (** Failure *)

  val pp : pp_label:'a Fmt.t -> Format.formatter -> 'a t -> unit
  (** Pretty-printer *)

  val pp_labeled : Format.formatter -> string t -> unit
  (** Pretty-printer for labelled programs *)

  val pp_indexed : Format.formatter -> int t -> unit
  (** Pretty-printer for integer-indexed programs *)

  val successors : int t -> int -> int list
  (** Possible successors of an command (in integer indexing) *)
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
    pred_definitions : ((string * string list) option * Asrt.t) list;
        (** Predicate definitions *)
    pred_pure : bool;  (** Is the predicate pure? *)
    pred_normalised : bool;  (** Has the predicate been previously normalised? *)
  }

  val init : t list -> (string, t) Hashtbl.t
  (** Populates a Hashtbl from the given predicate list *)

  val ins_and_outs : t -> Utils.Containers.SI.t * Utils.Containers.SI.t
  (** Returns the sets of in- and out-parameters of a predicate *)

  val in_params : t -> string list
  (** Returns the names of in-parameters *)

  val in_args : t -> 'a list -> 'a list
  (** Returns the in-parameters given all parameters *)

  val out_params : t -> string list
  (** Returns the names of in-parameters *)

  val out_args : t -> 'a list -> 'a list
  (** Returns the out-parameters given all parameters *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printer *)

  val check_pvars : (string, t) Hashtbl.t -> unit
  (** Sanity check on program variables inside normalised predicates *)

  val explicit_param_types : (string, t) Hashtbl.t -> t -> t
  (** Infers parameter types and makes them explicit in the assertions *)

  val combine_ins_outs : t -> 'a list -> 'a list -> 'a list
  (** Combines a list of ins and a list of outs putting them in the right order
    according to a given predicate. *)

  val iter_ins_outs :
    t -> ('a -> unit) -> ('b -> unit) -> 'a list * 'b list -> unit
  (** [iter_ins_outs p f_ins f_outs (ins, outs)] will iterate, applying [f_ins] on the [ins] and
    [f_outs] on the [outs], in the order specified *)

  val pp_ins_outs :
    t ->
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    Format.formatter ->
    'a list * 'b list ->
    unit
  (** Prints the ins and outs in the right order *)

  val get : (string, t) Hashtbl.t -> string -> t
  (** Retrieves a predicate definition by name *)
end

module Lemma : sig
  (** {b GIL Lemmas} *)
  type t = {
    lemma_name : string;  (** Name *)
    lemma_params : string list;  (** Parameters *)
    lemma_hyp : Asrt.t;  (** Hypothesis *)
    lemma_concs : Asrt.t list;  (** Conclusion *)
    lemma_proof : LCmd.t list option;  (** (Optional) Proof *)
    lemma_variant : Expr.t option;  (** Variant *)
    lemma_existentials : string list; (* Existentials *)
  }

  val pp : Format.formatter -> t -> unit
  (** Pretty-printer *)

  val parameter_types : (string, Pred.t) Hashtbl.t -> t -> t
  (** Infers types of parameters and adds them to the contained assertions *)
end

module Macro : sig
  (** {b GIL Macros } *)
  type t = {
    macro_name : string;  (** Name of the macro *)
    macro_params : string list;  (** Actual parameters *)
    macro_definition : LCmd.t list;  (** Macro definition *)
  }

  val pp : Format.formatter -> t -> unit
  (** Pretty-printer *)

  val pp_tbl : (string, t) Hashtbl.t Fmt.t
  (** Table pretty-printer *)

  val get : (string, t) Hashtbl.t -> string -> t option
  (** Retrieves a macro definition by name *)
end

module Flag : sig
  (** {b Return flags for GIL specifications}. *)

  type t = Normal  (** Normal return *) | Error  (** Error return *)

  val str : t -> string

  val pp : t Fmt.t

  module Set : Set.S with type elt := t
end

module Spec : sig
  (** {b GIL specifications}. *)
  type st = {
    ss_pre : Asrt.t;  (** Precondition *)
    ss_posts : Asrt.t list;  (** Postcondition *)
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
    spec_to_verify : bool;  (** Should the spec be verified? *)
  }

  val s_init :
    ?ss_label:string * string list ->
    Asrt.t ->
    Asrt.t list ->
    Flag.t ->
    bool ->
    st
  (** [s_init ~ss_label ss_pre ss_posts ss_flag ss_to_verify] creates a single specification with the given values *)

  val init : string -> string list -> st list -> bool -> bool -> t
  (** [init spec_name spec_params spec_sspecs spec_normalised spec_to_verify] creates a full specification with the given values *)

  val extend : t -> st list -> t
  (** Extends a full specfiication with a single specification *)

  val get_params : t -> string list
  (** Return the list of parameters of a Spec *)

  val pp_sspec : Format.formatter -> st -> unit

  val pp : Format.formatter -> t -> unit

  val parameter_types : (string, Pred.t) Hashtbl.t -> t -> t
  (** Makes the types of parameters explicit in the assertions *)

  val label_vars_to_set :
    ('a * Utils.Containers.SS.elt list) option ->
    ('a * Utils.Containers.SS.t) option
  (** For legacy purpose, some functions use string sets instead of string lists existentials.
    This function allows for a smooth translation *)
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

  val pp : Format.formatter -> t -> unit
  (** Pretty-printer *)
end

module Annot : sig
  (** {b GIL annot}. *)
  type t

  val init : ?line_offset:int option -> ?origin_id:int -> unit -> t
  (** Initialize an annotation *)

  val get_line_offset : t -> int option
  (** get the line offset *)

  val line_info_to_str : (string * int * int) list -> string
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

  val get_params : ('a, 'b) t -> string list
  (** Gets the parameters of the procedure *)

  val pp :
    show_labels:bool ->
    pp_label:'a Fmt.t ->
    Format.formatter ->
    ('b, 'a) t ->
    unit
  (** If the [show_labels] flag is true, the labels will be written before the command they correspond to *)

  val pp_labeled : Format.formatter -> ('a, string) t -> unit
  (** Print labelled *)

  val pp_indexed : Format.formatter -> ('a, int) t -> unit
  (** Print indexed *)

  val line_info : (Annot.t, 'a) t -> (string * int * int) list
  (** Line information *)

  val indexed_of_labeled : (Annot.t, string) t -> (Annot.t, int) t
  (** Returns the indexed procedure for a labeled procedures where the labels can be of any type.
    Equality of labels is decided by structural equality *)

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
  (** Makes a full program *)

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
  (** Initialises a labeled program (with empty predecessors, to be computed later) *)

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
  (** Initialises an indexed program (with empty proc_names and imports, useless for the rest) *)

  val create : unit -> ('a, string) t
  (** Creates an empty program *)

  (** {3 Getters} *)
  val get_lemmas : ('a, 'b) t -> Lemma.t list
  (** Get all lemmas *)

  val get_preds : ('a, 'b) t -> Pred.t list
  (** Get all predicates *)

  val get_ospecs : ('a, 'b) t -> Spec.t list
  (** Get all only-specs *)

  val get_specs : ('a, 'b) t -> Spec.t list
  (** Get all specs *)

  val get_procs : ?proc_names:string list -> ('a, 'b) t -> ('a, 'b) Proc.t list
  (** Get all procedures *)

  val get_bispecs : ('a, 'b) t -> BiSpec.t list
  (** Get all bi-abductive specs *)

  val get_noninternal_proc_names : ('a, 'b) t -> string list
  (** Get names of all procedures not marked as internal *)

  val get_proc : ('a, 'b) t -> string -> ('a, 'b) Proc.t option
  (** Get a specific procedure *)

  val get_proc_exn : ('a, 'b) t -> string -> ('a, 'b) Proc.t
  (** Get a specific procedure *)

  val get_pred : ('a, 'b) t -> string -> Pred.t option
  (** Get a specific procedure *)

  val get_pred_exn : ('a, 'b) t -> string -> Pred.t
  (** Get a specific procedure *)

  val get_bispec : ('a, 'b) t -> string -> BiSpec.t option
  (** Get a specific bi-abductive spec *)

  val get_lemma : ('a, 'b) t -> string -> Lemma.t
  (** Get a specific lemma *)

  val get_proc_specs : ('a, 'b) t -> Spec.t list
  (** Get all specifications *)

  (** {3 Setters} *)

  val update_specs : ('a, 'b) t -> ('c, 'd) t -> unit
  (** Add specs *)

  val update_imports : ('a, 'b) t -> (string * bool) list -> ('a, 'b) t
  (** Add imports *)

  val add_lemma : ('a, 'b) t -> Lemma.t -> ('a, 'b) t
  (** Add a lemma *)

  val add_pred : ('a, 'b) t -> Pred.t -> ('a, 'b) t
  (** Add a predicate *)

  val add_ospec : ('a, 'b) t -> Spec.t -> ('a, 'b) t
  (** Add an only-spec *)

  val add_proc : ('a, 'b) t -> ('a, 'b) Proc.t -> ('a, 'b) t
  (** Add a proc *)

  val add_macro : ('a, 'b) t -> Macro.t -> ('a, 'b) t
  (** Add a macro *)

  val add_bispec : ('a, 'b) t -> BiSpec.t -> ('a, 'b) t
  (** Add a bi-abductive spec *)

  val pp :
    show_labels:bool ->
    pp_label:'b Fmt.t ->
    Format.formatter ->
    ('a, 'b) t ->
    unit
  (** {3 Printers} *)

  val pp_labeled : Format.formatter -> ('a, string) t -> unit
  (** Print labelled *)

  val pp_indexed : Format.formatter -> ('a, int) t -> unit
  (** Print indexed *)

  val line_info : (Annot.t, 'b) t -> (string * int * int) list
  (** Line information *)
end

module Visitors : sig
  class ['c] map :
    object ('c)
      constraint
      'c = < visit_'annot : 'd -> 'g -> 'g
           ; visit_'label : 'd -> 'h -> 'h
           ; visit_ALoc : 'd -> ALoc.t -> Expr.t
           ; visit_And : 'd -> Formula.t -> Formula.t -> Formula.t
           ; visit_Apply : 'd -> string -> Expr.t -> 'h option -> 'h Cmd.t
           ; visit_ApplyLem :
               'd -> string -> Expr.t list -> string list -> SLCmd.t
           ; visit_Arguments : 'd -> string -> 'h Cmd.t
           ; visit_Assert : 'd -> Formula.t -> LCmd.t
           ; visit_Assignment : 'd -> string -> Expr.t -> 'h Cmd.t
           ; visit_Assume : 'd -> Formula.t -> LCmd.t
           ; visit_AssumeType : 'd -> string -> Type.t -> LCmd.t
           ; visit_BAnd : 'd -> BinOp.t
           ; visit_BOr : 'd -> BinOp.t
           ; visit_BSetMem : 'd -> BinOp.t
           ; visit_BSetSub : 'd -> BinOp.t
           ; visit_BinOp : 'd -> Expr.t -> BinOp.t -> Expr.t -> Expr.t
           ; visit_BitwiseAnd : 'd -> BinOp.t
           ; visit_BitwiseAndL : 'd -> BinOp.t
           ; visit_BitwiseNot : 'd -> UnOp.t
           ; visit_BitwiseOr : 'd -> BinOp.t
           ; visit_BitwiseOrL : 'd -> BinOp.t
           ; visit_BitwiseXor : 'd -> BinOp.t
           ; visit_BitwiseXorL : 'd -> BinOp.t
           ; visit_Bool : 'd -> bool -> Literal.t
           ; visit_BooleanType : 'd -> Type.t
           ; visit_Branch : 'd -> Formula.t -> LCmd.t
           ; visit_Call :
               'd ->
               string ->
               Expr.t ->
               Expr.t list ->
               'h option ->
               (string * (string * Expr.t) list) option ->
               'h Cmd.t
           ; visit_Car : 'd -> UnOp.t
           ; visit_Cdr : 'd -> UnOp.t
           ; visit_Constant : 'd -> Constant.t -> Literal.t
           ; visit_IDiv : 'd -> BinOp.t
           ; visit_FDiv : 'd -> BinOp.t
           ; visit_ECall :
               'd -> string -> Expr.t -> Expr.t list -> 'h option -> 'h Cmd.t
           ; visit_EList : 'd -> Expr.t list -> Expr.t
           ; visit_ESet : 'd -> Expr.t list -> Expr.t
           ; visit_Emp : 'd -> Asrt.t
           ; visit_Empty : 'd -> Literal.t
           ; visit_EmptyType : 'd -> Type.t
           ; visit_Epsilon : 'd -> Constant.t
           ; visit_Eq : 'd -> Expr.t -> Expr.t -> Formula.t
           ; visit_Equal : 'd -> BinOp.t
           ; visit_Error : 'd -> Flag.t
           ; visit_Fail : 'd -> string -> Expr.t list -> 'h Cmd.t
           ; visit_False : 'd -> Formula.t
           ; visit_Fold :
               'd ->
               string ->
               Expr.t list ->
               (string * (string * Expr.t) list) option ->
               SLCmd.t
           ; visit_ForAll :
               'd -> (string * Type.t option) list -> Formula.t -> Formula.t
           ; visit_GA : 'd -> string -> Expr.t list -> Expr.t list -> Asrt.t
           ; visit_GUnfold : 'd -> string -> SLCmd.t
           ; visit_Goto : 'd -> 'h -> 'h Cmd.t
           ; visit_GuardedGoto : 'd -> Expr.t -> 'h -> 'h -> 'h Cmd.t
           ; visit_If : 'd -> Expr.t -> LCmd.t list -> LCmd.t list -> LCmd.t
           ; visit_Invariant : 'd -> Asrt.t -> string list -> SLCmd.t
           ; visit_LAction : 'd -> string -> string -> Expr.t list -> 'h Cmd.t
           ; visit_LList : 'd -> Literal.t list -> Literal.t
           ; visit_LVar : 'd -> LVar.t -> Expr.t
           ; visit_LeftShift : 'd -> BinOp.t
           ; visit_LeftShiftL : 'd -> BinOp.t
           ; visit_Less : 'd -> Expr.t -> Expr.t -> Formula.t
           ; visit_LessEq : 'd -> Expr.t -> Expr.t -> Formula.t
           ; visit_ILessThan : 'd -> BinOp.t
           ; visit_ILessThanEqual : 'd -> BinOp.t
           ; visit_FLessThan : 'd -> BinOp.t
           ; visit_FLessThanEqual : 'd -> BinOp.t
           ; visit_SLessThan : 'd -> BinOp.t
           ; visit_ListType : 'd -> Type.t
           ; visit_Lit : 'd -> Literal.t -> Expr.t
           ; visit_Loc : 'd -> string -> Literal.t
           ; visit_LocalTime : 'd -> Constant.t
           ; visit_Logic : 'd -> LCmd.t -> 'h Cmd.t
           ; visit_LstCat : 'd -> NOp.t
           ; visit_LstLen : 'd -> UnOp.t
           ; visit_LstNth : 'd -> BinOp.t
           ; visit_LstRev : 'd -> UnOp.t
           ; visit_LstSub : 'd -> Expr.t -> Expr.t -> Expr.t -> Expr.t
           ; visit_M_abs : 'd -> UnOp.t
           ; visit_M_acos : 'd -> UnOp.t
           ; visit_M_asin : 'd -> UnOp.t
           ; visit_M_atan : 'd -> UnOp.t
           ; visit_M_atan2 : 'd -> BinOp.t
           ; visit_M_ceil : 'd -> UnOp.t
           ; visit_M_cos : 'd -> UnOp.t
           ; visit_M_exp : 'd -> UnOp.t
           ; visit_M_floor : 'd -> UnOp.t
           ; visit_M_isNaN : 'd -> UnOp.t
           ; visit_M_log : 'd -> UnOp.t
           ; visit_M_pow : 'd -> BinOp.t
           ; visit_M_round : 'd -> UnOp.t
           ; visit_M_sgn : 'd -> UnOp.t
           ; visit_M_sin : 'd -> UnOp.t
           ; visit_M_sqrt : 'd -> UnOp.t
           ; visit_M_tan : 'd -> UnOp.t
           ; visit_Macro : 'd -> string -> Expr.t list -> LCmd.t
           ; visit_Max_float : 'd -> Constant.t
           ; visit_MaxSafeInteger : 'd -> Constant.t
           ; visit_Min_float : 'd -> Constant.t
           ; visit_IMinus : 'd -> BinOp.t
           ; visit_FMinus : 'd -> BinOp.t
           ; visit_IMod : 'd -> BinOp.t
           ; visit_FMod : 'd -> BinOp.t
           ; visit_NOp : 'd -> NOp.t -> Expr.t list -> Expr.t
           ; visit_NoneType : 'd -> Type.t
           ; visit_Nono : 'd -> Literal.t
           ; visit_Normal : 'd -> Flag.t
           ; visit_Not : 'd -> Formula.t -> Formula.t
           ; visit_Null : 'd -> Literal.t
           ; visit_NullType : 'd -> Type.t
           ; visit_Int : 'd -> int -> Literal.t
           ; visit_Num : 'd -> float -> Literal.t
           ; visit_IntType : 'd -> Type.t
           ; visit_NumberType : 'd -> Type.t
           ; visit_ObjectType : 'd -> Type.t
           ; visit_Or : 'd -> Formula.t -> Formula.t -> Formula.t
           ; visit_PVar : 'd -> string -> Expr.t
           ; visit_PhiAssignment : 'd -> (string * Expr.t list) list -> 'h Cmd.t
           ; visit_Pi : 'd -> Constant.t
           ; visit_IPlus : 'd -> BinOp.t
           ; visit_FPlus : 'd -> BinOp.t
           ; visit_Pred : 'd -> string -> Expr.t list -> Asrt.t
           ; visit_Pure : 'd -> Formula.t -> Asrt.t
           ; visit_Random : 'd -> Constant.t
           ; visit_ReturnError : 'd -> 'h Cmd.t
           ; visit_ReturnNormal : 'd -> 'h Cmd.t
           ; visit_SL : 'd -> SLCmd.t -> LCmd.t
           ; visit_SepAssert : 'd -> Asrt.t -> string list -> SLCmd.t
           ; visit_SetDiff : 'd -> BinOp.t
           ; visit_SetInter : 'd -> NOp.t
           ; visit_SetMem : 'd -> Expr.t -> Expr.t -> Formula.t
           ; visit_SetSub : 'd -> Expr.t -> Expr.t -> Formula.t
           ; visit_SetToList : 'd -> UnOp.t
           ; visit_SetType : 'd -> Type.t
           ; visit_SetUnion : 'd -> NOp.t
           ; visit_SignedRightShift : 'd -> BinOp.t
           ; visit_SignedRightShiftL : 'd -> BinOp.t
           ; visit_Skip : 'd -> 'h Cmd.t
           ; visit_SpecVar : 'd -> string list -> LCmd.t
           ; visit_Star : 'd -> Asrt.t -> Asrt.t -> Asrt.t
           ; visit_StrCat : 'd -> BinOp.t
           ; visit_StrLen : 'd -> UnOp.t
           ; visit_StrLess : 'd -> Expr.t -> Expr.t -> Formula.t
           ; visit_StrNth : 'd -> BinOp.t
           ; visit_String : 'd -> string -> Literal.t
           ; visit_StringType : 'd -> Type.t
           ; visit_ITimes : 'd -> BinOp.t
           ; visit_FTimes : 'd -> BinOp.t
           ; visit_ToInt32Op : 'd -> UnOp.t
           ; visit_ToIntOp : 'd -> UnOp.t
           ; visit_ToNumberOp : 'd -> UnOp.t
           ; visit_ToStringOp : 'd -> UnOp.t
           ; visit_ToUint16Op : 'd -> UnOp.t
           ; visit_ToUint32Op : 'd -> UnOp.t
           ; visit_True : 'd -> Formula.t
           ; visit_Type : 'd -> Type.t -> Literal.t
           ; visit_TypeOf : 'd -> UnOp.t
           ; visit_TypeType : 'd -> Type.t
           ; visit_Types : 'd -> (Expr.t * Type.t) list -> Asrt.t
           ; visit_UNot : 'd -> UnOp.t
           ; visit_UTCTime : 'd -> Constant.t
           ; visit_UnOp : 'd -> UnOp.t -> Expr.t -> Expr.t
           ; visit_IUnaryMinus : 'd -> UnOp.t
           ; visit_FUnaryMinus : 'd -> UnOp.t
           ; visit_Undefined : 'd -> Literal.t
           ; visit_UndefinedType : 'd -> Type.t
           ; visit_Unfold :
               'd ->
               string ->
               Expr.t list ->
               (string * (string * Expr.t) list) option ->
               bool ->
               SLCmd.t
           ; visit_UnsignedRightShift : 'd -> BinOp.t
           ; visit_UnsignedRightShiftL : 'd -> BinOp.t
           ; visit_assertion : 'd -> Asrt.t -> Asrt.t
           ; visit_bindings :
               'd ->
               string * (string * Expr.t) list ->
               string * (string * Expr.t) list
           ; visit_binop : 'd -> BinOp.t -> BinOp.t
           ; visit_bispec : 'd -> BiSpec.t -> BiSpec.t
           ; visit_cmd : 'd -> 'h Cmd.t -> 'h Cmd.t
           ; visit_constant : 'd -> Constant.t -> Constant.t
           ; visit_expr : 'd -> Expr.t -> Expr.t
           ; visit_flag : 'd -> Flag.t -> Flag.t
           ; visit_formula : 'd -> Formula.t -> Formula.t
           ; visit_lcmd : 'd -> LCmd.t -> LCmd.t
           ; visit_lemma : 'd -> Lemma.t -> Lemma.t
           ; visit_literal : 'd -> Literal.t -> Literal.t
           ; visit_macro : 'd -> Macro.t -> Macro.t
           ; visit_nop : 'd -> NOp.t -> NOp.t
           ; visit_pred : 'd -> Pred.t -> Pred.t
           ; visit_proc : 'd -> ('g, 'h) Proc.t -> ('g, 'h) Proc.t
           ; visit_single_spec : 'd -> Spec.st -> Spec.st
           ; visit_slcmd : 'd -> SLCmd.t -> SLCmd.t
           ; visit_spec : 'd -> Spec.t -> Spec.t
           ; visit_typ : 'd -> Type.t -> Type.t
           ; visit_unop : 'd -> UnOp.t -> UnOp.t
           ; .. >

      method visit_'annot : 'd -> 'g -> 'g

      method visit_'label : 'd -> 'h -> 'h

      method visit_ALoc : 'd -> ALoc.t -> Expr.t

      method visit_And : 'd -> Formula.t -> Formula.t -> Formula.t

      method visit_Apply : 'd -> string -> Expr.t -> 'h option -> 'h Cmd.t

      method visit_ApplyLem :
        'd -> string -> Expr.t list -> string list -> SLCmd.t

      method visit_Arguments : 'd -> string -> 'h Cmd.t

      method visit_Assert : 'd -> Formula.t -> LCmd.t

      method visit_Assignment : 'd -> string -> Expr.t -> 'h Cmd.t

      method visit_Assume : 'd -> Formula.t -> LCmd.t

      method visit_AssumeType : 'd -> string -> Type.t -> LCmd.t

      method visit_BAnd : 'd -> BinOp.t

      method visit_BOr : 'd -> BinOp.t

      method visit_BSetMem : 'd -> BinOp.t

      method visit_BSetSub : 'd -> BinOp.t

      method visit_BinOp : 'd -> Expr.t -> BinOp.t -> Expr.t -> Expr.t

      method visit_BitwiseAnd : 'd -> BinOp.t

      method visit_BitwiseAndL : 'd -> BinOp.t

      method visit_BitwiseNot : 'd -> UnOp.t

      method visit_BitwiseOr : 'd -> BinOp.t

      method visit_BitwiseOrL : 'd -> BinOp.t

      method visit_BitwiseXor : 'd -> BinOp.t

      method visit_BitwiseXorL : 'd -> BinOp.t

      method visit_Bool : 'd -> bool -> Literal.t

      method visit_BooleanType : 'd -> Type.t

      method visit_Branch : 'd -> Formula.t -> LCmd.t

      method visit_Call :
        'd ->
        string ->
        Expr.t ->
        Expr.t list ->
        'h option ->
        (string * (string * Expr.t) list) option ->
        'h Cmd.t

      method visit_Car : 'd -> UnOp.t

      method visit_Cdr : 'd -> UnOp.t

      method visit_Constant : 'd -> Constant.t -> Literal.t

      method visit_IDiv : 'd -> BinOp.t

      method visit_FDiv : 'd -> BinOp.t

      method visit_ECall :
        'd -> string -> Expr.t -> Expr.t list -> 'h option -> 'h Cmd.t

      method visit_EList : 'd -> Expr.t list -> Expr.t

      method visit_Epsilon : 'd -> Constant.t

      method visit_ESet : 'd -> Expr.t list -> Expr.t

      method visit_Emp : 'd -> Asrt.t

      method visit_Empty : 'd -> Literal.t

      method visit_EmptyType : 'd -> Type.t

      method visit_Epsilon : 'd -> Constant.t

      method visit_Eq : 'd -> Expr.t -> Expr.t -> Formula.t

      method visit_Equal : 'd -> BinOp.t

      method visit_Error : 'd -> Flag.t

      method visit_Fail : 'd -> string -> Expr.t list -> 'h Cmd.t

      method visit_False : 'd -> Formula.t

      method visit_Fold :
        'd ->
        string ->
        Expr.t list ->
        (string * (string * Expr.t) list) option ->
        SLCmd.t

      method visit_ForAll :
        'd -> (string * Type.t option) list -> Formula.t -> Formula.t

      method visit_GA : 'd -> string -> Expr.t list -> Expr.t list -> Asrt.t

      method visit_GUnfold : 'd -> string -> SLCmd.t

      method visit_Goto : 'd -> 'h -> 'h Cmd.t

      method visit_GuardedGoto : 'd -> Expr.t -> 'h -> 'h -> 'h Cmd.t

      method visit_If : 'd -> Expr.t -> LCmd.t list -> LCmd.t list -> LCmd.t

      method visit_Invariant : 'd -> Asrt.t -> string list -> SLCmd.t

      method visit_LAction : 'd -> string -> string -> Expr.t list -> 'h Cmd.t

      method visit_LList : 'd -> Literal.t list -> Literal.t

      method visit_LVar : 'd -> LVar.t -> Expr.t

      method visit_LeftShift : 'd -> BinOp.t

      method visit_LeftShiftL : 'd -> BinOp.t

      method visit_Less : 'd -> Expr.t -> Expr.t -> Formula.t

      method visit_LessEq : 'd -> Expr.t -> Expr.t -> Formula.t

      method visit_ILessThan : 'd -> BinOp.t

      method visit_ILessThanEqual : 'd -> BinOp.t

      method visit_FLessThan : 'd -> BinOp.t

      method visit_FLessThanEqual : 'd -> BinOp.t

      method visit_SLessThan : 'd -> BinOp.t

      method visit_ListType : 'd -> Type.t

      method visit_Lit : 'd -> Literal.t -> Expr.t

      method visit_Loc : 'd -> string -> Literal.t

      method visit_LocalTime : 'd -> Constant.t

      method visit_Logic : 'd -> LCmd.t -> 'h Cmd.t

      method visit_LstCat : 'd -> NOp.t

      method visit_LstLen : 'd -> UnOp.t

      method visit_LstNth : 'd -> BinOp.t

      method visit_LstRev : 'd -> UnOp.t

      method visit_LstSub : 'd -> Expr.t -> Expr.t -> Expr.t -> Expr.t

      method visit_M_abs : 'd -> UnOp.t

      method visit_M_acos : 'd -> UnOp.t

      method visit_M_asin : 'd -> UnOp.t

      method visit_M_atan : 'd -> UnOp.t

      method visit_M_atan2 : 'd -> BinOp.t

      method visit_M_ceil : 'd -> UnOp.t

      method visit_M_cos : 'd -> UnOp.t

      method visit_M_exp : 'd -> UnOp.t

      method visit_M_floor : 'd -> UnOp.t

      method visit_M_isNaN : 'd -> UnOp.t

      method visit_M_log : 'd -> UnOp.t

      method visit_M_pow : 'd -> BinOp.t

      method visit_M_round : 'd -> UnOp.t

      method visit_M_sgn : 'd -> UnOp.t

      method visit_M_sin : 'd -> UnOp.t

      method visit_M_sqrt : 'd -> UnOp.t

      method visit_M_tan : 'd -> UnOp.t

      method visit_Macro : 'd -> string -> Expr.t list -> LCmd.t

      method visit_Max_float : 'd -> Constant.t

      method visit_MaxSafeInteger : 'd -> Constant.t

      method visit_Min_float : 'd -> Constant.t

      method visit_IMinus : 'd -> BinOp.t

      method visit_FMinus : 'd -> BinOp.t

      method visit_IMod : 'd -> BinOp.t

      method visit_FMod : 'd -> BinOp.t

      method visit_NOp : 'd -> NOp.t -> Expr.t list -> Expr.t

      method visit_NoneType : 'd -> Type.t

      method visit_Nono : 'd -> Literal.t

      method visit_Normal : 'd -> Flag.t

      method visit_Not : 'd -> Formula.t -> Formula.t

      method visit_Null : 'd -> Literal.t

      method visit_NullType : 'd -> Type.t

      method visit_Int : 'd -> int -> Literal.t

      method visit_Num : 'd -> float -> Literal.t

      method visit_IntType : 'd -> Type.t

      method visit_NumberType : 'd -> Type.t

      method visit_ObjectType : 'd -> Type.t

      method visit_Or : 'd -> Formula.t -> Formula.t -> Formula.t

      method visit_PVar : 'd -> string -> Expr.t

      method visit_PhiAssignment : 'd -> (string * Expr.t list) list -> 'h Cmd.t

      method visit_Pi : 'd -> Constant.t

      method visit_IPlus : 'd -> BinOp.t

      method visit_FPlus : 'd -> BinOp.t

      method visit_Pred : 'd -> string -> Expr.t list -> Asrt.t

      method visit_Pure : 'd -> Formula.t -> Asrt.t

      method visit_Random : 'd -> Constant.t

      method visit_ReturnError : 'd -> 'h Cmd.t

      method visit_ReturnNormal : 'd -> 'h Cmd.t

      method visit_SL : 'd -> SLCmd.t -> LCmd.t

      method visit_SepAssert : 'd -> Asrt.t -> string list -> SLCmd.t

      method visit_SetDiff : 'd -> BinOp.t

      method visit_SetInter : 'd -> NOp.t

      method visit_SetMem : 'd -> Expr.t -> Expr.t -> Formula.t

      method visit_SetSub : 'd -> Expr.t -> Expr.t -> Formula.t

      method visit_SetToList : 'd -> UnOp.t

      method visit_SetType : 'd -> Type.t

      method visit_SetUnion : 'd -> NOp.t

      method visit_SignedRightShift : 'd -> BinOp.t

      method visit_SignedRightShiftL : 'd -> BinOp.t

      method visit_Skip : 'd -> 'h Cmd.t

      method visit_SpecVar : 'd -> string list -> LCmd.t

      method visit_Star : 'd -> Asrt.t -> Asrt.t -> Asrt.t

      method visit_StrCat : 'd -> BinOp.t

      method visit_StrLen : 'd -> UnOp.t

      method visit_StrLess : 'd -> Expr.t -> Expr.t -> Formula.t

      method visit_StrNth : 'd -> BinOp.t

      method visit_String : 'd -> string -> Literal.t

      method visit_StringType : 'd -> Type.t

      method visit_ITimes : 'd -> BinOp.t

      method visit_FTimes : 'd -> BinOp.t

      method visit_ToInt32Op : 'd -> UnOp.t

      method visit_ToIntOp : 'd -> UnOp.t

      method visit_ToNumberOp : 'd -> UnOp.t

      method visit_ToStringOp : 'd -> UnOp.t

      method visit_ToUint16Op : 'd -> UnOp.t

      method visit_ToUint32Op : 'd -> UnOp.t

      method visit_True : 'd -> Formula.t

      method visit_Type : 'd -> Type.t -> Literal.t

      method visit_TypeOf : 'd -> UnOp.t

      method visit_TypeType : 'd -> Type.t

      method visit_Types : 'd -> (Expr.t * Type.t) list -> Asrt.t

      method visit_UNot : 'd -> UnOp.t

      method visit_UTCTime : 'd -> Constant.t

      method visit_UnOp : 'd -> UnOp.t -> Expr.t -> Expr.t

      method visit_IUnaryMinus : 'd -> UnOp.t

      method visit_FUnaryMinus : 'd -> UnOp.t

      method visit_Undefined : 'd -> Literal.t

      method visit_UndefinedType : 'd -> Type.t

      method visit_Unfold :
        'd ->
        string ->
        Expr.t list ->
        (string * (string * Expr.t) list) option ->
        bool ->
        SLCmd.t

      method visit_UnsignedRightShift : 'd -> BinOp.t

      method visit_UnsignedRightShiftL : 'd -> BinOp.t

      method visit_assertion : 'd -> Asrt.t -> Asrt.t

      method visit_bindings :
        'd -> string * (string * Expr.t) list -> string * (string * Expr.t) list

      method visit_binop : 'd -> BinOp.t -> BinOp.t

      method visit_bispec : 'd -> BiSpec.t -> BiSpec.t

      method visit_cmd : 'd -> 'h Cmd.t -> 'h Cmd.t

      method visit_constant : 'd -> Constant.t -> Constant.t

      method visit_expr : 'd -> Expr.t -> Expr.t

      method visit_flag : 'd -> Flag.t -> Flag.t

      method visit_formula : 'd -> Formula.t -> Formula.t

      method visit_lcmd : 'd -> LCmd.t -> LCmd.t

      method visit_lemma : 'd -> Lemma.t -> Lemma.t

      method visit_literal : 'd -> Literal.t -> Literal.t

      method visit_macro : 'd -> Macro.t -> Macro.t

      method visit_nop : 'd -> NOp.t -> NOp.t

      method visit_pred : 'd -> Pred.t -> Pred.t

      method visit_proc : 'd -> ('g, 'h) Proc.t -> ('g, 'h) Proc.t

      method visit_single_spec : 'd -> Spec.st -> Spec.st

      method visit_slcmd : 'd -> SLCmd.t -> SLCmd.t

      method visit_spec : 'd -> Spec.t -> Spec.t

      method visit_typ : 'd -> Type.t -> Type.t

      method visit_unop : 'd -> UnOp.t -> UnOp.t
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
           ; visit_AssumeType : 'c -> string -> Type.t -> 'f
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
           ; visit_Less : 'c -> Expr.t -> Expr.t -> 'f
           ; visit_LessEq : 'c -> Expr.t -> Expr.t -> 'f
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
           ; visit_Int : 'c -> int -> 'f
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
           ; visit_SpecVar : 'c -> string list -> 'f
           ; visit_Star : 'c -> Asrt.t -> Asrt.t -> 'f
           ; visit_StrCat : 'c -> 'f
           ; visit_StrLen : 'c -> 'f
           ; visit_StrLess : 'c -> Expr.t -> Expr.t -> 'f
           ; visit_StrNth : 'c -> 'f
           ; visit_String : 'c -> string -> 'f
           ; visit_StringType : 'c -> 'f
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
               (string * (string * Expr.t) list) option ->
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

      method visit_AssumeType : 'c -> string -> Type.t -> 'f

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

      method visit_Less : 'c -> Expr.t -> Expr.t -> 'f

      method visit_LessEq : 'c -> Expr.t -> Expr.t -> 'f

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

      method visit_Int : 'c -> int -> 'f

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

      method visit_SpecVar : 'c -> string list -> 'f

      method visit_Star : 'c -> Asrt.t -> Asrt.t -> 'f

      method visit_StrCat : 'c -> 'f

      method visit_StrLen : 'c -> 'f

      method visit_StrLess : 'c -> Expr.t -> Expr.t -> 'f

      method visit_StrNth : 'c -> 'f

      method visit_String : 'c -> string -> 'f

      method visit_StringType : 'c -> 'f

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
        (string * (string * Expr.t) list) option ->
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

  module Utils : sig
    module SS = Containers.SS

    class list_monoid :
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
