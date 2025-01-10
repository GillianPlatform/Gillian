let z_to_yojson z = `String (Z.to_string z)

let z_of_yojson = function
  | `String s -> ( try Ok (Z.of_string s) with Invalid_argument m -> Error m)
  | _ -> Error "Invalid yojson for Z"

type constant =
  | Min_float
  | Max_float
  | MaxSafeInteger
  | Epsilon
  | Random
  | Pi
  | UTCTime
  | LocalTime

and typ =
  | UndefinedType
  | NullType
  | EmptyType
  | NoneType
  | BooleanType
  | IntType
  | NumberType
  | StringType
  | ObjectType
  | ListType
  | TypeType
  | SetType
  | BvType of int

and literal =
  | Undefined
  | Null
  | Empty
  | Constant of constant
  | Bool of bool
  | Int of (Z.t[@opaque] [@to_yojson z_to_yojson] [@of_yojson z_of_yojson])
  | Num of float
  | String of string
  | Loc of string
  | Type of typ
  | LList of literal list
  | LBitvector of
      ((Z.t[@opaque] [@to_yojson z_to_yojson] [@of_yojson z_of_yojson]) * int)
  | Nono

and binop =
  | Equal
  | ILessThan
  | ILessThanEqual
  | IPlus
  | IMinus
  | ITimes
  | IDiv
  | IMod
  | FLessThan
  | FLessThanEqual
  | FPlus
  | FMinus
  | FTimes
  | FDiv
  | FMod
  | And
  | Or
  | Impl
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | LeftShift
  | SignedRightShift
  | UnsignedRightShift
  | BitwiseAndL
  | BitwiseOrL
  | BitwiseXorL
  | LeftShiftL
  | SignedRightShiftL
  | UnsignedRightShiftL
  | BitwiseAndF
  | BitwiseOrF
  | BitwiseXorF
  | LeftShiftF
  | SignedRightShiftF
  | UnsignedRightShiftF
  | M_atan2
  | M_pow
  | LstNth
  | LstRepeat
  | StrCat
  | StrNth
  | StrLess
  | SetDiff
  | SetMem
  | SetSub

and unop =
  | IUnaryMinus
  | FUnaryMinus
  | Not
  | BitwiseNot
  | M_isNaN
  | M_abs
  | M_acos
  | M_asin
  | M_atan
  | M_ceil
  | M_cos
  | M_exp
  | M_floor
  | M_log
  | M_round
  | M_sgn
  | M_sin
  | M_sqrt
  | M_tan
  | ToStringOp
  | ToIntOp
  | ToUint16Op
  | ToUint32Op
  | ToInt32Op
  | ToNumberOp
  | TypeOf
  | Car
  | Cdr
  | LstLen
  | LstRev
  | SetToList
  | StrLen
  | NumToInt
  | IntToNum
  | IsInt

and nop = LstCat | SetUnion | SetInter
and bvpred = BVUlt | BVUMulO | BVSMulO | BVNegO | BVUAddO | BVSAddO

and bvop =
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

and bv_arg = Literal of int | BvExpr of (expr * int)

and expr =
  | Lit of literal
  | PVar of string
  | LVar of string
  | ALoc of string
  | BVExprIntrinsic of bvop * bv_arg list * int
  | UnOp of unop * expr
  | BinOp of expr * binop * expr
  | LstSub of expr * expr * expr
  | NOp of nop * expr list
  | EList of expr list
  | ESet of expr list
  | Exists of (string * typ option) list * expr
  | ForAll of (string * typ option) list * expr

and formula =
  | True
  | False
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Eq of expr * expr
  | Impl of formula * formula
  | FLess of expr * expr
  | FLessEq of expr * expr
  | ILess of expr * expr
  | ILessEq of expr * expr
  | BVFormIntrinsic of bvpred * bv_arg list
  | StrLess of expr * expr
  | SetMem of expr * expr
  | SetSub of expr * expr
  | ForAll of (string * typ option) list * formula
  | IsInt of expr

and assertion =
  | Emp
  | Pred of string * expr list
  | Pure of expr
  | Types of (expr * typ) list
  | CorePred of string * expr list * expr list
  | Wand of { lhs : string * expr list; rhs : string * expr list }

and assertion = assertion_atom list
and bindings = string * (string * expr) list

and slcmd =
  | Fold of string * expr list * bindings option
  | Unfold of string * expr list * (string * string) list option * bool
  | Package of { lhs : string * expr list; rhs : string * expr list }
      (** Magic wand packaging *)
  | GUnfold of string
  | ApplyLem of string * expr list * string list
  | SepAssert of assertion * string list
  | Invariant of assertion * string list
  | Consume of assertion * string list
  | Produce of assertion
  | SymbExec

and lcmd =
  | If of expr * lcmd list * lcmd list
  | Branch of expr
  | Macro of string * expr list
  | Assert of expr
  | Assume of expr
  | AssumeType of expr * typ
  | FreshSVar of string
  | SL of slcmd

and 'label cmd =
  | Skip
  | Assignment of string * expr
  | LAction of string * string * expr list
  | Logic of lcmd
  | Goto of 'label
  | GuardedGoto of expr * 'label * 'label
  | Call of string * expr * expr list * 'label option * bindings option
  | ECall of string * expr * expr list * 'label option
  | Apply of string * expr * 'label option
  | Arguments of string
  | PhiAssignment of (string * expr list) list
  | ReturnNormal
  | ReturnError
  | Fail of string * expr list

and flag = Normal | Error | Bug

and pred = {
  pred_name : string;
  pred_source_path : string option;
  pred_internal : bool;
  pred_num_params : int;
  pred_params : (string * typ option) list;
  pred_ins : int list;
  pred_definitions : ((string * string list) option * assertion) list;
  pred_facts : expr list;
  pred_guard : assertion option;
  pred_pure : bool;
  pred_abstract : bool;
  pred_nounfold : bool;
  pred_normalised : bool;
}

and lemma_spec = {
  lemma_hyp : assertion;
  lemma_concs : assertion list;
  lemma_spec_variant : expr option;
}

and lemma = {
  lemma_name : string;
  lemma_source_path : string option;
  lemma_internal : bool;
  lemma_params : string list;
  lemma_specs : lemma_spec list;
  lemma_proof : lcmd list option;
  lemma_variant : expr option;
  lemma_existentials : string list;
}

and single_spec = {
  ss_pre : assertion;
  ss_posts : assertion list;
  ss_variant : expr option;
  ss_flag : flag;
  ss_to_verify : bool;
  ss_label : (string * string list) option;
}

and spec = {
  spec_name : string;
  spec_params : string list;
  spec_sspecs : single_spec list;
  spec_normalised : bool;
  spec_incomplete : bool;
  spec_to_verify : bool;
}

and bispec = {
  bispec_name : string;
  bispec_params : string list;
  bispec_pres : assertion list;
  bispec_normalised : bool;
}

and macro = {
  macro_name : string;
  macro_params : string list;
  macro_definition : lcmd list;
}

and ('annot, 'label) proc = {
  proc_name : string;
  proc_source_path : string option;
  proc_internal : bool;
  proc_body : ('annot * 'label option * 'label cmd) array;
  proc_params : string list;
  proc_spec : spec option;
  proc_aliases : string list;
  proc_calls : string list;
}
[@@deriving
  visitors { variety = "reduce" },
    visitors { variety = "endo" },
    visitors { variety = "iter" },
    yojson,
    eq]
