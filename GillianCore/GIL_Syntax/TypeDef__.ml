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

and literal =
  | Undefined
  | Null
  | Empty
  | Constant of constant
  | Bool of bool
  | Int of
      (Z.t
      [@opaque]
      [@to_yojson fun z -> `String (Z.to_string z)]
      [@of_yojson
        function
        | `String s -> (
            try Ok (Z.of_string s) with Invalid_argument m -> Error m)
        | _ -> Error "Invalid yojson for Z"])
  | Num of float
  | String of string
  | Loc of string
  | Type of typ
  | LList of literal list
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
  | SLessThan
  | BAnd
  | BOr
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
  | M_atan2
  | M_pow
  | LstNth
  | StrCat
  | StrNth
  | SetDiff
  | BSetMem
  | BSetSub

and unop =
  | IUnaryMinus
  | FUnaryMinus
  | UNot
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

and nop = LstCat | SetUnion | SetInter

and expr =
  | Lit of literal
  | PVar of string
  | LVar of string
  | ALoc of string
  | UnOp of unop * expr
  | BinOp of expr * binop * expr
  | LstSub of expr * expr * expr
  | NOp of nop * expr list
  | EList of expr list
  | ESet of expr list

and formula =
  | True
  | False
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Eq of expr * expr
  | FLess of expr * expr
  | FLessEq of expr * expr
  | ILess of expr * expr
  | ILessEq of expr * expr
  | StrLess of expr * expr
  | SetMem of expr * expr
  | SetSub of expr * expr
  | ForAll of (string * typ option) list * formula

and assertion =
  | Emp
  | Star of assertion * assertion
  | Pred of string * expr list
  | Pure of formula
  | Types of (expr * typ) list
  | GA of string * expr list * expr list

and bindings = string * (string * expr) list

and slcmd =
  | Fold of string * expr list * bindings option
  | Unfold of string * expr list * (string * string) list option * bool
  | GUnfold of string
  | ApplyLem of string * expr list * string list
  | SepAssert of assertion * string list
  | Invariant of assertion * string list
  | SymbExec

and lcmd =
  | If of expr * lcmd list * lcmd list
  | Branch of formula
  | Macro of string * expr list
  | Assert of formula
  | Assume of formula
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

and flag = Normal | Error

and pred = {
  pred_name : string;
  pred_source_path : string option;
  pred_internal : bool;
  pred_num_params : int;
  pred_params : (string * typ option) list;
  pred_ins : int list;
  pred_definitions :
    ((string * string list) option * assertion * string list) list;
  pred_facts : formula list;
  pred_pure : bool;
  pred_abstract : bool;
  pred_nounfold : bool;
  pred_normalised : bool;
}

and lemma_spec = {
  lemma_hyp : assertion;
  lemma_concs : assertion list;
  lemma_spec_variant : expr option;
  lemma_spec_hides : string list option;
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
}
[@@deriving
  visitors { variety = "reduce" },
    visitors { variety = "endo" },
    visitors { variety = "iter" },
    yojson]
