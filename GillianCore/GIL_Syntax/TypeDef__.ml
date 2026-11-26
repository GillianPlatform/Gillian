type position = Location.position = { pos_line : int; pos_column : int }

and location = Location.t = {
  loc_start : position;
  loc_end : position;
  loc_source : string;
}

and constant =
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
  | DatatypeType of string

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
  | Loc of (Id.Loc.t[@opaque])
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

and expr =
  | Lit of literal
  | PVar of (Id.Var.t[@opaque])
  | LVar of (Id.LVar.t[@opaque])
  | ALoc of (Id.ALoc.t[@opaque])
  | UnOp of unop * expr
  | BinOp of expr * binop * expr
  | LstSub of expr * expr * expr
  | NOp of nop * expr list
  | EList of expr list
  | ESet of expr list
  | Exists of ((Id.LVar.t[@opaque]) * typ option) list * expr
  | ForAll of ((Id.LVar.t[@opaque]) * typ option) list * expr
  | ConstructorApp of string * expr list
  | FuncApp of string * expr list
  | Cases of expr * (string * (Id.LVar.t[@opaque]) list * expr) list

and assertion_atom =
  | Emp
  | Pure of expr
  | Types of (expr * typ) list
  | CorePred of string * expr list * expr list

and assertion = assertion_atom list
and bindings = string * ((Id.LVar.t[@opaque]) * expr) list

and slcmd =
  | Fold of string * expr option list * bindings option
  | Unfold of
      string
      * expr option list
      * ((Id.LVar.t[@opaque]) * (Id.LVar.t[@opaque])) list option
      * bool
  | Package of { lhs : string * expr list; rhs : string * expr list }
      (** Magic wand packaging *)
  | GUnfold of string
  | ApplyLem of string * expr list * (Id.Any_var.t[@opaque]) list
  | SepAssert of assertion * (Id.Any_var.t[@opaque]) list
  | Invariant of assertion * (Id.Any_var.t[@opaque]) list
  | Consume of assertion * (Id.Any_var.t[@opaque]) list
  | Produce of assertion
  | SymbExec

and lcmd =
  | If of expr * lcmd list * lcmd list
  | Branch of expr
  | Macro of string * expr list
  | Assert of expr
  | Assume of expr
  | AssumeType of expr * typ
  | FreshSVar of (Id.Var.t[@opaque])
  | SL of slcmd

and function_call = {
  var_name : (Id.Var.t[@opaque]);
  fun_name : expr;
  args : expr list;
  bindings : bindings option;
}

and 'label cmd =
  | Skip
  | Assignment of (Id.Var.t[@opaque]) * expr
  | LAction of (Id.Var.t[@opaque]) * string * expr list
  | Logic of lcmd
  | Goto of 'label
  | GuardedGoto of expr * 'label * 'label
  | Call of function_call * 'label option
  | Par of function_call list
  | ECall of (Id.Var.t[@opaque]) * expr * expr list * 'label option
  | Apply of (Id.Var.t[@opaque]) * expr * 'label option
  | Arguments of (Id.Var.t[@opaque])
  | PhiAssignment of ((Id.Var.t[@opaque]) * expr list) list
  | ReturnNormal
  | ReturnError
  | Fail of string * expr list

and flag = Normal | Error | Bug

and pred = {
  pred_name : string;
  pred_source_path : string option;
  pred_loc : location option;
  pred_internal : bool;
  pred_num_params : int;
  pred_params : ((Id.Var.t[@opaque]) * typ option) list;
  ins_number : int;
  pred_definitions :
    ((string * (Id.LVar.t[@opaque]) list) option * assertion) list;
  pred_facts : expr list;
  pred_guard : assertion option;
  pred_pure : bool;
  pred_abstract : bool;
  pred_nounfold : bool;
}

and lemma_spec = {
  lemma_hyp : assertion * location option;
  lemma_concs : (assertion * location option) list;
  lemma_spec_variant : expr option;
}

and lemma = {
  lemma_name : string;
  lemma_source_path : string option;
  lemma_internal : bool;
  lemma_params : (Id.Var.t[@opaque]) list;
  lemma_specs : lemma_spec list;
  lemma_proof : lcmd list option;
  lemma_variant : expr option;
  lemma_existentials : (Id.LVar.t[@opaque]) list;
  lemma_location : location option;
}

and datatype = {
  datatype_name : string;
  datatype_source_path : string option;
  datatype_loc : location option;
  datatype_constructors : constructor list;
}

and func = {
  func_name : string;
  func_source_path : string option;
  func_loc : location option;
  func_num_params : int;
  func_params : ((Id.LVar.t[@opaque]) * typ option) list;
  func_definition : expr;
}

and constructor = {
  constructor_name : string;
  constructor_source_path : string option;
  constructor_loc : location option;
  constructor_num_fields : int;
  constructor_fields : typ option list;
  constructor_datatype : string;
}

and single_spec = {
  ss_pre : assertion * location option;
  ss_posts : (assertion * location option) list;
  ss_variant : expr option;
  ss_flag : flag;
  ss_to_verify : bool;
  ss_label : (string * (Id.LVar.t[@opaque]) list) option;
}

and spec = {
  spec_name : string;
  spec_params : (Id.Var.t[@opaque]) list;
  spec_sspecs : single_spec list;
  spec_incomplete : bool;
  spec_to_verify : bool;
  spec_location : location option;
}

and bispec = {
  bispec_name : string;
  bispec_params : (Id.Var.t[@opaque]) list;
  bispec_pres : (assertion * location option) list;
}

and macro = {
  macro_name : string;
  macro_params : (Id.Var.t[@opaque]) list;
  macro_definition : lcmd list;
}

and ('annot, 'label) proc = {
  proc_name : string;
  proc_source_path : string option;
  proc_internal : bool;
  proc_body : ('annot * 'label option * 'label cmd) array;
  proc_params : (Id.Var.t[@opaque]) list;
  proc_spec : spec option;
  proc_aliases : string list;
  proc_calls : string list;
  proc_display_name : (string * string) option;
  proc_hidden : bool;
}
[@@deriving
  visitors { variety = "reduce" },
  visitors { variety = "endo" },
  visitors { variety = "iter" },
  yojson,
  eq]
