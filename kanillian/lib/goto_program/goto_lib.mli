module Gerror : sig
  exception Unexpected_irep of Irep.t option * string
  exception Code_error of Irep.t option * string
end

module Machine_model_parse : sig
  (** Consumes the architecture data from the symtab, and returns the built machine_model. *)
  val consume_from_symtab : Symtab.t -> Machine_model.t
end

module Ops : sig
  module Binary : sig
    type t =
      | And
      | Ashr
      | Bitand
      | Bitor
      | Bitnand
      | Bitxor
      | Div
      | Equal
      | Ge
      | Gt
      | IeeeFloatEqual
      | IeeeFloatNotequal
      | Implies
      | Le
      | Lshr
      | Lt
      | Minus
      | Mod
      | Mult
      | Notequal
      | Or
      | OverflowResultMinus
      | OverflowResultMult
      | OverflowResultPlus
      | OverflowMinus
      | OverflowMult
      | OverflowPlus
      | Plus
      | ROk
      | Rol
      | Ror
      | Shl
      | Xor
    [@@deriving show]
  end

  module Self : sig
    type t = Postdecrement | Postincrement | Predecrement | Preincrement
    [@@deriving show]
  end

  module Unary : sig
    type t =
      | Bitnot
      | BitReverse
      | Bswap
      | IsDynamicObject
      | IsFinite
      | Not
      | ObjectSize
      | PointerObject
      | PointerOffset
      | Popcount
      | CountTrailingZeros of { allow_zero : bool }
      | CountLeadingZeros of { allow_zero : bool }
      | UnaryMinus
    [@@deriving show]
  end
end

module Location : sig
  type t = {
    origin_id : int;
    source : string option;
    line : int option;
    col : int option;
    comment : string option;
  }

  val of_irep : Irep.t -> t
end

module IntType : sig
  type t = I_bool | I_char | I_int | I_size_t | I_ssize_t
  [@@deriving show, eq]

  module Bv_encoding : sig
    type int_type = t
    type t = { signed : bool; width : int }

    val equal : t -> t -> bool
    val encode : machine:Kcommons.Machine_model.t -> int_type -> t
  end

  val which_int_type_opt :
    machine:Kcommons.Machine_model.t -> signed:bool -> width:int -> t option
end

module rec Param : sig
  type t = {
    type_ : Type.t;
    identifier : string option;
    base_name : string option;
  }

  val of_irep : machine:Machine_model.t -> Irep.t -> t
  val pp : Format.formatter -> t -> unit
end

and Datatype_component : sig
  type t =
    | Field of { name : string; type_ : Type.t }
    | Padding of { name : string; bits : int }
end

and Type : sig
  type t =
    | Array of t * int
    | Bool
    | CInteger of IntType.t
    | Float
    | Double
    | Signedbv of { width : int }
    | Unsignedbv of { width : int }
    | Code of { params : Param.t list; return_type : t }
    | Pointer of t
    | Struct of { components : Datatype_component.t list; tag : string }
    | IncompleteStruct of string
    | StructTag of string
    | Union of { components : Datatype_component.t list; tag : string }
    | UnionTag of string
    | Constructor
    | Empty
    | Vector of { type_ : t; size : int }
  [@@deriving show, eq]

  val show_simple : t -> string

  module Overflow_result : sig
    val is_overflow_result : tag_lookup:(string -> t) -> t -> bool

    type field = Result | Overflowed

    val field : string -> field
  end

  val size_of : machine:Machine_model.t -> tag_lookup:(string -> t) -> t -> int

  val offset_struct_field :
    machine:Machine_model.t -> tag_lookup:(string -> t) -> t -> string -> int

  val is_function : t -> bool
  val of_irep : machine:Machine_model.t -> Irep.t -> t
end

module rec Expr : sig
  type value =
    | Array of t list
    | IntConstant of Z.t
    | CBoolConstant of bool
    | BoolConstant of bool
    | PointerConstant of int
    | DoubleConstant of float
    | FloatConstant of float
    | Symbol of string
    | EFunctionCall of { func : t; args : t list }
    | BinOp of { op : Ops.Binary.t; lhs : t; rhs : t }
    | ByteExtract of { e : t; offset : int }
    | Dereference of t
    | EAssign of { lhs : t; rhs : t }
    | UnOp of { op : Ops.Unary.t; e : t }
    | Struct of t list
    | Member of { lhs : t; field : string }
    | AddressOf of t
    | Index of { array : t; index : t }
    | StringConstant of string
    | TypeCast of t
    | If of { cond : t; then_ : t; else_ : t }
    | StatementExpression of Stmt.t list
    | Nondet
    | EUnhandled of Id.t * string

  and t = { value : value; type_ : Type.t; location : Location.t }
  [@@deriving show]

  val pp_full : Format.formatter -> t -> unit
  val as_symbol : t -> string
  val value_of_irep : machine:Machine_model.t -> type_:Type.t -> Irep.t -> value
  val of_irep : machine:Machine_model.t -> Irep.t -> t
end

and Stmt : sig
  type body =
    | Decl of { lhs : Expr.t; value : Expr.t option }
    | SAssign of { lhs : Expr.t; rhs : Expr.t }
    | Assume of { cond : Expr.t }
    | Assert of { cond : Expr.t; property_class : string option }
    | Block of t list
    | Label of string * t list
    | Goto of string
    | SFunctionCall of {
        lhs : Expr.t option;
        func : Expr.t;
        args : Expr.t list;
      }
    | Switch of {
        control : Expr.t;
        cases : switch_case list;
        default : t option;
      }
    | Ifthenelse of { guard : Expr.t; then_ : t; else_ : t option }
    | Break
    | Skip
    | Expression of Expr.t
    | Output of { msg : Expr.t; value : Expr.t }
    | Return of Expr.t option
    | SUnhandled of Id.t

  and switch_case = { case : Expr.t; sw_body : t }
  and t = { stmt_location : Location.t; body : body; comment : string option }

  val pp : Format.formatter -> t -> unit
  val body_of_irep : machine:Machine_model.t -> Irep.t -> body
  val of_irep : machine:Machine_model.t -> Irep.t -> t
end

module Program : sig
  module Global_var : sig
    type t = {
      type_ : Type.t;
      symbol : string;
      value : Expr.t option;
      location : Location.t;
    }
  end

  module Func : sig
    type t = {
      params : Param.t list;
      body : Stmt.t option;
      return_type : Type.t;
      location : Location.t;
      symbol : string;
    }
  end

  type t = {
    vars : (string, Global_var.t) Hashtbl.t;
    funs : (string, Func.t) Hashtbl.t;
    types : (string, Type.t) Hashtbl.t;
    constrs : (string, unit) Hashtbl.t;
  }

  val of_symtab : machine:Machine_model.t -> Symtab.t -> t
  val fold_functions : (string -> Func.t -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_variables : (string -> Global_var.t -> 'a -> 'a) -> t -> 'a -> 'a
  val is_zst : prog:t -> machine:Machine_model.t -> Type.t -> bool
end

module Visitors : sig
  class ['a] iter :
    object
      method visit_location : ctx:'a -> Location.t -> unit
      method visit_binop : ctx:'a -> Ops.Binary.t -> unit
      method visit_unop : ctx:'a -> Ops.Unary.t -> unit
      method visit_selfop : ctx:'a -> Ops.Self.t -> unit
      method visit_int_type : ctx:'a -> IntType.t -> unit
      method visit_datatype_components : ctx:'a -> Datatype_component.t -> unit
      method visit_type : ctx:'a -> Type.t -> unit
      method visit_expr_value : ctx:'a -> type_:Type.t -> Expr.value -> unit
      method visit_expr : ctx:'a -> Expr.t -> unit
      method visit_stmt_body : ctx:'a -> Stmt.body -> unit
      method visit_stmt : ctx:'a -> Stmt.t -> unit
    end

  class ['a] map :
    object
      method visit_binop : ctx:'a -> Ops.Binary.t -> Ops.Binary.t

      method visit_datatype_components :
        ctx:'a -> Datatype_component.t -> Datatype_component.t

      method visit_expr : ctx:'a -> Expr.t -> Expr.t

      method visit_expr_value :
        ctx:'a -> type_:Type.t -> Expr.value -> Expr.value

      method visit_int_type : ctx:'a -> IntType.t -> IntType.t
      method visit_location : ctx:'a -> Location.t -> Location.t
      method visit_selfop : ctx:'a -> Ops.Self.t -> Ops.Self.t
      method visit_stmt : ctx:'a -> Stmt.t -> Stmt.t
      method visit_stmt_body : ctx:'a -> Stmt.body -> Stmt.body
      method visit_type : ctx:'a -> Type.t -> Type.t
      method visit_unop : ctx:'a -> Ops.Unary.t -> Ops.Unary.t
    end
end
