(* GIL Unary Operators *)

type t = TypeDef__.unop =
  (* Arithmetic *)
  | IUnaryMinus  (** Integer unary minus *)
  | FUnaryMinus  (** Float unary minus *)
  (* Boolean *)
  | UNot  (** Negation *)
  (* Bitwise *)
  | BitwiseNot  (** Bitwise negation *)
  (* Mathematics *)
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
  (* Types *)
  | ToStringOp  (** Converts a number (integer or float) to a string *)
  | ToIntOp  (** Converts a float to an integer *)
  | ToUint16Op  (** Converts an integer to a 16-bit unsigned integer *)
  | ToUint32Op  (** Converts an integer to a 32-bit unsigned integer *)
  | ToInt32Op  (** Converts an integer to a 32-bit signed integer *)
  | ToNumberOp  (** Converts a string to a number *)
  | TypeOf
  (* Lists *)
  | Car  (** Head of a list *)
  | Cdr  (** Tail of a list *)
  | LstLen  (** List length *)
  | LstRev  (** List reverse *)
  | SetToList  (** From set to list *)
  (* Strings *)
  | StrLen  (** String length *)

let str (x : t) =
  match x with
  | IUnaryMinus -> "i-"
  | FUnaryMinus -> "-"
  | UNot        -> "not"
  | BitwiseNot  -> "~"
  | M_isNaN     -> "isNaN"
  | M_abs       -> "m_abs"
  | M_acos      -> "m_acos"
  | M_asin      -> "m_asin"
  | M_atan      -> "m_atan"
  | M_ceil      -> "m_ceil"
  | M_cos       -> "m_cos"
  | M_exp       -> "m_exp"
  | M_floor     -> "m_floor"
  | M_log       -> "m_log"
  | M_round     -> "m_round"
  | M_sgn       -> "m_sgn"
  | M_sin       -> "m_sin"
  | M_sqrt      -> "m_sqrt"
  | M_tan       -> "m_tan"
  | ToStringOp  -> "num_to_string"
  | ToIntOp     -> "num_to_int"
  | ToUint16Op  -> "num_to_uint16"
  | ToInt32Op   -> "num_to_int32"
  | ToUint32Op  -> "num_to_uint32"
  | ToNumberOp  -> "string_to_num"
  | TypeOf      -> "typeOf"
  | Car         -> "car"
  | Cdr         -> "cdr"
  | LstLen      -> "l-len"
  | LstRev      -> "l-rev"
  | StrLen      -> "s-len"
  | SetToList   -> "set_to_list"
