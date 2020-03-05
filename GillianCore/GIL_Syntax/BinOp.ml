(* GIL Binary Operators *)

type t = TypeDef__.binop =
  (* Comparison *)
  | Equal  (** Equality *)
  | LessThan  (** Less *)
  | LessThanEqual  (** Less or equal for numbers *)
  | LessThanString  (** Less or equal for strings *)
  (* Int Arithmetic *)
  | IPlus (* Integer addition *)
  (* Real Arithmetic *)
  | FPlus  (** Float addition *)
  | Minus  (** Subtraction *)
  | Times  (** Multiplication *)
  | Div  (** Float division *)
  | Mod  (** Modulus *)
  (* Boolean *)
  | BAnd  (** Boolean conjunction *)
  | BOr  (** Boolean disjunction *)
  (* Bitwise *)
  | BitwiseAnd  (** Bitwise conjunction *)
  | BitwiseOr  (** Bitwise disjunction *)
  | BitwiseXor  (** Bitwise exclusive disjunction *)
  | LeftShift  (** Left bitshift *)
  | SignedRightShift  (** Signed right bitshift *)
  | UnsignedRightShift  (** Unsigned right bitshift *)
  (* Bitwise 64bits *)
  | BitwiseAndL  (** Bitwise conjunction 64bit *)
  | BitwiseOrL  (** Bitwise disjunction 64bit *)
  | BitwiseXorL  (** Bitwise exclusive disjunction 64bit *)
  | LeftShiftL  (** Left bitshift 64bit *)
  | SignedRightShiftL  (** Signed right bitshift 64bit *)
  | UnsignedRightShiftL  (** Unsigned right bitshift 64bit *)
  (* Mathematics *)
  | M_atan2  (** Arctangent y/x *)
  | M_pow  (** Power *)
  (* Lists *)
  | LstNth  (** Nth element of a string *)
  (* Strings *)
  | StrCat  (** String concatenation *)
  | StrNth  (** Nth element of a string *)
  (* Sets *)
  | SetDiff  (** Set difference *)
  | BSetMem  (** Set membership *)
  | BSetSub  (** Subset *)

let str (x : t) =
  match x with
  | Equal               -> "="
  | LessThan            -> "<"
  | LessThanEqual       -> "<="
  | LessThanString      -> "<s"
  | IPlus               -> "i+"
  | FPlus               -> "f+"
  | Minus               -> "-"
  | Times               -> "*"
  | Div                 -> "/"
  | Mod                 -> "%"
  | BAnd                -> "and"
  | BOr                 -> "or"
  | BitwiseAnd          -> "&"
  | BitwiseOr           -> "|"
  | BitwiseXor          -> "^"
  | LeftShift           -> "<<"
  | SignedRightShift    -> ">>"
  | UnsignedRightShift  -> ">>>"
  | BitwiseAndL         -> "&l"
  | BitwiseOrL          -> "|l"
  | BitwiseXorL         -> "^l"
  | LeftShiftL          -> "<<l"
  | SignedRightShiftL   -> ">>l"
  | UnsignedRightShiftL -> ">>>l"
  | M_atan2             -> "m_atan2"
  | M_pow               -> "**"
  | LstNth              -> "l-nth"
  | StrCat              -> "++"
  | StrNth              -> "s-nth"
  | SetDiff             -> "-d-"
  | BSetMem             -> "-e-"
  | BSetSub             -> "-s-"
