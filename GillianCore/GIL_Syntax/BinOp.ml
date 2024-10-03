(* GIL Binary Operators *)

type t = TypeDef__.binop =
  (* Comparison *)
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
  (* Boolean *)
  | BAnd  (** Boolean conjunction *)
  | BOr  (** Boolean disjunction *)
  | BImpl  (** Boolean implication *)
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
  | UnsignedRightShiftL  (** Right bitshift 64bit *)
  | BitwiseAndF  (** Bitwise conjunction float *)
  | BitwiseOrF  (** Bitwise disjunction float *)
  | BitwiseXorF  (** Bitwise exclusive disjunction float *)
  | LeftShiftF  (** Left bitshift float *)
  | SignedRightShiftF  (** Signed right bitshift float *)
  | UnsignedRightShiftF  (** Unsigned right bitshift float *)
  (* Mathematics *)
  | M_atan2  (** Arctangent y/x *)
  | M_pow  (** Power *)
  (* Lists *)
  | LstNth  (** Nth element of a string *)
  | LstRepeat (* [[a; b]] is the list that contains [b] times the element [a] *)
  (* Strings *)
  | StrCat  (** String concatenation *)
  | StrNth  (** Nth element of a string *)
  (* Sets *)
  | SetDiff  (** Set difference *)
  | BSetMem  (** Set membership *)
  | BSetSub  (** Subset *)
[@@deriving eq, ord]

let to_yojson = TypeDef__.binop_to_yojson
let of_yojson = TypeDef__.binop_of_yojson

let str (x : t) =
  match x with
  | Equal -> "="
  | ILessThan -> "i<"
  | ILessThanEqual -> "i<="
  | IPlus -> "i+"
  | IMinus -> "i-"
  | ITimes -> "i*"
  | IDiv -> "i/"
  | IMod -> "i%"
  | FLessThan -> "<"
  | FLessThanEqual -> "<="
  | FPlus -> "+"
  | FMinus -> "-"
  | FTimes -> "*"
  | FDiv -> "/"
  | FMod -> "%"
  | SLessThan -> "s<"
  | BAnd -> "and"
  | BOr -> "or"
  | BImpl -> "==>"
  | BitwiseAnd -> "&"
  | BitwiseOr -> "|"
  | BitwiseXor -> "^"
  | LeftShift -> "<<"
  | SignedRightShift -> ">>"
  | UnsignedRightShift -> ">>>"
  | BitwiseAndL -> "&l"
  | BitwiseOrL -> "|l"
  | BitwiseXorL -> "^l"
  | LeftShiftL -> "<<l"
  | SignedRightShiftL -> ">>l"
  | UnsignedRightShiftL -> ">>>l"
  | BitwiseAndF -> "&f"
  | BitwiseOrF -> "|f"
  | BitwiseXorF -> "^f"
  | LeftShiftF -> "<<f"
  | SignedRightShiftF -> ">>f"
  | UnsignedRightShiftF -> ">>>f"
  | M_atan2 -> "m_atan2"
  | M_pow -> "**"
  | LstNth -> "l-nth"
  | LstRepeat -> "l-repeat"
  | StrCat -> "++"
  | StrNth -> "s-nth"
  | SetDiff -> "-d-"
  | BSetMem -> "-e-"
  | BSetSub -> "-s-"
