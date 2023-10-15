module Binary = struct
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
  [@@deriving show { with_path = false }]
end

module Self = struct
  type t = Postdecrement | Postincrement | Predecrement | Preincrement
  [@@deriving show { with_path = false }]
end

module Unary = struct
  type t =
    | Bitnot  (**  `~self` *)
    | BitReverse  (**  `__builtin_bitreverse<n>(self)` *)
    | Bswap  (**  `__builtin_bswap<n>(self)` *)
    | IsDynamicObject  (**  `__CPROVER_DYNAMIC_OBJECT(self)` *)
    | IsFinite  (**  `isfinite(self)` *)
    | Not  (**  `!self` *)
    | ObjectSize  (**  `__CPROVER_OBJECT_SIZE(self)` *)
    | PointerObject  (**  `__CPROVER_POINTER_OBJECT(self)` *)
    | PointerOffset  (**  `__CPROVER_POINTER_OFFSET(self)` *)
    | Popcount  (**  `__builtin_p opcount(self)` *)
    | CountTrailingZeros of { allow_zero : bool }
        (**  `__builtin_cttz(self)` *)
    | CountLeadingZeros of { allow_zero : bool }  (**  `__builtin_ctlz(self)` *)
    | UnaryMinus  (**  `-self` *)
  [@@deriving show { with_path = false }]
end
