type kind = If_else_kind [@@deriving yojson, eq]
type case = If_else of bool | Unknown [@@deriving yojson]
type t = case * int [@@deriving yojson]
