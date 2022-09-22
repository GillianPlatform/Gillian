type tt =
  | Val of WVal.t
  | Var of string
  | BinOp of t * WBinOp.t * t
  | UnOp of WUnOp.t * t
  | List of t list
[@@deriving yojson]

and t [@@deriving yojson]

val get : t -> tt
val get_loc : t -> CodeLoc.t
val get_id : t -> int
val make : tt -> CodeLoc.t -> t
val get_by_id : int -> t -> [> `None | `WExpr of t ]
val pp : Format.formatter -> t -> unit
val str : t -> string
