(**
	GIL Types
*)

type t = TypeDef__.typ =
  | UndefinedType  (** Type of Undefined      *)
  | NullType  (** Type of Null           *)
  | EmptyType  (** Type of Empty          *)
  | NoneType  (** Type of logical values *)
  | BooleanType  (** Type of booleans       *)
  | IntType  (** Type of integers *)
  | NumberType  (** Type of floats         *)
  | StringType  (** Type of strings        *)
  | ObjectType  (** Type of objects        *)
  | ListType  (** Type of lists          *)
  | TypeType  (** Type of types          *)
  | SetType  (** Type of sets           *)
  [@@deriving yojson]

(** Print *)
let str (x : t) =
  match x with
  | UndefinedType -> "Undefined"
  | NullType      -> "Null"
  | EmptyType     -> "Empty"
  | NoneType      -> "None"
  | BooleanType   -> "Bool"
  | IntType       -> "Int"
  | NumberType    -> "Num"
  | StringType    -> "Str"
  | ObjectType    -> "Obj"
  | ListType      -> "List"
  | TypeType      -> "Type"
  | SetType       -> "Set"

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)
