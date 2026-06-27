type t =
  (* Used only for work in compilation *)
  | WList
  | WNull
  | WBool
  | WString
  | WPtr
  | WInt
  | WFloat
  | WAny
  | WSet

(** Are types t1 and t2 compatible *)
let compatible t1 t2 =
  match (t1, t2) with
  | WAny, _ -> true
  | _, WAny -> true
  | t1, t2 when t1 = t2 -> true
  | _ -> false

let strongest t1 t2 =
  match (t1, t2) with
  | WAny, t -> t
  | t, WAny -> t
  | _ -> t1

(* careful there is no strongest for two different types *)

let pp fmt t =
  let s = Format.fprintf fmt "@[%s@]" in
  match t with
  | WList -> s "List"
  | WNull -> s "NullType"
  | WBool -> s "Bool"
  | WString -> s "String"
  | WPtr -> s "Pointer"
  | WInt -> s "Int"
  | WFloat -> s "Float"
  | WAny -> s "Any"
  | WSet -> s "Set"

let to_gil : t -> Gil_syntax.Type.t = function
  | WList -> ListType
  | WInt -> IntType
  | WString -> StringType
  | WBool -> BooleanType
  | WFloat -> NumberType
  | t -> Fmt.failwith "Can't convert type '%a' to GIL!" pp t
