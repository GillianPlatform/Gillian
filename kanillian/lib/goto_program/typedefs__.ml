type type_ =
  | Array of type_ * int
  | Bool
  | CInteger of IntType.t
  | Float
  | Double
  | Signedbv of { width : int }
  | Unsignedbv of { width : int }
  | Code of { params : param list; return_type : type_ }
  | Pointer of type_
  | Struct of { components : datatype_component list; tag : string }
  | IncompleteStruct of string
  | StructTag of string
  | Union of { components : datatype_component list; tag : string }
  | UnionTag of string
  | Constructor
  | Empty
  | Vector of { type_ : type_; size : int }

(* | Signedbv of { width : int }
   | Unsignedbv of { width : int } *)
and param = {
  type_ : type_;
  identifier : string option;
  base_name : string option;
}

and datatype_component =
  | Field of { name : string; type_ : type_ }
  | Padding of { name : string; bits : int }
[@@deriving show { with_path = false }, eq]
