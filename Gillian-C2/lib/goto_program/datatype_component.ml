type t = Typedefs__.datatype_component =
  | Field of { name : string; type_ : Typedefs__.type_ }
  | Padding of { name : string; bits : int }
