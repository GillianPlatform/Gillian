type t = TypeDef__.constructor = {
  constructor_name : string;
  constructor_source_path : string option;
  constructor_loc : Location.t option;
  constructor_num_fields : int;
  constructor_fields : Type.t option list;
  constructor_datatype : string;
}
[@@deriving yojson]
