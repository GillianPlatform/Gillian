type t = TypeDef__.datatype = {
  datatype_name : string;
  datatype_source_path : string option;
  datatype_loc : Location.t option;
  datatype_constructors : Constructor.t list;
}
[@@deriving yojson]
