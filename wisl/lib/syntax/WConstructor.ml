type t = {
  constructor_name : string;
  constructor_fields : WType.t list;
  constructor_datatype : string;
  constructor_loc : CodeLoc.t;
  constructor_id : int;
}
