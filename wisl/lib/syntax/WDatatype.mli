type t = {
  datatype_name : string;
  datatype_constructors : WConstructor.t list;
  datatype_loc : CodeLoc.t;
  datatype_id : int;
}
