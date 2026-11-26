type t = TypeDef__.func = {
  func_name : string;
  func_source_path : string option;
  func_loc : Location.t option;
  func_num_params : int;
  func_params : (Id.LVar.t * Type.t option) list;
  func_definition : Expr.t;
}
