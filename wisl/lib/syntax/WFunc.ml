type t = {
  func_name : string;
  func_params : (string * WType.t option) list;
  func_definition : WLExpr.t;
  func_loc : CodeLoc.t;
}
