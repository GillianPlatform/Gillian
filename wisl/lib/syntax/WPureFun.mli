type t = {
  pure_fun_name : string;
  pure_fun_params : (string * WType.t option) list;
  pure_fun_definition : WLExpr.t;
  pure_fun_loc : CodeLoc.t;
}
