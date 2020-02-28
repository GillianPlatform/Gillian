/** Jose: works
  Bug specs:  0
  Succ specs: 10 
 */ 

/* @id hasProp */
function hasProp (o, p) {
  return Object.prototype.hasOwnProperty.call(o, p)
}

/* @id wf_expr */
function wf_expr (e) { 
  var te = typeof e; Assume ((te = "object") and (not (e = null)));
  var hastype = hasProp(e, "type"); Assume (hastype); 
  switch (e.type) {
    case "var" : 
      var hasname = hasProp(e, "name"); Assume (hasname);
      var typeofname = (typeof e.name); Assume (typeofname = "string")
      return true
    case "literal": 
      var hasvalue = hasProp(e, "value"); Assume(hasvalue); 
      var typeofvalue = typeof e.value; 
      Assume ((typeofvalue = "number") or (typeofvalue = "boolean"));
      return true
    
    case "unop": 
      var hasarg = hasProp(e, "arg"); Assume (hasarg);
      var hasoperator = hasProp(e, "operator"); Assume (hasoperator);
      var expr_wf = wf_expr (e.arg); Assume (expr_wf); 
      return true 

     case "binop":
      var hasleft = hasProp(e, "left"); Assume (hasleft);
      var hasright = hasProp(e, "right"); Assume (hasright);
      var hasoperator = hasProp(e, "operator"); Assume (hasoperator);
      var left_expr_wf  = wf_expr (e.left); Assume (left_expr_wf)
      return true

    default: Assume (false) 

  }
}