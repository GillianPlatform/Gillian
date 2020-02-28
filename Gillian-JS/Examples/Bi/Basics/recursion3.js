/** Jose: works
  Bug specs:  
    - eval_expr: 1
  Succ specs: 
    - hasProp: 1 
    - eval_binop: 8 (2 user errors, 2 or, 2 and, 1 +, 1 -)
    - eval_unop: 4 (2 user errors, 1 -, 1 not)
    - eval_expr: 131 (101 user errors, 1 lit, 1 var, 4 unop, 24 binop)

    - why 30 normal specs for eval_expr 
        - 1 for the literal case
        - 1 for the var case 
        - 4 for the unop case (lit num, var num, lit bool, var bool)
        - 24 for the binop case 
           the binop case uses the eval_binop function. this function has 6 normal specs,
           4 of them require two booleans and 2 require two numbers. We have 4 different 
           ways of constructing numbers using the specs before and four different ways 
           of constructing booleans, resulting in: 4*4 + 4*2 = 24 
 */ 

/* @id hasProp */
function hasProp (o, p) {
  return Object.prototype.hasOwnProperty.call(o, p)
}

/** @id eval_binop */
function eval_binop (op, v1, v2) { 
  var tv1 = typeof v1, tv2 = typeof v2;
  var test = ((tv1 === "number") || (tv1 === "boolean")) && (tv1 === tv2); 
  Assume (test); 
  switch (op) { 
    case "+"   : Assume (tv1 = "number");  return v1 +  v2
    case "-"   : Assume (tv1 = "number");  return v1 -  v2
    case "or"  : Assume (tv1 = "boolean"); return v1 || v2
    case "and" : Assume (tv1 = "boolean"); return v1 && v2
    default    : throw new Error("Unsupported binary operator")
  }
}

/** @id eval_unop */
function eval_unop (op, v) { 
   var tv1 = typeof v;
   var test = (tv1 === "number") || (tv1 === "boolean"); 
   Assume(test); 
   switch (op) {
     case "-"   : Assume (tv1 = "number"); return -v 
     case "not" : Assume (tv1 = "boolean"); return !v 
     default  : throw new Error ("Unsupported unary operator")
   }
}

/* @id eval_expr */
function eval_expr (store, e) { 
  if ((typeof e) !== "object") throw Error ("Illegal Expr!")
  switch (e.type) {
    case "var" : 
      if (hasProp(store, e.name)) {
        return store[e.name];
      } else { 
        throw Error ("Var not in store");
      }

    case "literal": return e.value; 
      
    case  "unop": return eval_unop(e.operator, eval_expr (store, e.arg));
    case "binop": return eval_binop(e.operator, eval_expr (store, e.left), eval_expr (store, e.right));

    default: throw Error ("Illegal Expr!")

  }
}