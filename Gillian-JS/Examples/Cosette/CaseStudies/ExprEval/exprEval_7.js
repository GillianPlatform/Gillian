/* @id evalExpr */
function evalExpr (store, e) {
  if (typeof e !== "object") throw new Error ("Expr"); 
  switch (e.type) {
    case "lit"   : return e.val
    case "var"   : return store[e.name]
    case "unop"  : 
      var arg_v = evalExpr(store, e.arg);  
      return evalUnop (e.op, arg_v) 
    case "binop" : 
      var left_v  = evalExpr(store, e.left); 
      var right_v = evalExpr(store, e.right); 
      return evalBinop (e.op, left_v, right_v) 
    default : throw new Error("E:Type")
  }
}

/* @id evalUnop */
function evalUnop (op, l) { 
  if (typeof op !== "string") throw new Error ("UnOp")
  switch (op) {
   case "-"   : return -l
   case "not" : return !l
   case "abs" : return l < 0 ? -l : l  
   default    : throw new Error ("UnOp")
  }
}

/* @id evalBinop */
function evalBinop (op, l1, l2) { 
  if (typeof op !== "string") throw new Error ("BinOp");
  switch (op) { 
    case "+"   : return l1 + l2
    case "-"   : return l1 - l2
    case "or"  : return l1 || l2
    case "and" : return l1 && l2
    default    : throw new Error("BinOp")
  }
}

/** Test 3 */

var store = Object.create(null); 
var x = symb_string(), y = symb_string(), v = symb();
var lit = { type: "var",  name: x };
store[y] = v;

var ret = evalExpr(store, lit);

Assert(((x = y) and (ret = v)) or (ret = undefined))