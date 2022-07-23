/* @id evalExpr */
function evalExpr (store, e) {
  if (typeof e !== "object") throw new Error ("Expr"); 
  switch (e.type) {
    case "lit"   : return e.val
    case "var"   : return store.get(e.name)
    case "unop"  : 
      var arg_v = evalExpr(store, e.arg);  
      return evalUnop (e.op, arg_v) 
    case "binop" : 
      var left_v  = evalExpr(store, e.left); 
      var right_v = evalExpr(store, e.right); 
      return evalBinop (e.op, left, right) 
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
  if (typeof e !== "string") throw new Error ("BinOp");
  switch (op) { 
    case "+"   : return l1 + l2
    case "-"   : return l1 - l2
    case "or"  : return l1 || l2
    case "and" : return l1 && l2
    default    : throw new Error("BinOp")
  }
}

/** Test 3 */

var store = {}; 
var n = symb_number(), s = symb_string(); 
var lit = { type: "lit",  val: n }; 
var e   = { type: "unop", op: s, arg: lit}; 
Assume (not (s = "not"));
try {
  var ret = evalExpr(store, e);
  var abs_ret = (n < 0) ? -n : n; 
  Assert (((s = "-") and (ret = -n)) or ((s = "abs") and (ret = abs_ret))) 
} catch (e) { 
  var msg = e.message;
  Assert(msg = "UnOp")
}