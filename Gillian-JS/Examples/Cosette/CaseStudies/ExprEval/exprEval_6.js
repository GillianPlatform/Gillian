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

var store = {}; 
var b1 = symb_bool(), b2 = symb_bool(), op = symb_string(); 
var l1 = { type: "lit",  val: b1 }, l2 = { type: "lit",  val: b2 }; 
var e   = { type: "binop", op: op, left: l1, right: l2}; 
Assume ((not (op = "+")) and (not (op = "-")));
try {
  var ret = evalExpr(store, e);
  Assert (((op = "and") and (ret = (b1 and b2))) or ((op = "or") and (ret = (b1 or b2))))
} catch (e) { 
  var msg = e.message;
  Assert(msg = "BinOp")
}