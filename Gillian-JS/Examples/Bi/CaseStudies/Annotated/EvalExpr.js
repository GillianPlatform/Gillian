'use strict';

/* 
  @id a_evalUnop
*/
function a_evalUnop (op, l) { 

  isString(op);

  switch (op) {
   case "-"   : isNumber(l); return -l;
   case "not" : isBool(l); return !l;
   
   default : throw new Error ("Unsupported unary operator")
  }
}

/* 
  @id b_evalBinop
*/
function b_evalBinop (op, l1, l2) { 

  isString(op);

  switch (op) { 
    case "+"   : isNumber(l1); isNumber(l2); return l1 + l2;
    case "-"   : isNumber(l1); isNumber(l2); return l1 - l2;
    case "or"  : isBool(l1); isBool(l2); return l1 || l2;
    case "and" : isBool(l1); isBool(l2); return l1 && l2;

    default : throw new Error("Unsupported binary operator")
  }
}

/*
  @id c_evalExpr
*/
function c_evalExpr (store, e) {

  isNullableObject(e);

  if ((typeof e) !== "object") throw Error ("Illegal Expr!")

  switch (e.category) {
    
    case "binop" : return b_evalBinop (e.op, c_evalExpr(store, e.left), c_evalExpr(store, e.right)) 
    case "unop"  : return a_evalUnop  (e.op, c_evalExpr(store, e.arg)) 
    case "lit"   : return e.val
    case "var"   : isObject(store); return store[e.name];
    
    default : throw new Error("Illegal Expr!")
  }
}