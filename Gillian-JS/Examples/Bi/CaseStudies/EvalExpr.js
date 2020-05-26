'use strict';

/*
  @id a_evalUnop
*/
function a_evalUnop (op, l) {

  switch (op) {
   case "-"   : /* Annotation */ isNumber(l); return -l;
   case "not" : /* Annotation */ isBool(l);   return !l;

   default : throw new Error ("Unsupported unary operator")
  }
}

/*
  @id b_evalBinop
*/
function b_evalBinop (op, l1, l2) {

  switch (op) {
    case "+"   : /* Annotation */ isNumber(l1); /* Annotation */ isNumber(l2); return l1 + l2;
    case "-"   : /* Annotation */ isNumber(l1); /* Annotation */ isNumber(l2); return l1 - l2;
    case "or"  : /* Annotation */ isBool(l1);   /* Annotation */ isBool(l2);   return l1 || l2;
    case "and" : /* Annotation */ isBool(l1);   /* Annotation */ isBool(l2);   return l1 && l2;

    default : throw new Error("Unsupported binary operator")
  }
}

/*
  @id c_evalExpr
*/
function c_evalExpr (store, e) {

  if ((typeof e) !== "object") throw Error ("Unsupported expression");

  switch (e.category) {

    case "lit"   : return e.val;
    case "var"   : return store[e.name];
    case "binop" : return b_evalBinop (e.op, c_evalExpr(store, e.left), c_evalExpr(store, e.right))
    case "unop"  : return a_evalUnop  (e.op, c_evalExpr(store, e.arg))

    default : throw new Error("Unsupported expression")
  }
}