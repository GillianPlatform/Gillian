/**
    @pred Map (+m, mp, kvs, keys) :
      JSObjWithProto(m, mp) *
      DataProp(m, "_contents", #c) * JSObject(#c) *
      ((m, "get") -> none) * ((m, "put") -> none) * ((m, "validKey") -> none) *
      ((#c, "hasOwnProperty") -> none) * KVPairs(#c, kvs, keys) * empty_fields(#c : -u- (keys, -{ "hasOwnProperty" }-));
    
    @pred KVPairs (+o, kvs : Set, keys : Set) :
      [def1]  (kvs == -{ }-) * (keys == -{ }-),
      [def2: #key]  
              (kvs == -u- (-{ {{ #key, #value }} }-, #rkvs)) * (keys == -u- (-{ #key }-, #rkeys)) * (! (#key --e-- #rkeys)) * 
              ValidKey(#key) * DataProp(o, #key, #value) * KVPairs(o, #rkvs, #rkeys) *
              types (#rkvs: Set, #rkeys: Set);
    
    @pred MapProto(mp) :
      JSObject(mp) *
      DataProp(mp, "get", #get_loc) * JSFunctionObject(#get_loc, "mapGet", _, _, _) *
      DataProp(mp, "put", #put_loc) * JSFunctionObject(#put_loc, "mapPut", _, _, _) *
      DataProp(mp, "validKey", #vK_loc) * JSFunctionObject(#vK_loc, "isValidKey", _, _, _) *
      ((mp, "_contents") -> none);

    @onlyspec map()
      [[
        initialHeapPostWeak() * 
        JSObjWithProto(this, #mp) *
        ((this, "_contents") -> none) *
        ((this, "get") -> none) *
        ((this, "put") -> none) *
        ((this, "validKey") -> none) *
        MapProto(#mp)
      ]] 
      [[ 
        initialHeapPostWeak() * 
        Map(this, #mp, #kvs, #keys) * (#kvs == -{ }-) * (#keys == -{ }-) * 
        MapProto(#mp) * (ret == this)
      ]]
      normal

    @onlyspec mapGet(k)
      <invalid_key> 
      [[ (k == #k) * Map(this, #mp, #kvs, #keys) * MapProto(#mp) * InvalidKey(#k) * GlobalObject() * ObjectPrototype() * BI_ErrorObject() ]] 
      [[ Map(this, #mp, #kvs, #keys) * MapProto(#mp) * ErrorObjectWithMessage(ret, "Invalid Key") * GlobalObject() * ObjectPrototype() * BI_ErrorObject() ]] 
      error; 

      <inexistent_key> 
      [[ (k == #k) * Map(this, #mp, #kvs, #keys) * MapProto(#mp) * ValidKey(#k) * (! (#k --e-- #keys)) * GlobalObject() * ObjectPrototype() ]]
      [[ Map(this, #mp, #kvs, #keys) * MapProto(#mp) * (ret == null) * GlobalObject() * ObjectPrototype() ]]
      normal;

      <existent_key: #v> 
      [[ (k == #k) * Map(this, #mp, #kvs, #keys) * MapProto(#mp) * ValidKey(#k) * (#k --e-- #keys) * ({{ #k, #v }} --e-- #kvs) * GlobalObject() * ObjectPrototype() ]]
      [[ Map(this, #mp, #kvs, #keys) * MapProto(#mp) * (ret == #v) * GlobalObject() * ObjectPrototype() ]]
      normal

    @onlyspec isValidKey(key)
      [[ ((key == #key) * ValidKey(#key)) ]]
      [[ (ret == true) ]] 
      normal; 
    
      [[ ((key == #key) * InvalidKey(#key)) ]] 
      [[ (ret == false) ]] 
      normal

    @onlyspec mapPut(k, v)
      <invalid_key>
      [[ (k == #k) * (v == #v) * Map(this, #mp, #kvs, #keys) * MapProto(#mp) * InvalidKey(#k) * GlobalObject() * ObjectPrototype() * BI_ErrorObject() ]] 
      [[ Map(this, #mp, #kvs, #keys) * MapProto(#mp) * ErrorObjectWithMessage(ret, "Invalid Key") * GlobalObject() * ObjectPrototype() * BI_ErrorObject() ]] 
      error; 

      <inexistent_key>
      [[ (k == #k) * (v == #v) * Map(this, #mp, #kvs, #keys) * MapProto(#mp) * ValidKey(#k) * (! (#k --e-- #keys)) * GlobalObject() * ObjectPrototype() ]] 
      [[ Map(this, #mp, -u- (-{ {{ #k, #v }} }-, #kvs), -u- (-{ #k }-, #keys)) * MapProto(#mp) * GlobalObject() * ObjectPrototype() ]] 
      normal; 

      <existent_key: #rkvs, #w>
      [[ (k == #k) * (v == #v) * Map(this, #mp, #kvs, #keys) * MapProto(#mp) * ValidKey(#k) * (#k --e-- #keys) * 
       (#kvs == -u- (-{ {{ #k, #w }} }-, #rkvs)) * GlobalObject() * ObjectPrototype() ]] 
      [[ Map(this, #mp, -u- (-{ {{ #k, #v }} }-, #rkvs), #keys) * MapProto(#mp) * GlobalObject() * ObjectPrototype() ]] 
      normal
*/

/**  
  @pred ValidKey(key) : 
    types(key : Str) * (! (key == "hasOwnProperty"));
    
  @pred InvalidKey(key) :
    types (key : Undefined),
    types (key : Null),
    types (key : Bool),
    types (key : Num),
    types (key : Str) * (key == "hasOwnProperty");
*/

/* ********************  *
 *                       *
 *      INTERPRETER      *
 *                       *
 * *********************** */

"use strict";

/* 
  @id evalUnop

  @pre  ((op == "-") * (l == #l) * types(#l : Num))
  @post (ret == -#l)

  @pre  ((op == "not") * (l == #l) * types(#l : Bool))
  @post (ret == not #l)
*/
function evalUnop (op, l) { 

  switch (op) {
   case "-"   : return -l;
   case "not" : return !l;
   
   default : throw new Error ("Unsupported unary operator")
  }
}

/* 
  @id evalBinop

  @pre  ((op == "+") * (l1 == #l1) * (l2 == #l2) * types(#l1 : Num) * types(#l2 : Num))
  @post (ret == #l1 + #l2)

  @pre  ((op == "-") * (l1 == #l1) * (l2 == #l2) * types(#l1 : Num) * types(#l2 : Num))
  @post (ret == #l1 - #l2)

  @pre  ((op == "or") * (l1 == #l1) * (l2 == #l2) * types(#l1 : Bool) * types(#l2 : Bool))
  @post (ret == #l1 or #l2)

  @pre  ((op == "and") * (l1 == #l1) * (l2 == #l2) * types(#l1 : Bool) * types(#l2 : Bool))
  @post (ret == #l1 and #l2)
*/
function evalBinop (op, l1, l2) { 

  switch (op) { 
    case "+"   : return l1 + l2;
    case "-"   : return l1 - l2;
    case "or"  : return l1 || l2;
    case "and" : return l1 && l2;

    default : throw new Error("Unsupported binary operator")
  }
}

/*
  @pred ValidVal(v) : 
    types(v : Str), 
    types(v : Bool);  

  @pred ExpressionStruct(+e:Obj, cat:Str) : 
    JSObject(e) * DataProp(e, "type", "expr") * DataProp(e, "category", cat);

  @pred ExpressionWithValue(+e:Obj, v, +bindings:Set, +vars:Set) : 
    ExpressionStruct(e, "lit") * DataProp(e, "val", v) * types(v : Num),
    ExpressionStruct(e, "lit") * DataProp(e, "val", v) * types(v : Bool),
 
    [var: #v] ExpressionStruct(e, "var") * DataProp(e, "name",  #var_name) * (v == #v) * 
      ValidKey(#var_name) * ({{ #var_name, #v }} --e-- bindings) * (#var_name --e-- vars) * ValidVal(#v), 

    ExpressionStruct(e, "unop") * DataProp(e, "op",  "-") * DataProp(e, "arg", #arg) * 
      ExpressionWithValue(#arg, #varg, bindings, vars) * types(#varg : Num) * (v == -#varg),
    ExpressionStruct(e, "unop") * DataProp(e, "op",  "not") * DataProp(e, "arg", #arg) * 
      ExpressionWithValue(#arg, #varg, bindings, vars) * types(#varg : Bool) * (v == not #varg),   

    ExpressionStruct(e, "binop") * DataProp(e, "op",  "+") * DataProp(e, "left", #left) * DataProp(e, "right", #right) * 
      ExpressionWithValue(#left, #vleft, bindings, vars) * ExpressionWithValue(#right, #vright, bindings, vars) * 
      types(#vleft : Num) * types(#vright : Num) * (v == #vleft + #vright),
    ExpressionStruct(e, "binop") * DataProp(e, "op",  "-") * DataProp(e, "left", #left) * DataProp(e, "right", #right) * 
      ExpressionWithValue(#left, #vleft, bindings, vars) * ExpressionWithValue(#right, #vright, bindings, vars) * 
      types(#vleft : Num) * types(#vright : Num) * (v == #vleft - #vright),
    ExpressionStruct(e, "binop") * DataProp(e, "op",  "and") * DataProp(e, "left", #left) * DataProp(e, "right", #right) * 
      ExpressionWithValue(#left, #vleft, bindings, vars) * ExpressionWithValue(#right, #vright, bindings, vars) * 
      types(#vleft : Bool) * types(#vright : Bool) * (v == #vleft and #vright),
    ExpressionStruct(e, "binop") * DataProp(e, "op",  "or") * DataProp(e, "left", #left) * DataProp(e, "right", #right) * 
      ExpressionWithValue(#left, #vleft, bindings, vars) * ExpressionWithValue(#right, #vright, bindings, vars) * 
      types(#vleft : Bool) * types(#vright : Bool) * (v == #vleft or #vright);
*/

/*
  @id evalExpr

  @pre  GlobalObject() * ObjectPrototype() * 
        scope(evalExpr  : #evalExpr)  * JSFunctionObject(#evalExpr,  "evalExpr",  _, _, _) * 
        scope(evalUnop  : #evalUnop)  * JSFunctionObject(#evalUnop,  "evalUnop",  _, _, _) * 
        scope(evalBinop : #evalBinop) * JSFunctionObject(#evalBinop, "evalBinop", _, _, _) * 
        ((store == #store) * (e == #e) * ExpressionWithValue(#e, #v, #bnds, #vars)) * 
        Map(#store, #mp, #bnds, #vars) * MapProto(#mp)

  @post (GlobalObject() * ObjectPrototype() * 
        scope(evalExpr : #evalExpr) * JSFunctionObject(#evalExpr, "evalExpr", _, _, _) * 
        scope(evalUnop : #evalUnop) * JSFunctionObject(#evalUnop, "evalUnop", _, _, _) * 
        scope(evalBinop : #evalBinop) * JSFunctionObject(#evalBinop, "evalBinop", _, _, _) * 
        ExpressionWithValue(#e, #v, #bnds, #vars) * (ret == #v) * 
        Map(#store, #mp, #bnds, #vars) * MapProto(#mp))
*/
function evalExpr (store, e) {
  switch (e.category) {
    
    case "binop" : return evalBinop (e.op, evalExpr(store, e.left), evalExpr(store, e.right)) 
    case "unop"  : return evalUnop  (e.op, evalExpr(store, e.arg)) 
    case "lit"   : return e.val
    case "var"   : return store.get(e.name)
    default : throw new Error("Unsupported expression")
  }
}