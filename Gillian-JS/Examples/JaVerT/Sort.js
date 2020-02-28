"use strict";

/**

  ###
  ### Nullable objects are either objects or null
  ###
  @pred nullableObject(o) : 
    [nOb01] types(o : Obj),
    [nOb02] (o == null);

  ###
  ### A node is an object with a property 'value' (number)
  ### and a pointer to the next node 'next' (nullableObject)
  ###
  ### In-parameters: n
  ###
  @pred Node(+n:Obj, v:Num, t):
    JSObject(n) *
    DataProp(n, "value", v) *
    DataProp(n, "next", t);
  
  ###
  ### A nodelist is a singly-linked list of nodes 
  ### starting from nl (nullableObject) and
  ### carrying the SET of values of that list (E)
  ###
  ### In-parameters: nl
  ###
  @pred NDList(+nl, E:Set):
    (nl == null) * (E == -{ }-),

    Node(nl, #v, #t) * NDList(#t, #tE) *
    (E == -u- (#tE, -{ #v }-)) *
    (! (#v --e-- #tE));

  ###
  ### A sorted list is a singly-linked list of nodes 
  ### starting from nl (nullableObject),
  ### with values sorted in ascending order,
  ### carrying the SET of values of that list (E),
  ###
  ### In-parameters: nl
  ###
  @pred SOList(+nl, E:Set):
    (nl == null) * (E == -{ }-),

    Node(nl, #v, #t) * SOList(#t, #tE) *
    (E == -u- (#tE, -{ #v }-)) *
    (forall #x:Num. ((! (#x --e-- #tE)) \/ (#v <# #x)));
 */
 
/**
	@id insert

	@pre (JSObject ($lg) * (node == #n) * (value == #v) * SOList(#n, #E) * types(#v: Num) * 
		 scope(insert: #insert_fun) * JSFunctionObject(#insert_fun, "insert", #insert_sc, #insert_len, #insert_proto))
	@post (JSObject ($lg) * (ret == #ret) * SOList(#ret, -u- (-{ #v }-, #E)) * types(#ret: Obj) *
		 scope(insert: #insert_fun) * JSFunctionObject(#insert_fun, "insert", #insert_sc, #insert_len, #insert_proto))
*/
function insert(node, value) {
    
    if (node === null) {
        return { next: null, value: value }
    } else if (node.value === value) {
        return node;
    } else if (node.value < value) {
        var rec = insert(node.next, value);
        return { next: rec, value: node.value }
    } else {
        return { next: node, value: value }
    }
}

/**
	@id sort

	@pre (JSObject ($lg) * (head == #h) * NDList(#h, #E) * 
		  scope(sort: #sort_fun) * JSFunctionObject(#sort_fun, "sort", #sort_sc, #sort_len, #sort_proto) * 
		  scope(insert: #insert_fun) * JSFunctionObject(#insert_fun, "insert", #insert_sc, #insert_len, #insert_proto))
	@post (JSObject ($lg) * SOList(ret, #E) * nullableObject(ret) * 
		  scope(sort: #sort_fun) * JSFunctionObject(#sort_fun, "sort", #sort_sc, #sort_len, #sort_proto) * 
		  scope(insert: #insert_fun) * JSFunctionObject(#insert_fun, "insert", #insert_sc, #insert_len, #insert_proto))
*/
function sort(head) {
    var result;

    if (head === null) {
        result = null
    } else {
        var rec = sort(head.next);
        result = insert(rec, head.value)
    }
    return result;
}

