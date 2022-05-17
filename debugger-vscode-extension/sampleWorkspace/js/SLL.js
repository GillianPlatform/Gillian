"use strict";

/*
  @pred Node(+x:Obj, val, next) :
    JSObject(x) * DataProp(x, "val", val) * DataProp(x, "next", next);

  @pred SLL(+x, alpha:List) :
    (x == null) * (alpha == {{ }}),
    Node(x, #val, #next) * SLL(#next, #beta) * (alpha == #val :: #beta);
*/


/**
  @id listCopy

  @pre  GlobalObject() * scope(listCopy: #listCopy) * JSFunctionObject(#listCopy, "listCopy", _, _, _) *
        (lst == #lst) * SLL(#lst, #alpha)

  @post GlobalObject() * scope(listCopy: #listCopy) * JSFunctionObject(#listCopy, "listCopy", _, _, _) *
        SLL(#lst, #alpha) * SLL(ret, #alpha)
*/
function listCopy (lst) {
  if (lst === null) {
    return null
  }  else {
    return { val: lst.val, next : listCopy(lst.next) }
  }
}

/**
  @id listConcat

  @pre  GlobalObject() * scope(listConcat: #listConcat) * JSFunctionObject(#listConcat, "listConcat", _, _, _) *
        (la == #la) * SLL(#la, #alpha) * (lb == #lb) * SLL(#lb, #beta)

  @post GlobalObject() * scope(listConcat: #listConcat) * JSFunctionObject(#listConcat, "listConcat", _, _, _) *
        SLL(ret, l+ (#alpha, #beta))
*/
function listConcat(la, lb) {
  if (la === null) return lb;
  la.next = listConcat(la.next, lb);
  return la
}

/**
  @id listAppend

  @pre  GlobalObject() * scope(listConcat: #listConcat) * JSFunctionObject(#listConcat, "listConcat", _, _, _) *
        (lst == #lst) * SLL(#lst, #alpha) * (v == #v)

  @post GlobalObject() * scope(listConcat: #listConcat) * JSFunctionObject(#listConcat, "listConcat", _, _, _) *
        SLL(ret, l+ (#alpha, {{ #v }} ))
*/
function listAppend(lst, v) {
  var newNode = { val: v, next : null };
  if (lst === null) {
    return newNode
  } else {
    return listConcat(lst, newNode)
  }
}