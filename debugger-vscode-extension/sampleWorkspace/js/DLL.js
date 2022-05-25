"use strict";

/*
  @pred Node(+x:Obj, val, prev, next) :
    JSObject(x) * DataProp(x, "val", val) * DataProp(x, "prev", prev) * DataProp(x, "next", next);

  @pred DLL(+x, alpha:List) :
    (x == null) * (alpha == {{ }}),
    Node(x, #val, #prev, #next) * DLL(#next, #beta) * (alpha == #val :: #beta);
*/

/**
  @id listCopy

  @pre  GlobalObject() * scope(listCopy: #listCopy) * JSFunctionObject(#listCopy, "listCopy", _, _, _) *
        (lst == #lst) * DLL(#lst, #alpha)

  @post GlobalObject() * scope(listCopy: #listCopy) * JSFunctionObject(#listCopy, "listCopy", _, _, _) *
        DLL(#lst, #alpha) * DLL(ret, #alpha)
*/
function listCopy (lst) {
  if (lst === null) {
    return null
  } else {
    return { val: lst.val, prev: lst.prev, next : listCopy(lst.next) }
  }
}

/**
  @id listConcat

  @pre  GlobalObject() * scope(listConcat: #listConcat) * JSFunctionObject(#listConcat, "listConcat", _, _, _) *
        (la == #la) * DLL(#la, #alpha) * (lb == #lb) * DLL(#lb, #beta)

  @post GlobalObject() * scope(listConcat: #listConcat) * JSFunctionObject(#listConcat, "listConcat", _, _, _) *
        DLL(ret, l+ (#alpha, #beta))
*/
function listConcat(la, lb) {
  if (la === null) return lb;
  if (lb === null) return la;

  if (la.next === null) { la.next = lb; lb.prev = la; return la }

  la.next = listConcat(la.next, lb);
  return la
}

/**
  @id listAppend

  @pre  GlobalObject() * scope(listConcat: #listConcat) * JSFunctionObject(#listConcat, "listConcat", _, _, _) *
        (lst == #lst) * DLL(#lst, #alpha) * (v == #v)

  @post GlobalObject() * scope(listConcat: #listConcat) * JSFunctionObject(#listConcat, "listConcat", _, _, _) *
        DLL(ret, l+ (#alpha, {{ #v }} ))
*/
function listAppend(lst, v) {
  var newNode = { val: v, prev : null, next : null };
  if (lst === null) {
    return newNode
  } else {
    return listConcat(lst, newNode)
  }
}