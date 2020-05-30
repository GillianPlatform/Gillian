'use strict';

/**
  Bug Specs:
    1. lst is undefined

  Success Specs:
    1. lst is empty
    2. lst has one element

  @id a_listCopy
*/
function a_listCopy (lst) {

  /* Annotation */ isNullableObject(lst);

  if (lst === null)
    return null;

  return { val: lst.val, prev: lst.prev, next : a_listCopy(lst.next) }
}

/**
  Bug Specs:
    1. lb is undefined
    2. la is undefined

  Success Specs:
    1. la has one element, lb not empty
    2. la not empty, lb empty
    3. la empty, lb whatever
    4. la has more than one element, lb not empty (recursive case)

  @id b_listConcat
*/
function b_listConcat(la, lb) {

  /* Annotation */ isNullableObject(la);
  if (la === null) return lb;

  /* Annotation */ isNullableObject(lb);
  if (lb === null) return la;

  /* Annotation */ isNullableObject(la.next);
  if (la.next === null) { la.next = lb; lb.prev = la; return la }

  la.next = b_listConcat(la.next, lb);
  return la
}

/**
  Success Specs:
    1. lst has one element
    2. lst has more than one element
    3. lst is empty

  @id c_listAppend
*/
function c_listAppend(lst, v) {
  var newNode = { val: v, prev : null, next : null };

  /* Annotation */ isNullableObject(lst);
  return (lst === null) ? newNode : b_listConcat(lst, newNode)
}