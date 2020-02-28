'use strict';

/** 
  Success Specs:
    1. lst is empty 
    2. lst has one element

  @id a_listCopy  
*/
function a_listCopy (lst) { 
  /* Annotation */ isNullableObject(lst);
  return (lst === null) ? null : { val: lst.val, next : a_listCopy(lst.next) }
}

/** 
  Success Specs:
    1. la empty, lb whatever 
    2. la not empty, lb whatever (recursive case)

  @id b_listConcat
*/
function b_listConcat(la, lb) {

  /* Annotation */ isNullableObject(la);

  if (la === null) return lb; 

  la.next = b_listConcat(la.next, lb);
  return la
}

/** 
  Success Specs:
    1. lst has one element
    2. lst is empty

  @id c_listAppend 
*/
function c_listAppend(lst, v) {
  var newNode = { val: v, next : null };
  return (lst === null) ? newNode : b_listConcat(lst, newNode) 
}