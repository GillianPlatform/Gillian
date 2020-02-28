'use strict';

/*
  Bug Specs:
    1. node is undefined

  @id a_insert 
*/
function a_insert(node, value) {
    
    if (node === null) {
        return { next: null, value: value }
    } else {
      if (node.value === value) {
        return node;
      } else if (node.value < value) {
        var rec = a_insert(node.next, value);
        return { next: rec, value: node.value }
      } else {
        return { next: node, value: value }
      }
    }
}

/** 
  Bug Specs:
    1. head is undefined

  @id b_sort 
*/
function b_sort(head) {    
    if (head === null) {
        return null
    } else {
        var rec = b_sort(head.next);
        return a_insert(rec, head.value)
    }
}

