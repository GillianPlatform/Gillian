'use strict';

/** @id a_insert */
function a_insert(node, value) {

    /* Annotation */ isNullableObject(node);
    /* Annotation */ isNumber(value);
    
    if (node === null) {
        return { next: null, value: value }
    } else {
      /* Annotation */ isNumber(node.value);
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

/* @id b_sort */
function b_sort(head) {

    /* Annotation */ isNullableObject(head);
    
    if (head === null) {
        return null
    } else {
        var rec = b_sort(head.next);
        return a_insert(rec, head.value)
    }
}

