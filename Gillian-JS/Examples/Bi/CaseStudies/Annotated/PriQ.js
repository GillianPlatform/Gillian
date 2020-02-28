'use strict';

/*
	@id Node
*/
function Node (pri, val) {
    /* Annotation */ isObject(this);
	this.pri = pri; 
	this.val = val; 
	this.next = null;
}

/*
	@id nodeInsert
*/
function nodeInsert (n, nl) {

    /* Annotation */ isObject(n);
    /* Annotation */ isNullableObject(nl);

	if (nl === null) {
	   return n
	}
    
    /* Annnotation */ isNumber(n.pri);
    /* Annnotation */ isNumber(nl.pri);
    
	if (n.pri > nl.pri) {
	   n.next = nl;
	   return n
	}
	
	var tmp = nodeInsert(n, nl.next);
	nl.next = tmp;
	return nl
}

/*
	@id PQ
*/
function PQ () {
    /* Annotation */ isObject(this);
	this._head = null;
};

/*
	@id pqEnqueue
*/
function pqEnqueue (q, pri, val) {
    /* Annotation */ isObject(q);
	var n = { pri: pri, val: val, next: null };
	q._head = nodeInsert(n, q._head);
};

/*
	@id pqDequeue
*/
function pqDequeue (q) {
  /* Annotation */ isObject(q);
  /* Annotation */ isNullableObject(q._head);
  if (q._head === null) {
    throw new Error("Queue is empty");
  }

  var first = q._head;
  q._head = q._head.next;
  return {pri: first.pri, val: first.val};
};
