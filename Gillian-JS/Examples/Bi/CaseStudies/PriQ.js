'use strict';

/*
	@id Node
*/
function Node (pri, val) {
	this.pri = pri; 
	this.val = val; 
	this.next = null;
}

/*
	@id nodeInsert
*/
function nodeInsert (n, nl) {

	if (nl === null) {
	   return n
	}
	
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
	this._head = null;
};

/*
	@id pqEnqueue
*/
function pqEnqueue (q, pri, val) {
	var n = { pri: pri, val: val, next: null };
	q._head = nodeInsert(n, q._head);
};

/*
	@id pqDequeue
*/
function pqDequeue (q) {
  if (q._head === null) {
    throw new Error("Queue is empty");
  }

  var first = q._head;
  q._head = q._head.next;
  return {pri: first.pri, val: first.val};
};
