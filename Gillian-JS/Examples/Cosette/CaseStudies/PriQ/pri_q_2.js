var PriorityQueue = (function () {

  var Node = function (pri, val) {
    this.pri = pri; 
    this.val = val; 
    this.next = null;
  }

  Node.prototype.insert = function (nl) {
    if (nl === null) {
       return this
    }
    
    if (this.pri > nl.pri) {
       this.next = nl;
       return this
    }
    
    var tmp = this.insert (nl.next);
    nl.next = tmp;
    return nl
  }

  var PQ = function () {
    this._head = null;
  };

  PQ.prototype.enqueue = function(pri, val) {
    var n = new Node(pri, val);
    this._head = n.insert(this._head);
  };

  PQ.prototype.dequeue = function () {
    if (this._head === null) {
      throw new Error("Queue is empty");
    }

    var first = this._head;
    this._head = this._head.next;
    return {pri: first.pri, val: first.val};
  };

  return PQ;
})();

/* Test function */
function isNode(n, pri, val, next) {
  var np = n.pri, nv = n.val, nn = n.next;
  return ((np === pri) && (nv === val) && (nn === next));
}

var pq = new PriorityQueue();

var pri1 = symb_number();
var val1 = symb();

var pri2 = symb_number();
var val2 = symb();

pq.enqueue(pri1, val1);
pq.enqueue(pri2, val2);

var head = pq._head;

var t1 = isNode(head, pri1, val1, head.next) && isNode(head.next, pri2, val2, null);
var t2 = isNode(head, pri2, val2, head.next) && isNode(head.next, pri1, val1, null);

Assert(((pri1 < pri2) and t2) or t1);