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
var h1 = pq._head; Assert(h1 = null);

try {
  pq.dequeue();
} catch (e) {
  var em = e.message; Assert(em = "Queue is empty");
}

var pri = symb_number();
var val = symb();
pq.enqueue(pri, val);
var t2 = isNode(pq._head, pri, val, null)
Assert(t2);

var ret = pq.dequeue();
var h3 = pq._head; Assert(h3 = null);
var rp = ret.pri, rv = ret.val;
Assert((rp = pri) and (rv = val));
