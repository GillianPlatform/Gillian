var buckets = require('../../buckets');

var pqueue = new buckets.PriorityQueue();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

pqueue.enqueue(x1);
pqueue.enqueue(x2);
pqueue.enqueue(x3);

var ar = pqueue.toArray();
var l1 = pqueue.size();
var l2 = ar.length;
Assert(l1 = l2);
var i;
for (i = 0; i < l1; i++) {
  var ari = ar[i]; 
  var resi = pqueue.contains(ari);
  Assert(resi);
}
