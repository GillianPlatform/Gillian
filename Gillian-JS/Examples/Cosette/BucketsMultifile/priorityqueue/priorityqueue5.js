var buckets = require('../../buckets');

var pqueue = new buckets.PriorityQueue();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

pqueue.enqueue(x1);
pqueue.enqueue(x2);
pqueue.enqueue(x3);

pqueue.dequeue();

var pqueue2 = new buckets.PriorityQueue();

var res1 = pqueue.equals(pqueue2);
Assert(not res1);

pqueue.forEach(function (x) {
  pqueue2.add(x);
});
var res2 = pqueue2.equals(pqueue);
Assert(res2);

var ar = pqueue.toArray();
var res3 = pqueue.equals(ar);
Assert(not res3);
