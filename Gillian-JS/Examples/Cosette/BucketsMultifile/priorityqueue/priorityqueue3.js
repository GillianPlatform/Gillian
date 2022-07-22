var buckets = require('../../buckets');

var pqueue = new buckets.PriorityQueue();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

pqueue.enqueue(x1);
pqueue.enqueue(x2);
pqueue.enqueue(x3);

var res1 = pqueue.size();
Assert(res1 = 3);
pqueue.clear();
var res2 = pqueue.isEmpty();
Assert(res2);
