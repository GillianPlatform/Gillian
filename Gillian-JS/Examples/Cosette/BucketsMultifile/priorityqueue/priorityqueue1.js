var buckets = require('../../buckets');

var pqueue = new buckets.PriorityQueue();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

pqueue.enqueue(x1);
pqueue.add(x2);
pqueue.enqueue(x3);

var y1 = pqueue.peek();
pqueue.dequeue();
var y2 = pqueue.dequeue();
var y3 = pqueue.dequeue();
Assert((y1 >= y2) and (y2 >= y3));

var y4 = pqueue.dequeue();
Assert(y4 = undefined);
