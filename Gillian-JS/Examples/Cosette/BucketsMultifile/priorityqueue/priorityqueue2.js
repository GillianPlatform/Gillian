var buckets = require('../../buckets');

var pqueue = new buckets.PriorityQueue();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

pqueue.enqueue(x1);
pqueue.enqueue(x2);
pqueue.enqueue(x3);

pqueue.dequeue();

var res = pqueue.contains(x1);
Assert((((x1 <= x2) or (x1 <= x3)) and res) or ((x1 > x2) and (x1 > x3) and (not res)));
