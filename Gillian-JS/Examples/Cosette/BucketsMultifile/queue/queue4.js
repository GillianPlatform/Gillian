var buckets = require('../../buckets');

var queue = new buckets.Queue();

var x1 = symb_number(x1); // 1
var x2 = symb_number(x2); // 2
var x3 = symb_number(x3); // 3
Assume((x1 < x2) and (x2 < x3));

function createQueue() {
  queue.enqueue(x1);
  queue.enqueue(x2);
}

var res1 = queue.isEmpty();
createQueue();
var res2 = queue.isEmpty();
Assert(res1);
Assert(not res2);
