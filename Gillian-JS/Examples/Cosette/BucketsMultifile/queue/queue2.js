var buckets = require('../../buckets');

var queue = new buckets.Queue();

var x1 = symb_number(); // 1
var x2 = symb_number(); // 2
var x3 = symb_number(); // 3
Assume((x1 < x2) and (x2 < x3));

function createQueue() {
  queue.enqueue(x1);
  queue.enqueue(x2);
  queue.enqueue(x3);
}

var queue = new buckets.Queue();
createQueue();
queue.dequeue();
queue.dequeue();
var res1 = queue.peek();
queue.dequeue();
queue.dequeue();
var res2 = queue.peek();
Assert(res1 = x3);
Assert(res2 = undefined);
