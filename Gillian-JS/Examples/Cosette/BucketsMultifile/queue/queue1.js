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
var size = queue.size();
Assert(size = 0);
createQueue();
var size = queue.size();
Assert(size = 3);
var x4 = symb_number();
queue.add(x4); // synonym to enqueue
var size = queue.size();
Assert(size = 4);
queue.dequeue();
var size = queue.size();
Assert(size = 3);
queue.clear();
var size = queue.size();
Assert(size = 0);
queue.clear();
