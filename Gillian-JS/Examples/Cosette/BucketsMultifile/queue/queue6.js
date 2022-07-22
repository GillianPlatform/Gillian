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

var queue2 = new buckets.Queue();

var ar = [];

createQueue();

queue.forEach(function (x) {
  ar.push(x);
  queue2.enqueue(x);
});

var res1 = queue.equals(queue2);
Assert(res1);

queue2.enqueue(x1);
var res2 = queue.equals(queue2);
Assert(not res2);

var res3 = queue.equals(ar);
Assert(not res3);
