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

createQueue();

var ar1 = queue.toArray();
var ar2 = [];

queue.forEach(function (x) {
  ar2.push(x);
});

var l1 = queue.size(), l2 = ar1.length, l3 = ar2.length;
Assert(l1 = l2);
Assert(l2 = l3);

var i;
for (i = 0; i < ar1.length; i++) {
  var ar1i = ar1[i], ar2i = ar2[i];
  Assert(ar1i = ar2i);
}
