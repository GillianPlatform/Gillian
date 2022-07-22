var buckets = require('../../buckets');

var heap = new buckets.Heap();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();
var x4 = symb_number();

Assume(not (x1 = x2));
Assume(not (x1 = x3));
Assume(not (x1 = x4));

Assume(not (x2 = x3));
Assume(not (x2 = x4));

Assume(not (x3 = x4));

heap.add(x1);
heap.add(x2);
heap.add(x3);
//heap.add(x4);

var heap2 = new buckets.Heap();
var res1 = heap.equals(heap2);
Assert(not res1);
heap.forEach(function (x) {
  heap2.add(x);
});
var res2 = heap.equals(heap2);
Assert(res2);

var ar = heap.toArray();
var res3 = heap.equals(ar);
Assert(not res3);
