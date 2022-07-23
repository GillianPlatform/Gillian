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

var res2 = heap.add(undefined);
Assert(res2 = undefined);
var res3 = heap.peek();
Assert(res3 = undefined);

heap.add(x1);
heap.add(x2);
heap.add(x3);
heap.add(x4);

var res1 = heap.peek();
