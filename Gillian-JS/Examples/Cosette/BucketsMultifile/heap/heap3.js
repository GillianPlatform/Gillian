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

heap.removeRoot();
var res1 = heap.contains(x1);
Assert(((x1 < x2) and (x1 < x3) and (not res1)) or ((not ((x1 < x2) and (x1 < x3))) and res1));

var res2 = heap.size();
Assert(res2 = 2);

heap.clear();
var res3 = heap.isEmpty();
Assert(res3);
var res4 = heap.removeRoot();
Assert(res4 = undefined);
