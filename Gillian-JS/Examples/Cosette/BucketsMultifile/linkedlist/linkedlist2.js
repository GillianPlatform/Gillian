var buckets = require('../../buckets');

var list = new buckets.LinkedList()

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();
var x4 = symb_number();

Assume(not (x1 = x2));
Assume(not (x1 = x3));
Assume(not (x2 = x3));

var res0 = list.first();
var res1 = list.last();
Assert(res0 = undefined);
Assert(res1 = undefined);

list.add(x1)
list.add(x2)
list.add(x3, x4)

var res3 = list.first();
Assert(((x4 = 0) and (res3 = x3)) or (res3 = x1));
var res4 = list.last();
Assert(((x4 = 2) and (res4 = x3)) or (res4 = x2));
