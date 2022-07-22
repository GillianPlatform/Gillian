var buckets = require('../../buckets');

var list = new buckets.LinkedList()

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();
var x4 = symb_number();

Assume(not (x1 = x2));
Assume(not (x1 = x3));
Assume(not (x2 = x3));

list.add(x1)
list.add(x2)
list.add(x3);

var res = list.remove(x4);
Assert((((x4 = x1) or (x4 = x2) or (x4 = x3)) and res) or ((not ((x4 = x1) or (x4 = x2) or (x4 = x3)) and (not res))));
list.remove(x1);
list.remove(x2);
list.remove(x3);
var res2 = list.size();
Assert(res2 = 0);
