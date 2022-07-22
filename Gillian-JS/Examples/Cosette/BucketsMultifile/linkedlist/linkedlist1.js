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
list.add(x3)

var res = list.elementAtIndex(x4);
Assert(((x4 = 0) and (res = x1)) or ((x4 = 1) and (res = x2)) or ((x4 = 2) and (res = x3)) or ( (((not (x4 = 0)) and (not (x4 = 1))) and (not (x4 = 2))) and (res = undefined) ));
