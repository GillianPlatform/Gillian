var buckets = require('../../../buckets_with_bugs');

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

var res = list.removeElementAtIndex(x4);
Assert(((x4 = 0) and (res = x1)) or ((x4 = 1) and (res = x2)) or ((x4 = 2) and (res = x3)) or (res = undefined));
list.removeElementAtIndex(0);
list.removeElementAtIndex(0);
var res2 = list.isEmpty();

Assert( ((((x4 = 0) or (x4 = 1)) or (x4 = 2)) and res2) or ((((not (x4 = 0)) and (not (x4 = 1))) and (not (x4 = 2))) and (not res2)) );
