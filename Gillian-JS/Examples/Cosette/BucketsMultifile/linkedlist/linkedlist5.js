var buckets = require('../../buckets');

var list1 = new buckets.LinkedList();
var list2 = new buckets.LinkedList();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();
var x4 = symb_number();

list1.add(x1);
list1.add(x2);

list2.add(x3);
var res1 = list1.equals(list2);
Assert(not res1);

list2.add(x4);
var res2 = list1.equals(list2);
Assert(((x1 = x3) and (x2 = x4) and res2) or ((not ((x1 = x3) and (x2 = x4))) and (not res2)));

var res3 = list2.equals([x3, x4]);
Assert(not res3);
