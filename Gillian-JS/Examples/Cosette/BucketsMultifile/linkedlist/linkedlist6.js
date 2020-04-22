var buckets = require('../../buckets');

var list = new buckets.LinkedList()

var x1 = symb_number(x1);
var x2 = symb_number(x2);
var x3 = symb_number(x3);
var x4 = symb_number(x4);

// Assume(not (x1 = x2));
// Assume(not (x1 = x3));
// Assume(not (x2 = x3));

list.add(x1);
list.add(x2);
list.add(x3);

var list2 = new buckets.LinkedList();
list.forEach(function (x) {
  list2.add(x);
});

list.reverse();
var res = list.equals(list2);
Assert(((x1 = x3) and res) or ((not (x1 = x3)) and (not res)));
