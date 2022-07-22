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

var ar = list.toArray();

var l1 = list.size();
var l2 = ar.length;
Assert(l1 = l2);

var i = 0
for (i = 0; i < l1; i++) {
  var li = list.elementAtIndex(i);
  var ari = ar[i];
  Assert(li = ari);
}
