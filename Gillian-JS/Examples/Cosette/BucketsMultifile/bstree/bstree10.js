var buckets = require('../../buckets');

var bst = new buckets.BSTree();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

Assume(not (x1 = x2));
Assume(not (x1 = x3));
Assume(not (x2 = x3));


bst.add(x1);
bst.add(x2);
bst.add(x3);

var ar = bst.toArray();
var l1 = ar.length;
var l2 = bst.size();
Assert(l1 = l2);

var y1 = ar[0];
var y2 = ar[1];
var y3 = ar[2];
Assert((y1 < y2) and (y2 < y3));
