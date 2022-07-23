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

var bst2;
var res1 = bst.equals(bst2);
Assert(not res1);
bst2 = new buckets.BSTree();
var res2 = bst.equals(bst2);
Assert(not res2);

var x4 = symb_number();

bst2.add(x2);
bst2.add(x3);
bst2.add(x4);

var res3 = bst.equals(bst2);
Assert(((x4 = x1) and res3) or ((not (x4 = x1)) and (not res3)));
