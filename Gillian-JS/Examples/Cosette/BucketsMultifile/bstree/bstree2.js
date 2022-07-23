var buckets = require('../../buckets');

var bst = new buckets.BSTree();

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

bst.add(x1);
bst.add(x2);
bst.add(x3);
bst.add(x4);

bst.remove(x2);
var res1 = bst.contains(x2);
Assert(not res1);
var res2 = bst.contains(x3);
Assert(res2);
var res0 = bst.contains(undefined);
Assert(not res0);

bst.remove(x1);
bst.remove(x3);
bst.remove(x4);
var res3 = bst.contains(x1);
Assert(not res3);
var res4 = bst.isEmpty();
Assert(res4);
