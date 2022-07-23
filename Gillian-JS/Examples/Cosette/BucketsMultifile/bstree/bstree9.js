var buckets = require('../../buckets');

var bst = new buckets.BSTree();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

Assume(not (x1 = x2));
Assume(not (x1 = x3));
Assume(not (x2 = x3));

var r1 = bst.minimum();
Assert(r1 = undefined);
var r2 = bst.maximum();
Assert(r2 = undefined);

bst.add(x1);
bst.add(x2);
bst.add(x3);

var res1 = bst.minimum();
Assert(((x1 < x2) and (x2 < x3) and (res1 = x1)) or ((x1 < x3) and (x3 < x2) and (res1 = x1)) or ((x2 < x1) and (x1 < x3) and (res1 = x2)) or ((x2 < x3) and (x3 < x1) and (res1 = x2)) or ((x3 < x1) and (x1 < x2) and (res1 = x3)) or ((x3 < x2) and (x2 < x1) and (res1 = x3)));

var res2 = bst.maximum();
Assert(((x1 < x2) and (x2 < x3) and (res2 = x3)) or ((x1 < x3) and (x3 < x2) and (res2 = x2)) or ((x2 < x1) and (x1 < x3) and (res2 = x3)) or ((x2 < x3) and (x3 < x1) and (res2 = x1)) or ((x3 < x1) and (x1 < x2) and (res2 = x2)) or ((x3 < x2) and (x2 < x1) and (res2 = x1)));
