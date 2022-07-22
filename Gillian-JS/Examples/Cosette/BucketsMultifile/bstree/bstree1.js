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

bst.add(undefined);
var size = bst.size();
Assert(size = 3);

var height = bst.height();
Assert(((x1 < x2) and (x2 < x3) and (height = 2)) or ((x1 < x3) and (x3 < x2) and (height = 2)) or ((x2 < x1) and (x1 < x3) and (height = 1)) or ((x2 < x3) and (x3 < x1) and (height = 2)) or ((x3 < x1) and (x1 < x2) and (height = 1)) or ((x3 < x2) and (x2 < x1) and (height = 2)));
