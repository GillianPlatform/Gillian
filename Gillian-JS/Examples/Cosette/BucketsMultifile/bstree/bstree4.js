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

// test 1: full traversal
var ar1 = [];
bst.inorderTraversal(function (x) {
  ar1.push(x);
});

var l = ar1.length;
Assert(l = 3);

var y1 = ar1[0];
var y2 = ar1[1];
var y3 = ar1[2];
Assert((y1 < y2) and (y2 < y3));
