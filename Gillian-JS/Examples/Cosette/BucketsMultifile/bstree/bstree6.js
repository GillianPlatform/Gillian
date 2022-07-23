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

// test2: something fails

var x4 = symb_number();
var ar2 = [];
bst.preorderTraversal(function(x) {
  if (x === x4) {
    return false;
  }
  ar2.push(x);
});
var l2 = ar2.length;
Assert(((not (x4 = x1)) and (not (x4 = x2)) and (not (x4 = x3)) and (l2 = 3)) or (((x4 = x1) or (x4 = x2) or (x4 = x3)) and (l2 < 3)));
