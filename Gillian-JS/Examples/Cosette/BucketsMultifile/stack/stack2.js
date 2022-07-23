var buckets = require('../../buckets');

var stack = new buckets.Stack();

var n1 = symb_number();
var n2 = symb_number();
var n3 = symb_number();
Assume(not (n1 = n2));
Assume(not (n1 = n3));
Assume(not (n2 = n3));

var res1 = stack.size();
Assert(res1 = 0);
stack.push(n1);
stack.push(n2);
stack.push(n3);
var res2 = stack.size();
Assert(res2 = 3);
var res3 = stack.isEmpty();
Assert(not res3);
stack.clear();
var res4 = stack.isEmpty();
Assert(res4);
