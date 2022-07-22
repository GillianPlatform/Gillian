var buckets = require('../../buckets');

var stack = new buckets.Stack();

var n1 = symb_number();
var n2 = symb_number();
var n3 = symb_number();
Assume(not (n1 = n2));
Assume(not (n1 = n3));
Assume(not (n2 = n3));

stack.push(n1);
stack.push(n2);
stack.push(n3);
stack.pop();
var res1 = stack.contains(n3);
Assert(not res1);
stack.pop();
var res2 = stack.contains(n1);
Assert(res2);
