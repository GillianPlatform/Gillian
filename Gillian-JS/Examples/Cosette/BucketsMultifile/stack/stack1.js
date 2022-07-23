var buckets = require('../../buckets');

var stack = new buckets.Stack();

var n1 = symb_number();
var n2 = symb_number();
var n3 = symb_number();
Assume(not (n1 = n2));
Assume(not (n1 = n3));
Assume(not (n2 = n3));

var res1 = stack.pop();
Assert(res1 = undefined);
stack.push(n1);
stack.push(n2);
stack.add(n3);
var res6 = stack.peek();
var res2 = stack.pop();
var res3 = stack.pop();
var res4 = stack.pop();
var res5 = stack.pop();
Assert(res2 = n3);
Assert(res6 = n3);
Assert(res3 = n2);
Assert(res4 = n1);
Assert(res5 = undefined);
