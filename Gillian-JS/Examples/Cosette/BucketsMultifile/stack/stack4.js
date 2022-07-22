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

var ar1 = [n3, n2, n1];
var res1 = stack.equals(ar1);
Assert(not res1);

var ar2 = stack.toArray();
var l = ar2.length;
Assert(l = 3);
var i;
for (i = 0; i < 3; i++) {
  var ar1i = ar1[i];
  var ar2i = ar2[i];
  Assert(ar1i = ar2i);
}

var stack2 = new buckets.Stack();
var res3 = stack2.equals(stack);
Assert(not res3);

stack.forEach(function (x) {
  stack2.push(x);
});

var stack3 = new buckets.Stack();

stack2.forEach(function (x) {
  stack3.push(x);
});

var res2 = stack3.equals(stack);
Assert(res2);
