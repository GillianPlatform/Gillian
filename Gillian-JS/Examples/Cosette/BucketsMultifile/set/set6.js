var buckets = require('../../buckets');

var set1 = new buckets.Set();
var set2 = new buckets.Set();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

Assume(not (x1 = x3));

set1.add(x1);
set1.add(x2);

set2.add(x3);
set2.add(x2);

var res1 = set1.equals(set2);
Assert(not res1);

set2.add(x1);
var res2 = set1.equals(set2);
Assert(((x2 = x3) and res2) or ((not (x2 = x3)) and (not res2)));

var ar = [];
set2.forEach(function (x) {
  ar.push(x);
});

var res3 = set2.equals(ar);
Assert(not res3);
