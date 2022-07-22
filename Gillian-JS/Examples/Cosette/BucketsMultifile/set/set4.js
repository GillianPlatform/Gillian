var buckets = require('../../buckets');

var set1 = new buckets.Set();
var set2 = new buckets.Set();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

Assume(not (x1 = x3));

set1.add(x1);
set1.add(x2);

set2.add(x2);
set2.add(x3);

set1.difference(set2);

var res1 = set1.size();
Assert(((x1 = x2) and (res1 = 0)) or ((not (x1 = x2)) and (res1 = 1)));

var res2 = set1.contains(x2);
Assert(not res2);
