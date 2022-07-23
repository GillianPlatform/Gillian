var buckets = require('../../buckets');

var set1 = new buckets.Set();
var set2 = new buckets.Set();

var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

Assume(not (x1 = x2));

set1.add(x1);
set1.add(x2);

set2.add(x2);
set2.add(x3);

set1.intersection(set2);

var res1 = set1.contains(x2);
Assert(res1);

var res2 = set1.contains(x1);
Assert((((x1 = x3)) and res2) or ((not (x1 = x3)) and (not res2)));
