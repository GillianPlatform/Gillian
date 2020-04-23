var buckets = require('../../buckets');

var set = new buckets.Set();

var x1 = symb_number(x1);
var x2 = symb_number(x2);

var res1 = set.isEmpty();
Assert(res1);

set.add(x1);
set.add(x2);

var res2 = set.isEmpty();
Assert(not res2);

set.clear();
var res3 = set.size();
Assert(res3 = 0);
