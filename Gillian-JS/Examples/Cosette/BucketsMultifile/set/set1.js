var buckets = require('../../buckets');

var set = new buckets.Set();

var x1 = symb_number(x1);
var x2 = symb_number(x2);

set.add(x1);
var res = set.add(x2);

var x3 = symb_number(x3);
var res2 = set.contains(x3);
