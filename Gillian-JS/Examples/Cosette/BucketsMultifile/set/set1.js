var buckets = require('../../buckets');

var set = new buckets.Set();

var x1 = symb_number();
var x2 = symb_number();

set.add(x1);
var res = set.add(x2);

var x3 = symb_number();
var res2 = set.contains(x3);
