var buckets = require('../../buckets');

var set1 = new buckets.Set();
var set2 = new buckets.Set();

var x1 = symb_string(x1);
var x2 = symb_string(x2);
var x3 = symb_string(x3);
var x4 = symb_string(x4);

set1.add(x1);
set1.add(x2);

set2.add(x3);
set2.add(x4);

var res1 = set2.isSubsetOf(set1);

set1.union(set2);

var res2 = set1.contains(x3); 
var res3 = set2.isSubsetOf(set1);

Assert((((((x3 = x1) or (x3 = x2)) and ((x4 = x1) or (x4 = x2))) and res1) or ((((not (x3 = x1)) and (not (x3 = x2))) or ((not (x4 = x1)) and (not (x4 = x2)))) and (not res1))) and (res2 and res3));
