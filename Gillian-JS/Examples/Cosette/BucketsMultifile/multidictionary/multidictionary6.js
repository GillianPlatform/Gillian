var buckets = require('../../buckets');

var dict1 = new buckets.MultiDictionary();
var dict2 = new buckets.MultiDictionary();

var s1 = symb_string();
var s2 = symb_string();
var x1 = symb_number();
var x2 = symb_number();

Assume (not (x1 = x2));

dict1.set(s1, x1);
dict1.set(s1, x2);

var res1 = dict1.equals(dict2);
Assert(not res1);
var res2 = dict1.equals(dict1.keys());
Assert(not res2);

dict2.set(s2, x1);
dict2.set(s2, x2);

var res3 = dict1.equals(dict2);
Assert(((s1 = s2) and res3) or ((not (s1 = s2)) and (not res3)));
