var buckets = require('../../buckets');

var dict = new buckets.MultiDictionary()

var s1 = symb_string();
var s2 = symb_string();
var x1 = symb_number();
var x2 = symb_number();
var x3 = symb_number();

Assume (not (x1 = x2));
Assume (not (x1 = x3));
Assume (not (x2 = x3));

dict.set(s1, x1);
dict.set(s1, x2);
dict.set(s2, x2);
dict.set(s2, x3);

var res1 = dict.size();
Assert(((s1 = s2) and (res1 = 1)) or ((not (s1 = s2)) and (res1 = 2)));
var res3 = dict.isEmpty();
Assert(not res3);

dict.clear();
var res2 = dict.size();
Assert(res2 = 0);
var res4 = dict.isEmpty();
Assert(res4);
