var buckets = require('../../buckets');

var dict = new buckets.MultiDictionary()

var s1 = symb_string(s1);
var s2 = symb_string(s2);
var x1 = symb_number(x1);
var x2 = symb_number(x2);

dict.set(s1, x1);
dict.set(s2, x2);

var res1 = dict.set(s1, undefined);
Assert(not res1);

var s3 = symb_string(s3);
Assume(not (s1 = s3));
Assume(not (s2 = s3));
var res2 = dict.get(s3).length;
Assert(res2 = 0);

var res = dict.get(s1).length;

Assert(((s1 = s2) and (not (x1 = x2)) and (res = 2)) or ((s1 = s2) and (x1 = x2) and (res = 1)) or ((not (s1 = s2)) and (res = 1)));
