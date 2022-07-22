var buckets = require('../../buckets');

var dict = new buckets.MultiDictionary()

var s1 = symb_string();
var s2 = symb_string();
var x1 = symb_number();
var x2 = symb_number();

Assume(not (x1 = x2));

dict.set(s1, x1);
dict.set(s2, x2);

var res1 = dict.remove(s1, x1);
Assert(res1);
var res2 = dict.remove(s1, x2);
Assert(((s1 = s2) and (not (x1 = x2)) and res2) or (((not (s1 = s2)) or ((s1 = s2) and (x1 = x2))) and (not res2)));

var dict2 = new buckets.MultiDictionary();
dict2.set(s1, x1);
dict2.set(s1, x2);
var res3 = dict2.remove(s1);
Assert(res3);
var res5 = dict2.remove(s1);
Assert(not res5);

dict2.set(s2, x1);
var res4 = dict2.remove(s2, x2);
Assert(not res4);
