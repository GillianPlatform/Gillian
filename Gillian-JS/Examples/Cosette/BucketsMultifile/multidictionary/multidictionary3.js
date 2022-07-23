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

var keys = dict.keys();
var l = keys.length;
Assert(((s1 = s2) and (l = 1)) or ((not (s1 = s2)) and (l = 2)));

var values = dict.values();
var l2 = values.length;
Assert(((s1 = s2) and (l2 = 3)) or ((not (s1 = s2)) and (l2 = 4)));
